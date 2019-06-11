context("Quick tests for basic summary stats (mean / total)")

library(srvyr)
library(survey)
source("utilities.R")

data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

df_test <- 30

################################################################################
# survey_mean
out_survey <- svymean(~api99, dstrata, deff = TRUE)

out_srvyr <- dstrata %>%
  summarise(survey_mean = survey_mean(api99))

test_that("survey_mean works for ungrouped surveys - no ci (with se)",
          expect_equal(unname(unlist(as.data.frame(out_survey)[, 1:2])),
                       unname(unlist(out_srvyr))))

out_srvyr_vartypeNULL <- dstrata %>%
  summarise(api_mean = survey_mean(api99, vartype = NULL))

test_that("survey_mean works for ungrouped surveys - with vartype = NULL",
          expect_equal(as.data.frame(out_survey)[, 1],
                       unname(unlist(out_srvyr_vartypeNULL))))

out_srvyr <- dstrata %>%
  summarise(survey_mean = survey_mean(api99, deff = TRUE, vartype = "ci", df = df_test))

test_that("deff works for ungrouped survey mean",
          expect_equal(c(out_survey[[1]],  deff(out_survey)[[1]]),
                       c(out_srvyr[["survey_mean"]][[1]], out_srvyr[["survey_mean_deff"]][[1]])))

test_that("df works for ungrouped survey mean",
          expect_equal(confint(out_survey, df = df_test)[c(1, 2)],
                       c(out_srvyr[["survey_mean_low"]][[1]], out_srvyr[["survey_mean_upp"]][[1]])))


out_survey <- svytotal(~api99, dstrata, deff = TRUE)

out_srvyr <- dstrata %>%
  summarise(survey_tot = survey_total(api99, deff = TRUE, vartype = "ci", df = df_test))

test_that("deff works for ungrouped survey total",
          expect_equal(c(out_survey[[1]], deff(out_survey)[[1]]),
                       c(out_srvyr[["survey_tot"]][[1]], out_srvyr[["survey_tot_deff"]][[1]])))

test_that("df works for ungrouped survey total",
          expect_equal(confint(out_survey, df = df_test)[c(1, 2)],
                       c(out_srvyr[["survey_tot_low"]][[1]], out_srvyr[["survey_tot_upp"]][[1]])))


temp_survey <- svyby(~api99, ~stype, dstrata, svymean, deff = TRUE, vartype = c("se", "ci"))
out_survey <- temp_survey %>%
  data.frame() %>%
  tibble::as_tibble() %>%
  rename(survey_mean = api99, survey_mean_se = se, survey_mean_low = ci_l,
         survey_mean_upp = ci_u, survey_mean_deff = `DEff.api99`)

out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_mean = survey_mean(api99))

test_that("survey_mean works for grouped surveys - no ci (with se)",
          expect_df_equal(select(out_survey, stype, survey_mean, survey_mean_se),
                          out_srvyr))

out_srvyr_vartypeNULL <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_mean = survey_mean(api99, vartype = NULL))

test_that("survey_mean works for grouped surveys - with vartype=NULL",
          expect_df_equal(select(out_survey, stype, survey_mean),
                          out_srvyr_vartypeNULL))

out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_mean = survey_mean(api99, deff = TRUE, vartype = "ci", df = df_test))

out_survey[, c("survey_mean_low", "survey_mean_upp")] <-
  confint(temp_survey, df = df_test)

test_that("deff and df work for grouped survey mean",
          expect_df_equal(out_srvyr,
                          select(out_survey, -survey_mean_se)))

################################################################################
# survey_total
out_survey <- svytotal(~api99, dstrata, deff = TRUE) %>%
  {cbind(as.data.frame(.),
         confint(., df = degf(dstrata)))} %>%
  as_tibble() %>%
  setNames(c("survey_total", "survey_total_se", "survey_total_deff",
             "survey_total_low", "survey_total_upp"))

out_srvyr <- dstrata %>%
  summarise(survey_total = survey_total(api99))

test_that("survey_total works for ungrouped surveys - no ci (with se)",
          expect_df_equal(select(out_survey, survey_total, survey_total_se),
                          out_srvyr))

out_srvyr <- dstrata %>%
  summarise(survey_total = survey_total(api99, vartype = "ci"))

test_that("survey_total works for ungrouped surveys - ci",
          expect_df_equal(select(out_survey, survey_total,
                                 survey_total_low, survey_total_upp),
                          out_srvyr))

out_srvyr_vartypeNULL <- dstrata %>%
  summarise(survey_total = survey_total(api99, vartype = NULL))

test_that("survey_total works for ungrouped surveys - with vartype = NULL",
          expect_df_equal(select(out_survey, survey_total),
                          out_srvyr_vartypeNULL))

temp_survey <- svyby(~api99, ~stype, dstrata, svytotal, deff = TRUE, vartype = c("se", "ci"))
out_survey <- temp_survey %>%
  data.frame() %>%
  tibble::as_tibble() %>%
  rename(survey_total = api99, survey_total_se = se,
         survey_total_low = ci_l, survey_total_upp = ci_u,
         survey_total_deff = `DEff.api99`)

out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_total = survey_total(api99))

test_that("survey_total works for grouped surveys - no ci (with se)",
          expect_df_equal(select(out_survey, stype, survey_total, survey_total_se),
                          out_srvyr))

out_srvyr_vartypeNULL <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_total = survey_total(api99, vartype = NULL))

test_that("survey_total works for grouped surveys - with vartype=NULL",
          expect_df_equal(select(out_survey, stype, survey_total),
                          out_srvyr_vartypeNULL))

out_survey[, c("survey_total_low", "survey_total_upp")] <-
  confint(temp_survey, df = df_test)

out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_total = survey_total(api99, deff = TRUE, vartype = "ci", df = df_test))

test_that("deff and df work for grouped survey total",
          expect_df_equal(out_srvyr,
                          select(out_survey, -survey_total_se)))
