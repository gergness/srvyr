context("Quick tests for basic summary stats (mean / total)")

library(srvyr)
library(survey)

data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

df_test <- 30

out_survey <- svymean(~api99, dstrata, deff = TRUE)

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


out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_mean = survey_mean(api99, deff = TRUE, vartype = "ci", df = df_test))

temp_survey <- svyby(~api99, ~stype, dstrata, svymean, deff = TRUE, vartype = c("se", "ci"))
out_survey <- temp_survey %>%
  data.frame() %>%
  dplyr::tbl_df() %>%
  rename(survey_mean = api99, survey_mean_low = ci_l, survey_mean_upp = ci_u,
         survey_mean_deff = `DEff.api99`) %>%
  select(-se)

out_survey[, c("survey_mean_low", "survey_mean_upp")] <-
  confint(temp_survey, df = df_test)

test_that("deff and df work for grouped survey mean",
          expect_equal(out_srvyr, out_survey))


out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_tot = survey_total(api99, deff = TRUE, vartype = "ci", df = df_test))

temp_survey <- svyby(~api99, ~stype, dstrata, svytotal, deff = TRUE, vartype = c("se", "ci"))
out_survey <- temp_survey %>%
  data.frame() %>%
  dplyr::tbl_df() %>%
  rename(survey_tot = api99, survey_tot_low = ci_l, survey_tot_upp = ci_u,
         survey_tot_deff = `DEff.api99`) %>%
  select(-se)

out_survey[, c("survey_tot_low", "survey_tot_upp")] <-
  confint(temp_survey, df = df_test)

test_that("deff and df work for grouped survey total",
          expect_equal(out_srvyr, out_survey))
