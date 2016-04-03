context("A basic workflow gets correct results.")

suppressPackageStartupMessages(library(survey))
data(api)

# Overall
## srvyr
dstrata_srvyr <- apistrat %>%
  as_survey(strata = stype, weights = pw)

srvyr_results <- dstrata_srvyr %>%
  summarise(api99_mn = survey_mean(api99, vartype = c("se", "var", "ci")),
            api99_tot = survey_total(api99, vartype = c("se", "var", "ci")))


## survey
dstrata_survey <- svydesign(ids = ~1, strata = ~stype, weights = ~pw,
                            data = apistrat)

survey_mn <- svymean(~api99, dstrata_survey)
survey_tot <- svytotal(~api99, dstrata_survey)

test_that("srvyr and survey get same mean (overall)",
          expect_equal(survey_mn[[1]], srvyr_results[[1]][[1]]))

test_that("srvyr and survey get same mean var (overall)",
          expect_equal(attr(survey_mn, "var")[[1]], srvyr_results[[3]][[1]]))

test_that("srvyr and survey get same mean CIs (overall)",
          expect_equal(confint(survey_mn)[1:2], c(srvyr_results[[4]][[1]],
                                                  srvyr_results[[5]][[1]])))

test_that("srvyr and survey get same total (overall)",
          expect_equal(survey_tot[[1]], srvyr_results[[6]][[1]]))

test_that("srvyr and survey get same total var (overall)",
          expect_equal(attr(survey_tot, "var")[[1]], srvyr_results[[8]][[1]]))

test_that("srvyr and survey get same total CIs (overall)",
          expect_equal(confint(survey_tot)[1:2], c(srvyr_results[[9]][[1]],
                                                   srvyr_results[[10]][[1]])))


# Grouped data
srvyr_grouped_results <- dstrata_srvyr %>%
  group_by(stype) %>%
  summarise(api99_mn = survey_mean(api99, vartype = c("se", "var", "ci")),
            api99_tot = survey_total(api99, vartype = c("se", "var", "ci")))

survey_grouped_results_mn <- svyby(~api99, ~stype, dstrata_survey, svymean,
                                vartype = c("se", "var", "ci"))

survey_grouped_results_tot <- svyby(~api99, ~stype, dstrata_survey, svytotal,
                                   vartype = c("se", "var", "ci"))

test_that("srvyr and survey get same mean (grouped)",
          expect_equal(survey_grouped_results_mn$api99,
                       srvyr_grouped_results$api99_mn))

test_that("srvyr and survey get same mean var (grouped)",
          expect_equal(survey_grouped_results_mn$var,
                       srvyr_grouped_results$api99_mn_var))

test_that("srvyr and survey get same mean lower CIs (grouped)",
          expect_equal(survey_grouped_results_mn$ci_l,
                       srvyr_grouped_results$api99_mn_low))

test_that("srvyr and survey get same mean upper CIs (grouped)",
          expect_equal(survey_grouped_results_mn$ci_u,
                       srvyr_grouped_results$api99_mn_upp))

test_that("srvyr and survey get same total (grouped)",
          expect_equal(survey_grouped_results_tot$api99,
                       srvyr_grouped_results$api99_tot))

test_that("srvyr and survey get same total var (grouped)",
          expect_equal(survey_grouped_results_tot$var,
                       srvyr_grouped_results$api99_tot_var))

test_that("srvyr and survey get same total lower CIs (grouped)",
          expect_equal(survey_grouped_results_tot$ci_l,
                       srvyr_grouped_results$api99_tot_low))

test_that("srvyr and survey get same total upper CIs (grouped)",
          expect_equal(survey_grouped_results_tot$ci_u,
                       srvyr_grouped_results$api99_tot_upp))
