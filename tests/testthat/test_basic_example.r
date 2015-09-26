context("A basic workflow gets correct results.")

suppressPackageStartupMessages(library(survey))
data(api)

# Overall
## srvyr
dstrata_srvyr <- apistrat %>%
  design_survey(strata = stype, weights = pw)

srvyr_results <- dstrata_srvyr %>%
  summarise(api99 = survey_mean(api99, vartype = c("se", "var", "ci")))


## survey
dstrata_survey <- svydesign(ids = ~1, strata = ~stype, weights = ~pw, data = apistrat)

survey_mn <- svymean(~api99, dstrata_srvyr)

test_that("srvyr and survey get same mean (overall)",
          expect_equal(survey_mn[[1]], srvyr_results[[1, 1]]))

test_that("srvyr and survey get same var (overall)",
          expect_equal(attr(survey_mn, "var")[[1]], srvyr_results[[1, 3]]))

test_that("srvyr and survey get same CIs (overall)",
          expect_equal(confint(survey_mn)[1:2], c(srvyr_results[[1, 4]], srvyr_results[[1, 5]])))


# Grouped data
srvyr_grouped_results <- dstrata_srvyr %>%
  group_by(stype) %>%
  summarise(api99 = survey_mean(api99, vartype = c("se", "var", "ci")))

survey_grouped_results <- svyby(~api99, ~stype, dstrata_survey, svymean,
                                vartype = c("se", "var", "ci"))


test_that("srvyr and survey get same mean (grouped)",
          expect_equal(survey_grouped_results$api99, srvyr_grouped_results$api99))

test_that("srvyr and survey get same var (grouped)",
          expect_equal(survey_grouped_results$var, srvyr_grouped_results$api99_var))

test_that("srvyr and survey get same lower CIs (grouped)",
          expect_equal(survey_grouped_results$ci_l, srvyr_grouped_results$api99_low))

test_that("srvyr and survey get same upper CIs (grouped)",
          expect_equal(survey_grouped_results$ci_u, srvyr_grouped_results$api99_upp))
