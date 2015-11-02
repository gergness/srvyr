context("Quick tests for added summary statistics (survey_ratio / survey_quantile)")

library(srvyr)
library(survey)

data(api)
dstrata <- apistrat %>%
  design_survey(strata = stype, weights = pw)

out_survey <- svyratio(~api00, ~api99, dstrata)

out_srvyr <- dstrata %>%
  summarise(api_ratio = survey_ratio(api00, api99))

test_that("survey_ratio works for ungrouped surveys",
          expect_equal(c(out_survey[[1]], sqrt(out_survey$var)),
                       c(out_srvyr[[1, 1]], out_srvyr[[1, 2]])))


out_survey <- svyby(~api00, ~stype, denominator = ~api99, dstrata, svyratio) %>%
  as.data.frame()

out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(api_ratio = survey_ratio(api00, api99))

test_that("survey_ratio works for ungrouped surveys",
          expect_true(all(out_survey == out_srvyr)))


# survey_quantile
out_survey <- svyquantile(~api00, dstrata, c(0.5, 0.75))


out_srvyr <- dstrata %>%
  summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75)))

test_that("survey_quantile works for ungrouped surveys - no ci",
          expect_equal(c(out_survey[[1]], out_survey[[2]]),
                       c(out_srvyr[[1, 1]], out_srvyr[[1, 2]])))



out_survey <- svyquantile(~api00, dstrata, c(0.5, 0.75), ci = TRUE)

out_srvyr <- dstrata %>%
  summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75), vartype = "ci"))

test_that("survey_quantile works for ungrouped surveys - with ci",
          expect_equal(c(out_survey$CIs[[1]], out_survey$CIs[[2]]),
                       c(out_srvyr[[1, "api00_q50_low"]], out_srvyr[[1, "api00_q50_upp"]])))


suppressWarnings(out_survey <- svyby(~api00, ~stype, dstrata, svyquantile, quantiles = c(0.5, 0.75), ci = TRUE))

suppressWarnings(out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75), vartype = "se")))

test_that("survey_quantile works for grouped surveys - with se",
          expect_equal(c(out_survey$`0.5`[[1]], out_survey$`se.0.5`[[1]]),
                       c(out_srvyr[[1, "api00_q50"]], out_srvyr[[1, "api00_q50_se"]])))

# survey_quantile
out_survey <- svyquantile(~api00, dstrata, c(0.5))


out_srvyr <- dstrata %>%
  summarise(api00 = survey_median(api00))

test_that("survey_quantile works for ungrouped surveys - no ci",
          expect_equal(c(out_survey[[1]]),
                       c(out_srvyr[[1, 1]])))
