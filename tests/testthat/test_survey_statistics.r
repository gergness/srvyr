context("Quick tests for added summary statistics (survey_ratio)")

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


