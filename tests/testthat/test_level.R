context("Test that level accepts vector correctly")

suppressPackageStartupMessages(library(survey))
data(api)

# Overall
## srvyr
dstrata_srvyr <- apistrat %>%
  as_survey(strata = stype, weights = pw)

srvyr_results <- dstrata_srvyr %>%
  summarise(api99_mn = survey_mean(api99, vartype = "ci", level=c(.75,.9,.95)),
            api99_tot = survey_total(api99, vartype = "ci", level=c(.75,.9,.95)))


## survey
dstrata_survey <- svydesign(ids = ~1, strata = ~stype, weights = ~pw,
                            data = apistrat)

survey_mn <- svymean(~api99, dstrata_survey)
survey_tot <- svytotal(~api99, dstrata_survey)

test_that("Test that CIs are correct for .75",
          expect_equal(confint(survey_mn,level=.75)[1:2], c(srvyr_results[[1,2]],srvyr_results[[1,3]])))
test_that("Test that CIs are correct for .9",
          expect_equal(confint(survey_mn,level=.9)[1:2], c(srvyr_results[[1,4]],srvyr_results[[1,5]])))
test_that("Test that CIs are correct for .95",
          expect_equal(confint(survey_mn,level=.95)[1:2], c(srvyr_results[[1,6]],srvyr_results[[1,7]])))
test_that("Test that CIs are correct for .75",
          expect_equal(confint(survey_tot,level=.75)[1:2], c(srvyr_results[[1,9]],srvyr_results[[1,10]])))
test_that("Test that CIs are correct for .9",
          expect_equal(confint(survey_tot,level=.9)[1:2], c(srvyr_results[[1,11]],srvyr_results[[1,12]])))
test_that("Test that CIs are correct for .95",
          expect_equal(confint(survey_tot,level=.95)[1:2], c(srvyr_results[[1,13]],srvyr_results[[1,14]])))


# Grouped data
srvyr_grouped_results <- dstrata_srvyr %>%
  group_by(stype) %>%
  summarise(api99_mn = survey_mean(api99, vartype = "ci",level=c(.75,.9,.95)),
            api99_tot = survey_total(api99, vartype ="ci",level=c(.75,.9,.95)))

survey_grouped_results_mn <- svyby(~api99, ~stype, dstrata_survey, svymean)

survey_grouped_results_tot <- svyby(~api99, ~stype, dstrata_survey, svytotal)

test_that("Test that CIs are correct for .75",
          expect_equal(c(unname(confint(survey_grouped_results_mn,level=.75)[,1]),unname(confint(survey_grouped_results_mn,level=.75)[,2])),
                       c(srvyr_grouped_results[[3]],srvyr_grouped_results[[4]])))
test_that("Test that CIs are correct for .90",
          expect_equal(c(unname(confint(survey_grouped_results_mn,level=.9)[,1]),unname(confint(survey_grouped_results_mn,level=.9)[,2])),
                       c(srvyr_grouped_results[[5]],srvyr_grouped_results[[6]])))
test_that("Test that CIs are correct for .95",
          expect_equal(c(unname(confint(survey_grouped_results_mn,level=.95)[,1]),unname(confint(survey_grouped_results_mn,level=.95)[,2])),
                       c(srvyr_grouped_results[[7]],srvyr_grouped_results[[8]])))
test_that("Test that CIs are correct for .75",
          expect_equal(c(unname(confint(survey_grouped_results_tot,level=.75)[,1]),unname(confint(survey_grouped_results_tot,level=.75)[,2])),
                       c(srvyr_grouped_results[[10]],srvyr_grouped_results[[11]])))
test_that("Test that CIs are correct for .90",
          expect_equal(c(unname(confint(survey_grouped_results_tot,level=.9)[,1]),unname(confint(survey_grouped_results_tot,level=.9)[,2])),
                       c(srvyr_grouped_results[[12]],srvyr_grouped_results[[13]])))
test_that("Test that CIs are correct for .95",
          expect_equal(c(unname(confint(survey_grouped_results_tot,level=.95)[,1]),unname(confint(survey_grouped_results_tot,level=.95)[,2])),
                       c(srvyr_grouped_results[[14]],srvyr_grouped_results[[15]])))
