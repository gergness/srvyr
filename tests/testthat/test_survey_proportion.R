context("Quick tests for survey proportions")

library(srvyr)
library(survey)
source("utilities.R")

data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)


out_srvyr <- dstrata %>%
  summarize(x = survey_mean(awards == "Yes", vartype = "ci",
                            prop_method = "likelihood", proportion = TRUE))

out_survey <- svyciprop(~awards == "Yes", dstrata, method = "likelihood")
out_survey <- dplyr::bind_cols(data.frame(coef(out_survey)[1]),
                               data.frame(t(confint(out_survey)[1:2])))
names(out_survey) <- c("x", "x_low", "x_upp")

test_that("ungrouped proportion works correctly",
          expect_df_equal(out_srvyr, out_survey))


out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarize(x = survey_mean(awards == "Yes", vartype = "ci",
                            prop_method = "likelihood", proportion = TRUE))

out_survey <- svyby(~awards == "Yes", ~stype, dstrata, svyciprop,
                    vartype = "ci", method = "likelihood")
out_survey <- dplyr::tbl_df(data.frame(out_survey))
names(out_survey) <- c("stype", "x", "x_low", "x_upp")

test_that("grouped proportion works correctly",
          expect_df_equal(out_srvyr, out_survey))


test_that("Can set level for grouped proportion (#44)", {
  data(api)
  api_des = apistrat %>% as_survey_design(ids=dnum, strata=stype, weights=pw, nest=T)

  # 90% CI for grouped data
  cl_90 = api_des %>%
    group_by(stype) %>%
    summarize(prop=survey_mean(comp.imp == "Yes", na.rm=T, proportion=T, vartype="ci", level=0.9))

  # 99% CI for grouped data
  cl_99 = api_des %>%
    group_by(stype) %>%
    summarize(prop=survey_mean(comp.imp == "Yes", na.rm=T, proportion=T, vartype="ci", level=0.99))

  expect_true(!identical(cl_90, cl_99))


})
