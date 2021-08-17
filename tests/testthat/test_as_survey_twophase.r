context("as_survey_twophase works as expected.")
library(srvyr)
library(survey)
library(dplyr)
source("utilities.R")


# From survey::twophase examples
data(pbc, package="survival")

pbc <- pbc %>%
  mutate(randomized = !is.na(trt) & trt > 0,
         id = row_number())

d2pbc_survey <- twophase(id=list(~id,~id), data=pbc, subset=~randomized)

d2pbc_srvyr <- pbc %>%
  as_survey_twophase(id = list(id, id), subset = randomized)

survey_results <- list(
  as.data.frame(svymean(~bili, d2pbc_survey)),
  as.data.frame(svytotal(~bili, d2pbc_survey)),
  svyquantile(~bili, d2pbc_survey, quantiles = 0.5, ci = TRUE, df = NULL) %>%
    {cbind(as.data.frame(.[[1]][,'quantile']),
           as.data.frame(SE(.)))},
  as.data.frame(svyvar(~bili, d2pbc_survey)),
  data.frame(albumin = svyratio(~bili, ~albumin, d2pbc_survey)[[1]],
             se = sqrt(unname(svyratio(~bili, ~albumin, d2pbc_survey)[[2]])))
) %>% dplyr::bind_cols() %>%
  setNames(c("mean", "mean_se", "total", "total_se", "median", "median_se",
             "var", "var_se", "ratio", "ratio_se")) %>%
  tibble::as_tibble()

srvyr_results <- d2pbc_srvyr %>%
  summarize(mean = survey_mean(bili),
            total = survey_total(bili),
            median = survey_median(bili, df = NULL),
            var = survey_var(bili),
            ratio = survey_ratio(bili, albumin))

test_that("as_survey_twophase gets same mean / total / median / var / ratio in srvyr",
          expect_df_equal(survey_results, srvyr_results))

srvyr_results_vartypeNULL <- d2pbc_srvyr %>%
  summarize(mean = survey_mean(bili, vartype = NULL),
            total = survey_total(bili, vartype = NULL),
            median = survey_median(bili, vartype = NULL),
            var = survey_var(bili, vartype = NULL),
            ratio = survey_ratio(bili, albumin, vartype = NULL))

test_that("as_survey_twophase gets same mean / total / median / var / ratio in srvyr - with vartype = NULL",
          expect_df_equal(select(survey_results, -ends_with("_se")),
                                 srvyr_results_vartypeNULL))

survey_results <- list(
  as.data.frame(svyby(~bili, ~sex, d2pbc_survey, svymean)),
  as.data.frame(svyby(~bili, ~sex, d2pbc_survey, svytotal)),
  as.data.frame(suppressWarnings(svyby(~bili, ~sex, d2pbc_survey, svyquantile,
                                       quantiles = 0.5, ci = TRUE, df = NULL)))
) %>%
  as.data.frame() %>%
  dplyr::bind_cols() %>%
  setNames(c("sex", "mean", "mean_se", "sex2", "total", "total_se",
             "sex3", "median", "median_se")) %>%
  select(-sex2, -sex3) %>%
  tibble::as_tibble()

attr(survey_results, "svyby") <- NULL
attr(survey_results, "call") <- NULL

suppressWarnings(srvyr_results <- d2pbc_srvyr %>%
  group_by(sex) %>%
  summarize(mean = survey_mean(bili),
            total = survey_total(bili),
            median = survey_median(bili, vartype = "se", df = NULL)))

test_that(
  "as_survey_twophase gets same mean / total / median / ratio in (grouped)",
  expect_df_equal(survey_results, srvyr_results))
