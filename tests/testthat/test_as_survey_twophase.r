context("as_survey_twophase works as expected.")
library(srvyr)
library(survey)

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
  as.data.frame(svyquantile(~bili, d2pbc_survey, quantiles = 0.5)),
  data.frame(albumin = svyratio(~bili, ~albumin, d2pbc_survey)[[1]],
             se = sqrt(unname(svyratio(~bili, ~albumin, d2pbc_survey)[[2]])))
) %>% dplyr::bind_cols() %>%
  setNames(c("mean", "mean_se", "total", "total_se", "median_q50", "ratio",
             "ratio_se")) %>%
  dplyr::tbl_df()

srvyr_results <- d2pbc_srvyr %>%
  summarize(mean = survey_mean(bili),
            total = survey_total(bili),
            median = survey_median(bili),
            ratio = survey_ratio(bili, albumin))


test_that("as_survey_twophase gets same mean / total / median / ratio in srvyr",
          expect_equal(survey_results, srvyr_results))


survey_results <- list(
  as.data.frame(svyby(~bili, ~sex, d2pbc_survey, svymean)),
  as.data.frame(svyby(~bili, ~sex, d2pbc_survey, svytotal)),
  as.data.frame(suppressWarnings(svyby(~bili, ~sex, d2pbc_survey, svyquantile,
                                       quantiles = 0.5, ci = TRUE)))
) %>% dplyr::bind_cols() %>%
  setNames(c("sex", "mean", "mean_se", "sex2", "total", "total_se",
             "sex3", "median_q50", "median_q50_se")) %>%
  select(-sex2, -sex3) %>%
  dplyr::tbl_df()

attr(survey_results, "svyby") <- NULL
attr(survey_results, "call") <- NULL

suppressWarnings(srvyr_results <- d2pbc_srvyr %>%
  group_by(sex) %>%
  summarize(mean = survey_mean(bili),
            total = survey_total(bili),
            median = survey_median(bili, vartype = "se")))

test_that(
  "as_survey_twophase gets same mean / total / median / ratio in (grouped)",
  expect_equal(survey_results, srvyr_results))
