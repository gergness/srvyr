context("Quick tests for added summary stats (ratio / quantile)")

library(srvyr)
library(survey)

data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

out_survey <- svyratio(~api00, ~api99, dstrata)

out_srvyr <- dstrata %>%
  summarise(api_ratio = survey_ratio(api00, api99))

test_that("survey_ratio works for ungrouped surveys",
          expect_equal(c(out_survey[[1]], sqrt(out_survey$var)),
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))


out_survey <- svyby(~api00, ~stype, denominator = ~api99, dstrata,
                    svyratio) %>%
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
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))



out_survey <- svyquantile(~api00, dstrata, c(0.5, 0.75), ci = TRUE)

out_srvyr <- dstrata %>%
  summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                    vartype = "ci"))

test_that("survey_quantile works for ungrouped surveys - with ci",
          expect_equal(c(out_survey$CIs[[1]], out_survey$CIs[[2]]),
                       c(out_srvyr[["api00_q50_low"]][[1]],
                         out_srvyr[["api00_q50_upp"]][[1]])))


suppressWarnings(out_survey <- svyby(~api00, ~stype, dstrata, svyquantile,
                                     quantiles = c(0.5, 0.75), ci = TRUE))

suppressWarnings(out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                    vartype = "se")))

test_that("survey_quantile works for grouped surveys - with se",
          expect_equal(c(out_survey$`0.5`[[1]], out_survey[["se.0.5"]][[1]]),
                       c(out_srvyr[["api00_q50"]][[1]],
                         out_srvyr[["api00_q50_se"]][[1]])))

# survey_quantile
out_survey <- svyquantile(~api00, dstrata, c(0.5))


out_srvyr <- dstrata %>%
  summarise(api00 = survey_median(api00))

test_that("survey_quantile works for ungrouped surveys - no ci",
          expect_equal(c(out_survey[[1]]),
                       c(out_srvyr[[1]][[1]])))


suppressWarnings(
  out_survey <- svyby(~api00, ~stype + awards, dstrata, svyquantile,
                      quantiles = c(0.5, 0.75), ci = TRUE)
)

suppressWarnings(
  out_srvyr <- dstrata %>%
    group_by(stype, awards) %>%
    summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                      vartype = "se"))
)

test_that(
  "survey_quantile works for grouped surveys - multiple grouping variables",
  expect_equal(c(out_survey$`0.5`[[1]], out_survey[["se.0.5"]][[1]]),
               c(out_srvyr[["api00_q50"]][[1]], out_srvyr[["api00_q50_se"]][[1]])))



out_srvyr <- dstrata %>%
  summarize(mn = survey_mean(api00, vartype = "ci", level = 0.9),
            ratio = survey_ratio(api00, api99, vartype = "ci", level = 0.9),
            mdn = survey_median(api00, vartype = "ci", level = 0.9)) %>%
  select(-mn, -ratio, -mdn_q50) %>%
  unlist()

mn <- svymean(~api00, dstrata)
mn <- confint(mn, level = 0.9)
ratio <- svyratio(~api00, ~api99, dstrata)
ratio <- confint(ratio, level = 0.9)
mdn <- svyquantile(~api00, dstrata, quantile = 0.5, ci = TRUE, level = 0.9)
mdn <- confint(mdn)
out_survey <- c(mn[1], mn[2], ratio[1], ratio[2], mdn[1], mdn[2])
names(out_survey) <- c("mn_low", "mn_upp", "ratio_low",
                       "ratio_upp", "mdn_q50_low", "mdn_q50_upp")

test_that("mean/median/ratio with CIs respect level parameter (ungrouped)",
          expect_equal(out_srvyr, out_survey))


suppressWarnings(out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarize(mn = survey_mean(api00, vartype = "ci", level = 0.9),
            ratio = survey_ratio(api00, api99, vartype = "ci", level = 0.9),
            mdn = survey_median(api00, vartype = "ci", level = 0.9)) %>%
  select(-mn, -ratio, -mdn_q50, -stype)
)

mn <- svyby(~api00, ~stype, dstrata, svymean)
mn <- confint(mn, level = 0.9)
ratio <- svyby(~api00, ~stype, denominator = ~api99, dstrata, svyratio)
ratio <- confint(ratio, level = 0.9)
suppressWarnings(mdn <- svyby(~api00, ~stype, dstrata, svyquantile,
                              quantile = 0.5, ci = TRUE, level = 0.90))
mdn <- confint(mdn, level = 0.9)
out_survey <- dplyr::bind_cols(data.frame(mn), data.frame(ratio),
                               data.frame(mdn))
names(out_survey) <- c("mn_low", "mn_upp", "ratio_low", "ratio_upp",
                       "mdn_q50_low", "mdn_q50_upp")

test_that("mean/median/ratio with CIs respect level parameter (ungrouped)",
          expect_equal(out_srvyr, out_survey))
