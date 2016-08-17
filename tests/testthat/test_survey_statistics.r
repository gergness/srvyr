context("Quick tests for summary stats (ratio / quantile)")

library(srvyr)
library(survey)

df_test <- 30

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
  summarize(ratio = survey_ratio(api00, api99, vartype = "ci", level = 0.9),
            mdn = survey_median(api00, vartype = "ci", level = 0.9)) %>%
  select(-ratio, -mdn_q50) %>%
  unlist()

ratio <- svyratio(~api00, ~api99, dstrata)
ratio <- confint(ratio, level = 0.9, df = degf(dstrata))
mdn <- svyquantile(~api00, dstrata, quantile = 0.5, ci = TRUE, alpha = 0.1)
mdn <- confint(mdn)
out_survey <- c(ratio[1], ratio[2], mdn[1], mdn[2])
names(out_survey) <- c("ratio_low", "ratio_upp", "mdn_q50_low", "mdn_q50_upp")

test_that("median/ratio with CIs respect level parameter (ungrouped)",
          expect_equal(out_srvyr, out_survey))


suppressWarnings(out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarize(ratio = survey_ratio(api00, api99, vartype = "ci", level = 0.9),
            mdn = survey_median(api00, vartype = "ci", level = 0.9)) %>%
  select(-ratio, -mdn_q50, -stype)
)

ratio <- svyby(~api00, ~stype, denominator = ~api99, dstrata, svyratio)
ratio <- confint(ratio, level = 0.9, df = degf(dstrata))

suppressWarnings(mdn <- svyby(~api00, ~stype, dstrata, svyquantile,
                              quantile = 0.5, ci = TRUE, alpha = 0.1, vartype = "ci") %>%
  data.frame() %>%
  select(-api00, -stype))

out_survey <- dplyr::bind_cols(data.frame(ratio), mdn)
names(out_survey) <- c("ratio_low", "ratio_upp", "mdn_q50_low", "mdn_q50_upp")

test_that("median/ratio with CIs respect level parameter (grouped)",
          expect_equal(out_srvyr, out_survey))


out_survey <- svyratio(~api99, ~api00, dstrata, deff = TRUE)

out_srvyr <- dstrata %>%
  summarise(survey_ratio = survey_ratio(api99, api00, deff = TRUE, vartype = "ci", df = df_test))

test_that("deff works for ungrouped survey total",
          expect_equal(c(out_survey[[1]], deff(out_survey)[[1]]),
                       c(out_srvyr[["survey_ratio"]][[1]], out_srvyr[["survey_ratio_deff"]][[1]])))

test_that("df works for ungrouped survey total",
          expect_equal(confint(out_survey, df = df_test)[c(1, 2)],
                       c(out_srvyr[["survey_ratio_low"]][[1]], out_srvyr[["survey_ratio_upp"]][[1]])))





out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_ratio = survey_ratio(api99, api00, deff = TRUE, vartype = "ci", df = df_test))

temp_survey <- svyby(~api99, ~stype, dstrata, svyratio, deff = TRUE, vartype = c("se", "ci"),
                     denominator = ~api00)
out_survey <- temp_survey %>%
  data.frame() %>%
  dplyr::tbl_df() %>%
  rename(survey_ratio = api99.api00, survey_ratio_low = ci_l, survey_ratio_upp = ci_u,
         survey_ratio_deff = `DEff`) %>%
  select(-se.api99.api00)

out_survey[, c("survey_ratio_low", "survey_ratio_upp")] <-
  confint(temp_survey, df = df_test)

test_that("deff and df work for grouped survey total",
          expect_equal(out_srvyr, out_survey))


out_survey <- svyquantile(~api99, dstrata, c(0.5), ci = TRUE, df = df_test)

out_srvyr <- dstrata %>%
  summarise(survey = survey_median(api99, vartype = "ci", df = df_test))

test_that("df works for ungrouped survey total",
          expect_equal(confint(out_survey)[c(1, 2)],
                       c(out_srvyr[["survey_q50_low"]][[1]], out_srvyr[["survey_q50_upp"]][[1]])))





out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(survey = survey_median(api99, vartype = "ci", df = df_test))
)

temp_survey <- suppressWarnings(svyby(~api99, ~stype, dstrata, svyquantile, quantiles = c(0.5), ci = TRUE,
                     vartype = c("se", "ci"), df = df_test))
out_survey <- temp_survey %>%
  data.frame() %>%
  dplyr::tbl_df() %>%
  rename(survey_q50 = api99, survey_q50_low = ci_l, survey_q50_upp = ci_u) %>%
  select(-se)

test_that("df works for grouped survey quantile",
          expect_equal(out_srvyr, out_survey))


data(scd, package = "survey")

scd <- scd %>%
  mutate(rep1 = 2 * c(1, 0, 1, 0, 1, 0),
         rep2 = 2 * c(1, 0, 0, 1, 0, 1),
         rep3 = 2 * c(0, 1, 1, 0, 0, 1),
         rep4 = 2 * c(0, 1, 0, 1, 1, 0))

suppressWarnings(mysvy <- scd %>%
  as_survey_rep(type = "BRR", repweights = starts_with("rep"),
                combined_weights = FALSE))

results_srvyr <- mysvy %>%
  summarize(x = survey_median(arrests, interval_type = "probability"))

results_survey <- svyquantile(~arrests, mysvy, quantiles = 0.5,
                              interval_type = "probability")

test_that("srvyr allows you to select probability for interval_type of replicate weights",
          expect_equal(results_srvyr[[1]], results_survey[[1]]))


results_srvyr <- mysvy %>%
  summarize(x = survey_median(arrests))

results_survey <- svyquantile(~arrests, mysvy, quantiles = 0.5)

test_that("srvyr does the right thing by default for quantiles of replicate surveys",
          expect_equal(results_srvyr[[1]], results_survey[[1]]))
