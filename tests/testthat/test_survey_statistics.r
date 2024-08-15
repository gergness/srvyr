context("Quick tests for summary stats (ratio / quantile)")

library(srvyr)
library(survey)
source("utilities.R")

df_test <- 30

data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

################################################################################
# survey_ratio
out_survey <- svyratio(~api00, ~api99, dstrata)

out_srvyr <- dstrata %>%
  summarise(api_ratio = survey_ratio(api00, api99))

test_that("survey_ratio works for ungrouped surveys",
          expect_equal(c(out_survey[[1]], sqrt(out_survey$var)),
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))

out_srvyr_vartypeNULL <- dstrata %>%
  summarise(api_ratio = survey_ratio(api00, api99, vartype = NULL))

test_that("survey_ratio works for ungrouped surveys - with vartype = NULL",
          expect_equal(c(out_survey[[1]]),
                       out_srvyr_vartypeNULL[[1]]))


out_survey <- svyby(~api00, ~stype, denominator = ~api99, dstrata, svyratio) %>%
  as.data.frame()

out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(api_ratio = survey_ratio(api00, api99))

test_that("survey_ratio works for grouped surveys",
          expect_true(all(out_survey == out_srvyr)))

out_srvyr_vartypeNULL <- dstrata %>%
  group_by(stype) %>%
  summarise(api_ratio = survey_ratio(api00, api99, vartype = NULL))

test_that("survey_ratio works for grouped surveys - with vartype=NULL",
          expect_true(all(select(out_survey,
                                 -starts_with("se.")) == out_srvyr_vartypeNULL)))

################################################################################
# survey_quantile
out_survey <- svyquantile(~api00, dstrata, c(0.5, 0.75), df = NULL) %>%
  {data.frame(
    api00_q50 = .[[1]]['0.5','quantile'], api00_q75 = .[[1]]['0.75','quantile'],
    api00_q50_se = .[[1]]['0.5', 'se'], api00_q75_se = .[[1]]['0.75', 'se'],
    api00_q50_low = .[[1]]['0.5', 'ci.2.5'], api00_q75_low = .[[1]]['0.75', 'ci.2.5'],
    api00_q50_upp = .[[1]]['0.5', 'ci.97.5'],  api00_q75_upp = .[[1]]['0.75', 'ci.97.5'],
    api00_q50_var = vcov(.)[1,1], api00_q75_var = vcov(.)[2,2]
  )} %>%
  mutate(api00_q50_cv = api00_q50_se/api00_q50,
         api00_q75_cv = api00_q75_se/api00_q75)

out_srvyr <- dstrata %>%
  summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                    vartype = c("se", "ci", "var", "cv"), df = NULL))

test_that("survey_quantile works for ungrouped surveys",
          expect_equal(unname(unlist(out_survey[colnames(out_survey)])),
                       unname(unlist(out_srvyr[colnames(out_survey)]))))

out_srvyr_vartypeNULL <- dstrata %>%
  summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                    vartype = NULL))

test_that("survey_quantile works for ungrouped surveys - with vartype=NULL",
          expect_equal(unname(unlist(select(out_survey, -matches("_(se|var|cv|low|upp)$")))),
                       unname(unlist(out_srvyr_vartypeNULL))))

out_survey <- suppressWarnings(
  svyby(~api00, ~stype, dstrata, svyquantile,
        quantiles = c(0.5, 0.75), ci = TRUE, df = NULL,
        vartype = c("se"))) %>%
  as.data.frame() %>%
  rename(api00_q50 = api00.0.5, api00_q75 = api00.0.75,
         api00_q50_se = se.api00.0.5, api00_q75_se = se.api00.0.75)

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                      vartype = "se", df = NULL)))

test_that("survey_quantile works for grouped surveys - with se",
          expect_equal(unlist(out_survey[,colnames(out_survey)]),
                       unlist(out_srvyr[,colnames(out_survey)])))

out_srvyr_vartypeNULL <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                      vartype = NULL)))

test_that("survey_quantile works for grouped surveys - with vartype=NULL",
          expect_identical(unname(unlist(select(out_survey, -ends_with("_se")))),
                           unname(unlist(out_srvyr_vartypeNULL))))

out_survey <- suppressWarnings(
  svyby(~api00, ~stype + awards, dstrata, svyquantile,
        quantiles = c(0.5, 0.75), ci = TRUE, df = NULL)) %>%
  as.data.frame() %>%
  rename(
    api00_q50 = api00.0.5, api00_q75 = api00.0.75,
    api00_q50_se = se.api00.0.5, api00_q75_se = se.api00.0.75
  ) %>%
  dplyr::arrange(stype, awards)

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype, awards) %>%
    summarise(api00 = survey_quantile(api00, quantiles = c(0.5, 0.75),
                                      vartype = "se", df = NULL))) %>%
  ungroup() %>%
  dplyr::arrange(stype, awards)

test_that(
  "survey_quantile works for grouped surveys - multiple grouping variables",
  expect_equal(unlist(out_srvyr[,colnames(out_survey)]),
               unlist(out_survey[,colnames(out_srvyr)])))

################################################################################
# survey_median
out_survey <- svyquantile(~api00, dstrata, c(0.5, 0.75), df = NULL) %>%
  {data.frame(
    api00 = .[[1]]['0.5','quantile'],
    api00_se = .[[1]]['0.5', 'se'],
    api00_low = .[[1]]['0.5', 'ci.2.5'],
    api00_upp = .[[1]]['0.5', 'ci.97.5'],
    api00_var = vcov(.)[1,1]
  )} %>%
  mutate(api00_cv = api00_se/api00)

out_srvyr <- dstrata %>%
  summarise(api00 = survey_median(api00, vartype = c("se", "ci", "var", "cv"), df = NULL))

test_that("survey_median works for ungrouped surveys",
          expect_equal(unname(unlist(out_survey[colnames(out_survey)])),
                       unname(unlist(out_srvyr[colnames(out_survey)]))))

out_srvyr <- dstrata %>%
  summarise(api00 = survey_median(api00, df = NULL))

test_that("survey_median works for ungrouped surveys - no ci",
          expect_equal(unname(unlist(out_survey[,c("api00", "api00_se")])),
                       unname(unlist(out_srvyr))))

out_srvyr_vartypeNULL <- dstrata %>%
  summarise(api00 = survey_median(api00, vartype = NULL))

test_that("survey_median works for ungrouped surveys - with vartype=NULL",
          expect_equal(unname(unlist(select(out_survey, api00))),
                       unname(unlist(out_srvyr_vartypeNULL))))

out_survey <- suppressWarnings(
  svyby(~api00, ~stype, dstrata, svyquantile,
        quantiles = 0.5, ci = TRUE, df = NULL))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_median(api00, vartype = "se", df = NULL)))

test_that("survey_median works for grouped surveys - with se",
          expect_equal(unname(unlist(out_survey)),
                       unname(unlist(out_srvyr))))

out_srvyr_vartypeNULL <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_median(api00, vartype = NULL)))

test_that("survey_median works for grouped surveys - with vartype=NULL",
          expect_identical(unname(unlist(select(out_survey, -starts_with("se")))),
                           unname(unlist(out_srvyr_vartypeNULL))))

################################################################################
# level parameter in survey_ratio and survey_quantile
out_srvyr <- dstrata %>%
  summarize(ratio = survey_ratio(api00, api99, vartype = "ci", level = 0.9),
            mdn = survey_median(api00, vartype = "ci", level = 0.9)) %>%
  select(-ratio, -mdn) %>%
  unlist()

ratio <- svyratio(~api00, ~api99, dstrata)
ratio <- confint(ratio, level = 0.9, df = degf(dstrata))
mdn <- svyquantile(~api00, dstrata, quantile = 0.5, ci = TRUE, alpha = 0.1, df = NULL)
mdn <- confint(mdn)
out_survey <- c(ratio[1], ratio[2], mdn[1], mdn[2])
names(out_survey) <- c("ratio_low", "ratio_upp", "mdn_low", "mdn_upp")

test_that("median/ratio with CIs respect level parameter (ungrouped)",
          expect_df_equal(out_srvyr, out_survey))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarize(ratio = survey_ratio(api00, api99, vartype = "ci", level = 0.9),
              mdn = survey_median(api00, vartype = "ci", level = 0.9)) %>%
    select(-ratio, -mdn, -stype))

ratio <- svyby(~api00, ~stype, denominator = ~api99, dstrata, svyratio)
ratio <- confint(ratio, level = 0.9, df = degf(dstrata))

mdn <- suppressWarnings(
  svyby(~api00, ~stype, dstrata, svyquantile,
        quantile = 0.5, ci = TRUE, alpha = 0.1, vartype = "ci", df = NULL) %>%
    data.frame() %>%
    select(-api00, -stype))

out_survey <- dplyr::bind_cols(data.frame(ratio), mdn)
names(out_survey) <- c("ratio_low", "ratio_upp", "mdn_low", "mdn_upp")

test_that("median/ratio with CIs respect level parameter (grouped)",
          expect_df_equal(out_srvyr, out_survey))

################################################################################
# deff and df parameters
out_survey <- svyratio(~api99, ~api00, dstrata, deff = TRUE)

out_srvyr <- dstrata %>%
  summarise(survey_ratio = survey_ratio(api99, api00, deff = TRUE,
                                        vartype = "ci", df = df_test))

test_that("deff works for ungrouped survey total",
          expect_equal(c(out_survey[[1]], deff(out_survey)[[1]]),
                       c(out_srvyr[["survey_ratio"]][[1]],
                         out_srvyr[["survey_ratio_deff"]][[1]])))

test_that("df works for ungrouped survey total",
          expect_equal(confint(out_survey, df = df_test)[c(1, 2)],
                       c(out_srvyr[["survey_ratio_low"]][[1]],
                         out_srvyr[["survey_ratio_upp"]][[1]])))

test_that(
  "deff works with `deff='replace'` (#46)",
  {
    data(api)
    dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat,
                        fpc = ~fpc)
    survey_answer <- unname(deff(svymean(~api99, dstrat, deff = "replace")))

    srvyr_answer <- dstrat %>%
      as_survey() %>%
      summarize(api99 = survey_mean(api99, deff = "replace")) %>%
      dplyr::pull(api99_deff)

    expect_equal(survey_answer, srvyr_answer)
  }
)

out_srvyr <- dstrata %>%
  group_by(stype) %>%
  summarise(survey_ratio = survey_ratio(api99, api00, deff = TRUE,
                                        vartype = "ci", df = df_test))

temp_survey <- svyby(~api99, ~stype, dstrata, svyratio, deff = TRUE,
                     vartype = c("se", "ci"), denominator = ~api00)
out_survey <- temp_survey %>%
  data.frame() %>%
  tibble::as_tibble() %>%
  rename(survey_ratio = api99.api00, survey_ratio_low = ci_l,
         survey_ratio_upp = ci_u, survey_ratio_deff = `DEff`) %>%
  select(-se.api99.api00)

sv_ci <- confint(temp_survey, df = df_test)
out_survey$survey_ratio_low <- unname(sv_ci[, 1])
out_survey$survey_ratio_upp <- unname(sv_ci[, 2])

test_that("deff and df work for grouped survey ratio",
          expect_df_equal(out_srvyr, out_survey))

out_survey <- svyquantile(~api99, dstrata, c(0.5), ci = TRUE, df = df_test)

out_srvyr <- dstrata %>%
  summarise(survey = survey_median(api99, vartype = "ci", df = df_test))

test_that("df works for ungrouped survey quantile",
          expect_equal(confint(out_survey)[c(1, 2)],
                       c(out_srvyr[["survey_low"]][[1]], out_srvyr[["survey_upp"]][[1]])))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(survey = survey_median(api99, vartype = "ci", df = df_test)))

temp_survey <- suppressWarnings(
  svyby(~api99, ~stype, dstrata, svyquantile, quantiles = c(0.5), ci = TRUE,
        vartype = c("se", "ci"), df = df_test))

out_survey <- temp_survey %>%
  data.frame() %>%
  tibble::as_tibble() %>%
  rename(survey = api99, survey_low = ci_l, survey_upp = ci_u) %>%
  select(-starts_with("se."))

test_that("df works for grouped survey quantile",
          expect_df_equal(out_srvyr, out_survey))

################################################################################
# replicate weights
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
  summarize(x = survey_median(arrests, interval_type = "quantile"))

results_survey <- svyquantile(~arrests, mysvy, quantiles = 0.5,
                              interval.type = "quantile")[[1]]

test_that("srvyr allows you to select `quantile` for interval_type of replicate weights",
          expect_equal(results_srvyr[[1]], results_survey[[1]]))

results_srvyr <- mysvy %>%
  summarize(x = survey_median(arrests))

results_survey <- svyquantile(~arrests, mysvy, quantiles = 0.5)[[1]]

test_that("srvyr does the right thing by default for quantiles of replicate surveys",
          expect_equal(results_srvyr[[1]], results_survey[[1]]))

test_that(
  "Can calculate multiple quantiles on grouped data (#38)",
  {
    dstrata <- apistrat %>%
      as_survey_design(strata = stype, weights = pw)

    srvyr <- suppressWarnings(
      dstrata %>%
        group_by(awards) %>%
        summarise(api99 = survey_quantile(api99, c(0.25, 0.5, 0.75),
                                          vartype = c("se", "ci"), df = NULL)))

    survey <- suppressWarnings(
      svyby( ~api99, ~awards, dstrata, svyquantile,
             quantiles = c(0.25, 0.5, 0.75), ci = TRUE, vartype = c("se", "ci"), df = NULL))

    expect_equal(srvyr$api99_q25, survey$`api99.0.25`)
    expect_equal(srvyr$api99_q25_se, survey$`se.api99.0.25`)
    expect_equal(srvyr$api99_q25_low, survey$`ci_l.api99.0.25`)
    expect_equal(srvyr$api99_q25_upp, survey$`ci_u.api99.0.25`)
  }
)

################################################################################
# survey_var and survey_sd
test_that(
  "survey_var works for ungrouped surveys - with se",
  {
    srvyr <- dstrata %>%
      summarise(api99 = survey_var(api99, vartype = "se"))

    survey <- svyvar(~api99, dstrata) %>%
      as.data.frame() %>%
      setNames(c("api99", "api99_se"))

    expect_df_equal(srvyr, survey)
  }
)

test_that(
  "survey_var works for ungrouped surveys - with ci",
  {
    srvyr <- dstrata %>%
      summarise(api99 = survey_var(api99, vartype = "ci"))

    survey <- svyvar(~api99, dstrata) %>%
      {cbind(select(as.data.frame(.), variance),
             as.data.frame(confint(., df = degf(dstrata))))} %>%
      setNames(c("api99", "api99_low", "api99_upp"))

    expect_df_equal(srvyr, survey)
  }
)

test_that(
  "survey_var works for ungrouped surveys - with vartype=NULL",
  {
    srvyr <- dstrata %>%
      summarise(api99 = survey_var(api99, vartype = NULL))

    survey <- svyvar(~api99, dstrata) %>%
      as.data.frame() %>%
      setNames(c("api99", "api99_se"))

    expect_df_equal(srvyr,
                    select(survey, -ends_with("_se")))
  }
)

test_that(
  "survey_var works for grouped surveys - with se",
  {
    srvyr <- dstrata %>%
      group_by(awards) %>%
      summarise(api99 = survey_var(api99, vartype = "se"))

    survey <- svyby(~api99, ~awards, dstrata, svyvar) %>%
      setNames(c("awards", "api99", "api99_se"))

    expect_df_equal(srvyr, survey)
  }
)

test_that(
  "survey_var works for grouped surveys - with ci",
  {
    srvyr <- dstrata %>%
      group_by(awards) %>%
      summarise(api99 = survey_var(api99, vartype = "ci"))

    survey <- svyby(~api99, ~awards, dstrata, svyvar) %>%
      {cbind(select(., -se),
             confint(., df = degf(dstrata)))} %>%
      setNames(c("awards", "api99", "api99_low", "api99_upp"))

    expect_df_equal(srvyr, survey)
  }
)

test_that(
  "survey_var works for grouped surveys - with vartype=NULL",
  {
    srvyr <- dstrata %>%
      group_by(awards) %>%
      summarise(api99 = survey_var(api99, vartype = NULL))

    survey <- svyby(~api99, ~awards, dstrata, svyvar) %>%
      setNames(c("awards", "api99", "api99_se"))

    expect_df_equal(srvyr,
                    select(survey, -ends_with("_se")))
  }
)

test_that(
  "survey_var doesn't works for grouped surveys if there are groups with less than 2 observations",
  {
    expect_error(
      {
        dstrata %>%
          group_by(dname) %>%
          summarise(api99 = survey_var(api99))
      },
      class = "error",
      "Population variance can't be computed because some groups contain less than 2 observations"
    )
  }
)

test_that(
  "survey_sd works",
  {
    srvyr <- dstrata %>%
      summarise(sd = survey_sd(api99),
                var = survey_var(api99, vartype = NULL))
    expect_equal(srvyr$sd^2, srvyr$var)
  }
)

################################################################################
# assertion errors
test_that("survey_STATISTIC functions fail on ungrouped surveys with no x provided",
          {
            errorPattern = "Variable should be provided as an argument to survey_STATISTIC() or grouped survey object should be used."
            errorPattern = "Variable should be provided as an argument to survey_STATISTIC()."
            expect_error(summarise(dstrata, survey_quantile()),
                         sub("STATISTIC", "quantile", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_median()),
                         sub("STATISTIC", "quantile", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_var()),
                         sub("STATISTIC", "var", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_sd()),
                         sub("STATISTIC", "var", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_ratio()),
                         "numerator", class = "error")
            expect_error(summarise(dstrata, survey_ratio(api99)),
                         "denominator", class = "error")
          }
)

test_that("some survey_STATISTIC functions fail on grouped surveys with no x provided",
          {
            dstrataGrp = group_by(dstrata, stype)
            errorPattern = "Variable should be provided as an argument to survey_STATISTIC()."
            expect_error(summarise(dstrata, survey_quantile()),
                         sub("STATISTIC", "quantile", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_median()),
                         sub("STATISTIC", "quantile", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_var()),
                         sub("STATISTIC", "var", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_sd()),
                         sub("STATISTIC", "var", errorPattern), fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_ratio()), fixed = TRUE, class = "error",
                         "numerator")
            expect_error(summarise(dstrata, survey_ratio(api99)),
                         "denominator", class = "error")
          }
)

test_that("survey_STATISTIC functions fail with x being a factor",
          {
            errorPattern = "Factor not allowed in survey functions, should be used as a grouping variable."
            expect_error(summarise(dstrata, survey_mean(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_total(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_ratio(stype, api99)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_ratio(api99, stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_quantile(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_median(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_var(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_sd(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            dstrataGrp = group_by(dstrata, awards)
            expect_error(summarise(dstrataGrp, survey_mean(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_total(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_ratio(stype, api99)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_ratio(api99, stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_quantile(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_median(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_var(stype)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_sd(stype)),
                         errorPattern, fixed = TRUE, class = "error")
          }
)

test_that("survey_STATISTIC functions fail with x being a character",
          {
            errorPattern = "Character vectors not allowed in survey functions, should be used as a grouping variable."
            expect_error(summarise(dstrata, survey_mean(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_total(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_ratio(name, api99)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_ratio(api99, name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_quantile(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_median(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_var(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrata, survey_sd(name)),
                         errorPattern, fixed = TRUE, class = "error")
            dstrataGrp = group_by(dstrata, awards)
            expect_error(summarise(dstrataGrp, survey_mean(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_total(name))
                         , errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_ratio(name, api99)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_ratio(api99, name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_quantile(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_median(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_var(name)),
                         errorPattern, fixed = TRUE, class = "error")
            expect_error(summarise(dstrataGrp, survey_sd(name)),
                         errorPattern, fixed = TRUE, class = "error")
          }
)

test_that("some other errors and warnings",
          {
            expect_warning(summarise(dstrata,
                                     survey_mean(api00 > 875,
                                                 proportion = TRUE, deff = TRUE)),
                           "Cannot calculate design effects on proportions.",
                           fixed = TRUE)
            dstrataGrp = group_by(dstrata, stype)
            expect_warning(summarise(dstrataGrp,
                                     none = survey_mean(api00 > 875,
                                                        proportion = TRUE, deff = TRUE)),
                           "Cannot calculate design effects on proportions.",
                           fixed = TRUE)
          }
)


test_that(
  "summarize behaves well with differently sized groups (#49)",
  {
    data(api)
    dclus1 <- as_survey(apiclus1, id = dnum, weights = pw, fpc = fpc)

    results <- dclus1 %>%
      mutate(yr.rnd = factor(yr.rnd, levels = c("No", "Yes", "Maybe"))) %>%
      group_by(yr.rnd, .drop = TRUE) %>%
      summarize(
        n = survey_total(),
        ratio = survey_ratio(api00, api99)
      )

    expect_equal(as.character(results$yr.rnd), c("No", "Yes"))

    results <- dclus1 %>%
      mutate(yr.rnd = factor(yr.rnd, levels = c("No", "Yes", "Maybe"))) %>%
      group_by(yr.rnd, .drop = FALSE) %>%
      summarize(
        n = survey_total(),
        ratio = survey_ratio(api00, api99)
      )

    expect_equal(as.character(results$yr.rnd), c("No", "Yes", "Maybe"))

  }
)

test_that(
  "unweighted works in simple ungrouped case", {
    data(api, package = "survey")
    dclus1 <- as_survey_design(apiclus1, id = dnum, weights = pw, fpc = fpc)
    test <- dclus1 %>%
      summarize(n = unweighted(n()))

    expect_equal(test$n, c(183))
  }
)

test_that(
  "unweighted works in simple grouped case", {
    data(api, package = "survey")
    dclus1 <- as_survey_design(apiclus1, id = dnum, weights = pw, fpc = fpc)
    test <- dclus1 %>%
      group_by(sch.wide) %>%
      summarize(n = unweighted(n()))

    expect_equal(test$n, c(23, 160))
  }
)

test_that(
  "unweighted allows passing functions from environment", {
    data(api, package = "survey")
    dclus1 <- as_survey_design(apiclus1, id = dnum, weights = pw, fpc = fpc)

    test_reference <- dclus1 %>%
      group_by(sch.wide) %>%
      summarize(n = unweighted(length(api99)))


    no_quo_wrapper <- function(x) {
      unweighted(length(x))
    }

    test1 <- dclus1 %>%
      group_by(sch.wide) %>%
      summarize(n = no_quo_wrapper(api99))

    expect_equal(test1, test_reference)


    right_wrapper <- function(x) {
      x <- rlang::enquo(x)
      unweighted(length(!!x))
    }

    test2 <- dclus1 %>%
      group_by(sch.wide) %>%
      summarize(n = right_wrapper(api99))

    expect_equal(test2, test_reference)

    # see issue https://github.com/gergness/srvyr/issues/103
    func_of_svy <- function(svy) {
      func_inside_func <- function(x) {
        length(x)
      }

      svy %>%
        group_by(sch.wide) %>%
        summarize(n = srvyr::unweighted(func_inside_func(api99)))
    }

    test3 <- dclus1 %>%
      func_of_svy()

    expect_equal(test3, test_reference)
  }
)

test_that("unweighted allows named arguments",  {
  data(api, package = "survey")
  dclus1 <- as_survey_design(apiclus1, id = dnum, weights = pw, fpc = fpc)

  expect_equal(
    dclus1 %>% summarize(n = unweighted(a = n())),
    tibble::tibble(na = 183)
  )

  expect_equal(
    dclus1 %>% summarize(unweighted(a = n())),
    tibble::tibble(a = 183)
  )
})

test_that(
  "unweighted works with filtered data in calibrated or PPS designs", {

    # First check for calibrated designs
    data(api, package = "survey")
    dclus1 <- as_survey_design(apiclus1, id = dnum, weights = pw, fpc = fpc)

    pop.types <- data.frame(stype=c("E","H","M"), Freq=c(4421,755,1018))
    pop.schwide <- data.frame(sch.wide=c("No","Yes"), Freq=c(1072,5122))

    raked_design <- rake(dclus1,
                         sample.margins = list(~stype,~sch.wide),
                         population.margins = list(pop.types, pop.schwide))

    # Check when filtering returns at least one row
    out_calib <- raked_design %>%
      filter(sch.wide == "Yes") %>%
      group_by(stype) %>%
      summarize(sample_size = unweighted(n()))

    out_noncalib <- dclus1 %>%
      filter(sch.wide == "Yes") %>%
      group_by(stype) %>%
      summarize(sample_size = unweighted(n()))

    expect_equal(out_calib[['sample_size']],
                 out_noncalib[['sample_size']])

    # Check when filtering returns zero rows

    out_calib <- raked_design %>%
      filter(sch.wide == "Fake Category") %>%
      summarize(sample_size = unweighted(n()))

    out_noncalib <- dclus1 %>%
      filter(sch.wide == "Fake Category") %>%
      summarize(sample_size = unweighted(n()))

    expect_equal(out_calib[['sample_size']],
                 expected = 0)
    expect_equal(out_noncalib[['sample_size']],
                 expected = 0)

    # Next check for PPS design
    data(election, package = "survey")

    non_pps_design <- as_survey_design(election_pps, id = 1)
    pps_design <- as_survey_design(election_pps, id = 1, fpc = p, pps = "brewer")

    # Check correct results when filtering returns at least one row
    out_nonpps <- non_pps_design %>%
      filter(County == "Los Angeles") %>%
      summarize(n_rows = unweighted(n()))

    out_pps <- non_pps_design %>%
      filter(County == "Los Angeles") %>%
      summarize(n_rows = unweighted(n()))

    expect_equal(out_pps[['n_rows']],
                 out_nonpps[['n_rows']])

    # Check correct results when filtering returns zero rows
    out_nonpps <- non_pps_design %>%
      filter(County == "Fake Category") %>%
      summarize(n_rows = unweighted(n()))

    out_pps <- pps_design %>%
      filter(County == "Fake Category") %>%
      summarize(n_rows = unweighted(n()))

    expect_equal(out_pps[['n_rows']],
                 expected = 0)
    expect_equal(out_nonpps[['n_rows']],
                 expected = 0)
  }
)

test_that(
  "Can groupby before or after a filter (#59)", {
    data(api, package = "survey")

    set.seed(10)
    apistrat$sch.wide[sample(1:nrow(apistrat), 40)] <- NA

    d <- apistrat %>% as_survey_design(strata = stype, weights = pw) %>%
      mutate(testvar = as.numeric(sch.wide == "Yes"))

    t2 <- d %>%
      group_by(stype) %>%
      filter(!is.na(testvar)) %>%
      summarize(a = survey_mean(testvar, na.rm = T, proportion = T))

    t3 <- d %>%
      filter(!is.na(testvar)) %>%
      group_by(stype) %>%
      summarize(a = survey_mean(testvar, na.rm = T, proportion = T))

    expect_equal(t2, t3)
  }
)

test_that("survey_ci_prop handles no row case (#156)", {
  issue156_data <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = rep(NA_character_, 100)
  )

  issue156_survey <- srvyr::as_survey(issue156_data, strata = groups)

  results <- issue156_survey %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("groups"))) %>%
    dplyr::filter(!is.na(value), .preserve = T) %>%
    srvyr::summarise(x = srvyr::survey_prop(proportion = T))

  expect_equal(names(results), c("groups", "x", "x_se"))
  expect_equal(results$groups, c("a", "b"))
  expect_true(all(is.na(results$x)))
  expect_true(all(is.na(results$x_se)))


  results <- issue156_survey %>%
    dplyr::group_by(dplyr::across(dplyr::any_of("groups"))) %>%
    dplyr::filter(!is.na(value), .preserve = T) %>%
    srvyr::summarise(x = srvyr::survey_prop(vartype = c("ci", "se"), proportion = T))

  expect_equal(names(results), c("groups", "x", "x_low", "x_upp", "x_se"))
  expect_equal(results$groups, c("a", "b"))
  expect_true(all(is.na(results$x)))
  expect_true(all(is.na(results$x_low)))
  expect_true(all(is.na(results$x_upp)))
  expect_true(all(is.na(results$x_se)))
})


################################################################################
# survey_old_quantile
out_survey <- oldsvyquantile(~api00, dstrata, c(0.5, 0.75), ci = TRUE, df = NULL) %>%
  {cbind(as.data.frame(.$quantiles),
         SE(.) %>%
           as.list() %>%
           setNames(paste0(names(.), "_se")) %>%
           as.data.frame(check.names = FALSE))}

out_srvyr <- dstrata %>%
  summarise(api00 = survey_old_quantile(api00, quantiles = c(0.5, 0.75), df = NULL))

test_that("survey_old_quantile works for ungrouped surveys - no ci (with se)",
          expect_equal_or_warn(unname(unlist(out_survey)),
                               unname(unlist(out_srvyr))))

out_srvyr_vartypeNULL <- dstrata %>%
  summarise(api00 = survey_old_quantile(api00, quantiles = c(0.5, 0.75),
                                        vartype = NULL))

test_that("survey_old_quantile works for ungrouped surveys - with vartype=NULL",
          expect_equal_or_warn(unname(unlist(select(out_survey, -ends_with("_se")))),
                               unname(unlist(out_srvyr_vartypeNULL))))

out_survey <- oldsvyquantile(~api00, dstrata, c(0.5, 0.75), ci = TRUE, df = NULL)

out_srvyr <- dstrata %>%
  summarise(api00 = survey_old_quantile(api00, quantiles = c(0.5, 0.75),
                                        vartype = "ci", df = NULL))

test_that("survey_old_quantile works for ungrouped surveys - with ci",
          expect_equal_or_warn(c(out_survey$CIs[[1]], out_survey$CIs[[2]]),
                               c(out_srvyr[["api00_q50_low"]][[1]],
                                 out_srvyr[["api00_q50_upp"]][[1]])))

out_survey <- suppressWarnings(
  svyby(~api00, ~stype, dstrata, oldsvyquantile,
        quantiles = c(0.5, 0.75), ci = TRUE, df = NULL))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_old_quantile(api00, quantiles = c(0.5, 0.75),
                                          vartype = "se", df = NULL)))

test_that("survey_old_quantile works for grouped surveys - with se",
          expect_equal_or_warn(c(out_survey$`0.5`[[1]], out_survey[["se.0.5"]][[1]]),
                               c(out_srvyr[["api00_q50"]][[1]],
                                 out_srvyr[["api00_q50_se"]][[1]])))

out_srvyr_vartypeNULL <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_old_quantile(api00, quantiles = c(0.5, 0.75),
                                          vartype = NULL)))

test_that("survey_old_quantile works for grouped surveys - with vartype=NULL",
          expect_identical_or_warn(unname(unlist(select(out_survey, -starts_with("se.")))),
                                   unname(unlist(out_srvyr_vartypeNULL))))

out_survey <- suppressWarnings(
  svyby(~api00, ~stype + awards, dstrata, oldsvyquantile,
        quantiles = c(0.5, 0.75), ci = TRUE, df = NULL))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype, awards) %>%
    summarise(api00 = survey_old_quantile(api00, quantiles = c(0.5, 0.75),
                                          vartype = "se", df = NULL)))

test_that(
  "survey_old_quantile works for grouped surveys - multiple grouping variables",
  expect_equal_or_warn(c(out_survey$`0.5`[[1]], out_survey[["se.0.5"]][[1]]),
                       c(out_srvyr[["api00_q50"]][[1]], out_srvyr[["api00_q50_se"]][[1]])))

################################################################################
# survey_old_median
out_survey <- oldsvyquantile(~api00, dstrata, c(0.5), ci = TRUE, df = NULL) %>%
  {cbind(as.data.frame(.$quantiles),
         SE(.) %>%
           as.list() %>%
           setNames(paste0(names(.), "_se")) %>%
           as.data.frame(check.names = FALSE))}

out_srvyr <- dstrata %>%
  summarise(api00 = survey_old_median(api00, df = NULL))

test_that("survey_old_median works for ungrouped surveys - no ci",
          expect_equal_or_warn(unname(unlist(out_survey)),
                               unname(unlist(out_srvyr))))

out_srvyr_vartypeNULL <- dstrata %>%
  summarise(api00 = survey_old_median(api00, vartype = NULL))

test_that("survey_old_median works for ungrouped surveys - with vartype=NULL",
          expect_equal_or_warn(unname(unlist(select(out_survey, -ends_with("_se")))),
                               unname(unlist(out_srvyr_vartypeNULL))))

out_survey <- suppressWarnings(
  svyby(~api00, ~stype, dstrata, oldsvyquantile,
        quantiles = 0.5, ci = TRUE, df = NULL))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_old_median(api00, vartype = "se", df = NULL)))

test_that("survey_old_median works for grouped surveys - with se",
          expect_equal_or_warn(unname(unlist(out_survey)),
                               unname(unlist(out_srvyr))))

out_srvyr_vartypeNULL <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(api00 = survey_old_median(api00, vartype = NULL)))

test_that("survey_old_median works for grouped surveys - with vartype=NULL",
          expect_identical_or_warn(unname(unlist(select(out_survey, -starts_with("se")))),
                                   unname(unlist(out_srvyr_vartypeNULL))))

################################################################################
# old_survey_quantile: level parameter in survey_quantile
out_srvyr <- dstrata %>%
  summarize(mdn = survey_old_median(api00, vartype = "ci", level = 0.9)) %>%
  select(-mdn) %>%
  unlist()

mdn <- oldsvyquantile(~api00, dstrata, quantile = 0.5, ci = TRUE, alpha = 0.1, df = NULL)
mdn <- confint(mdn)
out_survey <- c(mdn[1], mdn[2])
names(out_survey) <- c("mdn_low", "mdn_upp")

test_that("survey_old_median: median with CIs respect level parameter (ungrouped)",
          expect_df_equal_or_warn(out_srvyr, out_survey))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarize(mdn = survey_old_median(api00, vartype = "ci", level = 0.9)) %>%
    select(-mdn, -stype))

mdn <- suppressWarnings(
  svyby(~api00, ~stype, dstrata, oldsvyquantile,
        quantile = 0.5, ci = TRUE, alpha = 0.1, vartype = "ci", df = NULL) %>%
    data.frame() %>%
    select(-api00, -stype))

out_survey <- mdn
names(out_survey) <- c("mdn_low", "mdn_upp")

test_that("old_survey_median: median with CIs respect level parameter (grouped)",
          expect_df_equal_or_warn(out_srvyr, out_survey))

################################################################################
# survey_old_quantile: deff and df parameters

out_survey <- oldsvyquantile(~api99, dstrata, c(0.5), ci = TRUE, df = df_test)

out_srvyr <- dstrata %>%
  summarise(survey = survey_old_median(api99, vartype = "ci", df = df_test))

test_that("survey_old_quantile: df works for ungrouped survey quantile",
          expect_equal_or_warn(confint(out_survey)[c(1, 2)],
                               c(out_srvyr[["survey_low"]][[1]], out_srvyr[["survey_upp"]][[1]])))

out_srvyr <- suppressWarnings(
  dstrata %>%
    group_by(stype) %>%
    summarise(survey = survey_old_median(api99, vartype = "ci", df = df_test)))

temp_survey <- suppressWarnings(
  svyby(~api99, ~stype, dstrata, oldsvyquantile, quantiles = c(0.5), ci = TRUE,
        vartype = c("se", "ci"), df = df_test))

out_survey <- temp_survey %>%
  data.frame() %>%
  tibble::as_tibble() %>%
  rename(survey = api99, survey_low = ci_l, survey_upp = ci_u) %>%
  select(-se)

test_that("survey_old_quantile: df works for grouped survey quantile",
          expect_df_equal_or_warn(out_srvyr, out_survey))

################################################################################
# old_survey_quantile with replicate weights
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
  summarize(x = survey_old_median(arrests, interval_type = "probability"))

results_survey <- oldsvyquantile(~arrests, mysvy, quantiles = 0.5,
                                 interval_type = "probability")

test_that("survey_old_quantile: srvyr allows you to select probability for interval_type of replicate weights",
          expect_equal_or_warn(results_srvyr[[1]], results_survey[[1]]))

results_srvyr <- mysvy %>%
  summarize(x = survey_old_median(arrests))

results_survey <- oldsvyquantile(~arrests, mysvy, quantiles = 0.5)

test_that("survey_old_quantile: srvyr does the right thing by default for quantiles of replicate surveys",
          expect_equal_or_warn(results_srvyr[[1]], results_survey[[1]]))

test_that(
  "survey_old_quantile: Can calculate multiple quantiles on grouped data (#38)",
  {
    dstrata <- apistrat %>%
      as_survey_design(strata = stype, weights = pw)

    srvyr <- suppressWarnings(
      dstrata %>%
        group_by(awards) %>%
        summarise(api99 = survey_old_quantile(api99, c(0.25, 0.5, 0.75),
                                              vartype = c("se", "ci"), df = NULL)))

    survey <- suppressWarnings(
      svyby( ~api99, ~awards, dstrata, oldsvyquantile,
             quantiles = c(0.25, 0.5, 0.75), ci = TRUE, vartype = c("se", "ci"), df = NULL))

    expect_equal_or_warn(srvyr$api99_q25, survey$`0.25`)
    expect_equal_or_warn(srvyr$api99_q25_se, survey$`se.0.25`)
    expect_equal_or_warn(srvyr$api99_q25_low, survey$`ci_l.0.25_api99`)
    expect_equal_or_warn(srvyr$api99_q25_upp, survey$`ci_u.0.25_api99`)
  }
)
