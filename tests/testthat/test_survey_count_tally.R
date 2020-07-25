context("survey_count/survey_tally")

test_that("Basic tally examples work", {
  dstrata <- apistrat %>%
    as_survey_design(strata = stype, weights = pw)

  expected <- svytotal(~awards, dstrata, na.rm = TRUE)

  res <- dstrata %>%
    group_by(awards) %>%
    survey_tally()

  expect_equal(res$n, as.vector(expected))
  expect_equal(res$n_se, unname(SE(expected)))
})

test_that("Basic count example works", {
  dstrata <- apistrat %>%
    as_survey_design(strata = stype, weights = pw)

  expected <- svytotal(~awards, dstrata, na.rm = TRUE)

  res <- dstrata %>%
    survey_count(awards)

  expect_equal(res$n, as.vector(expected))
  expect_equal(res$n_se, unname(SE(expected)))
})

test_that("Parameters for survey_count/tally work", {
  dstrata <- apistrat %>%
    as_survey_design(strata = stype, weights = pw) %>%
    mutate(n = 1)

  expected <- svyby(~n, ~awards, dstrata, svytotal, na.rm = TRUE, vartype = "cv")

  res <- dstrata %>%
    survey_count(awards, wt = n, sort = TRUE, vartype = "cv")

  expect_equal(res$n, sort(as.vector(expected$n), decreasing = TRUE))
  expect_equal(res$n_cv, as.vector(expected$cv)[rev(order(as.vector(expected$n)))])
})

test_that("survey_count/tally works with integer grouping variables", {
  dstrata <- apistrat %>%
    as_survey_design(strata = stype, weights = pw)

  factor_survey <- dstrata %>%
    mutate(api_diff = dplyr::case_when(api00 - api99 > 0 ~ "Increase",
                                       api00 - api99 < 0 ~ "Decrease",
                                       api00 == api99 ~ "Unchanged")) %>%
    mutate(fct_api_diff = factor(api_diff, levels = c("Decrease", "Unchanged", "Increase")))

  int_survey <- factor_survey %>%
    mutate(int_api_diff = as.integer(fct_api_diff))

  int_tally_out <- int_survey %>%
    group_by(int_api_diff) %>%
    survey_tally()

  factor_tally_out <- factor_survey %>%
    group_by(fct_api_diff) %>%
    survey_tally()

  int_count_out <- int_survey %>%
    survey_count(int_api_diff)

  factor_count_out <- factor_survey %>%
    survey_count(fct_api_diff)

  expect_equal(object = int_tally_out[['n']],
               expected = factor_tally_out[['n']])
  expect_equal(object = int_tally_out[['n_se']],
               expected = factor_tally_out[['n_se']])
  expect_equal(object = int_count_out[['n']],
               expected = factor_count_out[['n']])
  expect_equal(object = int_count_out[['n_se']],
               expected = factor_count_out[['n_se']])
})

test_that("survey_count/tally works with numeric double grouping variables", {
  dstrata <- apistrat %>%
    as_survey_design(strata = stype, weights = pw) %>%
    mutate(dbl_group_var = sample(c(48908512.23121595515890890346346,
                                    0.298908534689384963334663431241,
                                    89068093804986346.12141251885551),
                                  size = nrow(apistrat),
                                  replace = TRUE),
           fct_group_var = as.factor(as.character(dbl_group_var)))

  dbl_tally_out <- dstrata %>%
    group_by(dbl_group_var) %>%
    survey_tally()

  factor_tally_out <- dstrata %>%
    group_by(fct_group_var) %>%
    survey_tally()

  dbl_count_out <- dstrata %>%
    survey_count(dbl_group_var)

  factor_count_out <- dstrata %>%
    survey_count(fct_group_var)

  expect_equal(object = dbl_tally_out[['n']],
               expected = factor_tally_out[['n']])
  expect_equal(object = dbl_tally_out[['n_se']],
               expected = factor_tally_out[['n_se']])
  expect_equal(object = dbl_count_out[['n']],
               expected = factor_count_out[['n']])
  expect_equal(object = dbl_count_out[['n_se']],
               expected = factor_count_out[['n_se']])
})

test_that("no noticeable precision loss of numeric grouping variable in survey_count/tally ", {
  dstrata <- apistrat %>%
    as_survey_design(strata = stype, weights = pw) %>%
    mutate(numeric_group_var = sample(c(48908512.23121595515890890346346,
                                        0.298908534689384963334663431241,
                                        89068093804986346.12141251885551),
                                      size = nrow(apistrat),
                                      replace = TRUE),
           char_group_var = as.character(numeric_group_var))

  numeric_tally_out <- dstrata %>%
    group_by(numeric_group_var) %>%
    survey_tally()

  expect_equal(object = sort(unique(numeric_tally_out[['numeric_group_var']])),
               expected = sort(unique(dstrata$variables[['numeric_group_var']])),
               tolerance = .Machine$double.eps)

})
