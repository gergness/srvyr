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
