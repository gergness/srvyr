context("Function as_survey design works.")
df <- data.frame(id = 1:5, strata = c(2, 2, 3, 3, 3), x = 11:15)

test_that("as_survey_design works with both tbl_dfs and data.frames",
          expect_equal(df %>%
                         as_survey_design(ids = id, strata = strata),
                       df %>%
                         tibble::as_tibble() %>%
                         as_survey_design(ids = id, strata = strata)))

test_that("as_survey_design preserves groups", {
  data(api, package = "survey")

  results <- apistrat %>%
    group_by(stype) %>%
    as_survey_design(strata = stype, weights = pw)

  expect_s3_class(results, "grouped_svy")
  expect_equal(group_vars(results), "stype")

  expect_warning(
    apistrat %>%
      group_by(stype) %>%
      as_survey_design(strata = stype, weights = pw, variables = c("api99", "api00")),
    "Not all grouping variables exist in survey"
  )
})

test_that("as_survey_design can handle NA weights", {
  data(api, package = "survey")

  apistrat_na <- apistrat
  apistrat_na$pw[1:5] <- NA

  expect_warning(
    apistrat_na |>
      as_survey_design(strata = stype, weights = pw, na_weights = "warn"),
    "missing values in weights; records will be dropped"
  )

  expect_error(
    apistrat_na |>
      as_survey_design(strata = stype, weights = pw),
    "missing values in weights and na_weights='fail'"
  )

  expect_error(
    apistrat_na |>
      as_survey_design(strata = stype, weights = pw, na_weights = "fail"),
    "missing values in weights and na_weights='fail'"
  )
})