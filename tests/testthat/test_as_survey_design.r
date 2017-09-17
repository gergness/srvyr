context("Function as_survey design works.")
df <- data.frame(id = 1:5, strata = c(2, 2, 3, 3, 3), x = 11:15)

 test_that("as_survey_design works with both tbl_dfs and data.frames",
           expect_equal(df %>%
                          as_survey_design(ids = id, strata = strata),
                        df %>%
                          dplyr::tbl_df() %>%
                          as_survey_design(ids = id, strata = strata)))


 test_that("as_survey_design_ works with character",
           expect_equal(df %>%
                          as_survey_design_(ids = "id", strata = "strata"),
                        df %>%
                          as_survey_design(ids = id, strata = strata)))

 test_that("as_survey_design_ works with formulas",
           expect_equal(df %>%
                          as_survey_design_(ids = ~id, strata = ~strata),
                        df %>%
                          as_survey_design(ids = id, strata = strata)))
