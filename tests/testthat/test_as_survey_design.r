context("Function as_survey_ design works.")
data(api, package = "survey")
df <- data.frame(id = 1:5, strata = c(2, 2, 3, 3, 3), x = 11:15)

 test_that("as_survey_design works with both tbl_dfs and data.frames",
           expect_equal(df %>%
                          as_survey_design(ids = id, strata = strata),
                        df %>%
                          dplyr::tbl_df() %>%
                          as_survey_design(ids = id, strata = strata)))

 test_that(
   "as_survey_design ids argument = 1 doesn't use the first column as an id #7",
   expect_equal(
     apisrs %>% as_survey_design(ids = 1, fpc = fpc),
     apisrs %>% as_survey_design(fpc = fpc)
   ))
