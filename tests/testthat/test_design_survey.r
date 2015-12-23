context("Function design_survey works.")

df <- data.frame(id = 1:5, strata = c(2, 2, 3, 3, 3), x = 11:15)

 test_that("design_survey works with both tbl_dfs and data.frames",
           expect_equal(df %>%
                          design_survey(ids = id, strata = strata),
                        df %>%
                          dplyr::tbl_df() %>%
                          design_survey(ids = id, strata = strata)))

 test_that(
   "design_survey ids argument = 1 doesn't use the first column as an id #7",
   expect_equal(
     apisrs %>% design_survey(ids = 1, fpc = fpc),
     apisrs %>% design_survey(fpc = fpc)
   ))
