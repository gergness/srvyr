context("Survey package functions work.")

suppressPackageStartupMessages(library(survey))
data(api)

# Overall
## srvyr
dclus1_srvyr <- apiclus1 %>%
  as_survey(id = dnum, weights = pw, fpc = fpc)

## survey
dclus1_survey <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1,
                           fpc = ~fpc)
survey_chisq <- svychisq(~sch.wide + stype, dclus1_survey)[["p.value"]]

## survey with a tibble
dclus1_survey_tibble <- svydesign(id = ~dnum, weights = ~pw, fpc = ~fpc,
                                  data = dplyr::as_data_frame(apiclus1))
srvyr_chisq <- svychisq(~sch.wide + stype, dclus1_srvyr)[["p.value"]]


test_that("srvyr and survey get same chisq",
          expect_equal(survey_chisq, srvyr_chisq))

test_that("custom method is still needed for svychisq because of issue with tibbles [#23]",
          expect_error(svychisq(~sch.wide + stype, dclus1_survey_tibble)))
