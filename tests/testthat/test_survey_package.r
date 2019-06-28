context("Survey package functions work.")

suppressPackageStartupMessages(library(survey))
suppressPackageStartupMessages(library(srvyr))
data(api)
source("utilities.R")

# Overall
## srvyr
dclus1_srvyr <- apiclus1 %>%
  as_survey(id = dnum, weights = pw, fpc = fpc)

## survey
dclus1_survey <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1,
                           fpc = ~fpc)
survey_chisq <- suppressWarnings(svychisq(~sch.wide + stype, dclus1_survey)[["p.value"]])

## survey with a tibble
dclus1_survey_tibble <- svydesign(id = ~dnum, weights = ~pw, fpc = ~fpc,
                                  data = tibble::as_tibble(apiclus1))
srvyr_chisq <- suppressWarnings(svychisq(~sch.wide + stype, dclus1_srvyr)[["p.value"]])


test_that("srvyr and survey get same chisq",
          expect_df_equal(survey_chisq, srvyr_chisq))

