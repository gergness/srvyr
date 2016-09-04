suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(srvyr)
  library(RSQLite)
})



if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  context("Database objects work as expected - designs.")
  data(api)
  db_file <- tempfile()
  my_db <- src_sqlite(db_file, create = T)

  api_db <- copy_to(my_db, apistrat, temporary = FALSE)

  api_db <- tbl(my_db, sql("SELECT * FROM apistrat"))

  svys <- list(db = api_db %>%
                 as_survey_design(strata = stype, weights = pw),
               local = apistrat %>%
                 as_survey_design(strata = stype, weights = pw)
  )
}

test_that("Mutate works",
          expect_equal(svys$db %>%
                         mutate(apidiff = api00 - api99) %>%
                         select(apidiff) %>%
                         .$variables %>%
                         collect(),
                       svys$local %>%
                         mutate(apidiff = api00 - api99) %>%
                         select(apidiff) %>%
                         .$variables,
                       skip_on_cran()
          )
)


test_that("Ungrouped summaries work",
          expect_equal(svys$db %>%
                         summarize(x = survey_mean(api99),
                                   y = survey_median(api99),
                                   z = survey_ratio(api99, api00)),
                       svys$local %>%
                         summarize(x = survey_mean(api99),
                                   y = survey_median(api99),
                                   z = survey_ratio(api99, api00)),
                       skip_on_cran()
          )
)

test_that("grouped survey_mean and survey_total work",
          expect_equal(svys$db %>%
                         group_by(stype) %>%
                         summarize(x = survey_mean(api99),
                                   y = survey_total(api99)),
                       svys$local %>%
                         group_by(stype) %>%
                         summarize(x = survey_mean(api99),
                                   y = survey_total(api99)) %>%
                         mutate(stype = as.character(stype)), # dbs aren't careful about factor vs char
                       skip_on_cran()
          )
)
