suppressPackageStartupMessages({
  library(dplyr)
  library(survey)
  library(srvyr)
  library(RSQLite)
  require(MonetDBLite) # MonetDBLite not on all CRAN systems
})

data(api)

for (db_type in c("RSQLite", "MonetDBLite")) {

  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    context(paste0("Database objects work as expected - ", db_type))

    if (db_type == "RSQLite") {
      db_file <- tempfile()
      my_db <- src_sqlite(db_file, create = T)
    } else if (db_type == "MonetDBLite") {
      my_db <- src_monetdblite()
    }

    api_db <- copy_to(my_db, apistrat, temporary = FALSE)

    api_db <- tbl(my_db, sql("SELECT * FROM apistrat"))

    svys <- list(db = api_db %>%
                   as_survey_design(strata = stype, weights = pw,
                                    uid = cds),
                 local = apistrat %>%
                   as_survey_design(strata = stype, weights = pw)
    )

    # Rep weights
    data(scd)
    # use BRR replicate weights from Levy and Lemeshow
    scd <- scd %>%
      mutate(rep1 = 2 * c(1, 0, 1, 0, 1, 0),
             rep2 = 2 * c(1, 0, 0, 1, 0, 1),
             rep3 = 2 * c(0, 1, 1, 0, 0, 1),
             rep4 = 2 * c(0, 1, 0, 1, 1, 0),
             uidxxx = row_number())

    scd_db <- copy_to(my_db, scd, temporary = FALSE)

    scd_db <- tbl(my_db, sql("SELECT * FROM scd"))

    suppressWarnings(
      svysrep <- list(
        db = scd_db %>%
          as_survey_rep(type = "BRR", repweights = starts_with("rep"),
                        combined_weights = FALSE, uid = uidxxx),
        local = scd %>%
          as_survey_rep(type = "BRR", repweights = starts_with("rep"),
                        combined_weights = FALSE)
      )
    )

    # Twophase
    data(mu284)
    mu284_1 <- mu284 %>%
      dplyr::slice(c(1:15, rep(1:5, n2[1:5] - 3))) %>%
      mutate(id = row_number(),
             sub = rep(c(TRUE, FALSE), c(15, 34-15)))

    mu284_1_db <- copy_to(my_db, mu284_1, temporary = FALSE)

    mu284_1_db <- tbl(my_db, sql("SELECT * FROM mu284_1"))

  }

  test_that("Mutate works", {
    skip_on_cran()
    expect_equal(svys$db %>%
                   mutate(apidiff = api00 - api99) %>%
                   select(apidiff) %>%
                   .$variables %>%
                   collect() %>%
                   select(-matches("SRVYR_ORDER")) %>%
                   mutate(apidiff = as.integer(apidiff)),
                 svys$local %>%
                   mutate(apidiff = api00 - api99) %>%
                   select(apidiff) %>%
                   .$variables
    )
  })


  test_that("multiple uid works", {
    skip_on_cran()
    expect_equal(api_db %>%
                   as_survey_design(strata = stype, weights = pw,
                                    uid = c(stype, cds)) %>%
                   .$uid %>%
                   names(),
                 srvyr:::get_uid_names(2)
    )
  })

  test_that("Ungrouped summaries work", {
    skip_on_cran()
    expect_equal(svys$db %>%
                   summarize(x = survey_mean(api99),
                             y = survey_median(api99),
                             z = survey_ratio(api99, api00)) %>%
                   as.matrix(),
                 svys$local %>%
                   summarize(x = survey_mean(api99),
                             y = survey_median(api99),
                             z = survey_ratio(api99, api00)) %>%
                   as.matrix(),
                 tolerance = 0.00001 # tolerance not supported for all.equal.tbl_svy
    )
  })

  test_that("grouped survey_mean and survey_total work", {
    skip_on_cran()
    expect_equal(svys$db %>%
                   group_by(stype) %>%
                   summarize(x = survey_mean(api99),
                             y = survey_total(api99)) %>%
                   select(-stype) %>%
                   as.matrix(),
                 svys$local %>%
                   group_by(stype) %>%
                   summarize(x = survey_mean(api99),
                             y = survey_total(api99)) %>%
                   mutate(stype = as.character(stype)) %>%
                   select(-stype) %>%
                   as.matrix(),  # tolerance not supported for all.equal.tbl_svy
                 tolerance = 0.00001 # dbs aren't careful about factor vs char
    )
  })

  test_that("grouped factor (single) work", {
    skip_on_cran()
    expect_equal(svys$db %>%
                   group_by(stype) %>%
                   summarize(x = survey_mean()) %>%
                   select(-stype) %>%
                   as.matrix(),
                 svys$local %>%
                   group_by(stype) %>%
                   summarize(x = survey_mean()) %>%
                   mutate(stype = as.character(stype)) %>%
                   select(-stype) %>%
                   as.matrix(),  # tolerance not supported for all.equal.tbl_svy
                 tolerance = 0.00001 # dbs aren't careful about factor vs char
    )
  })

  test_that("grouped factor (multiple) work", {
    skip_on_cran()
    expect_equal(svys$db %>%
                   group_by(stype, awards) %>%
                   summarize(x = survey_mean()) %>%
                   select(-stype) %>%
                   as.matrix(),
                 svys$local %>%
                   group_by(stype, awards) %>%
                   summarize(x = survey_mean()) %>%
                   mutate(stype = as.character(stype)) %>%
                   select(-stype) %>%
                   as.matrix(),  # tolerance not supported for all.equal.tbl_svy
                 tolerance = 0.00001 # dbs aren't careful about factor vs char
    )
  })

  test_that("na.rm works", {
    skip_on_cran()
    expect_equal(svys$db %>%
                   mutate(api99_cap = ifelse(api99 > 800, NA, api99)) %>%
                   summarize(x = survey_mean(api99_cap, na.rm = TRUE)) %>%
                   as.matrix(),
                 svys$local %>%
                   mutate(api99_cap = ifelse(api99 > 800, NA, api99)) %>%
                   summarize(x = survey_mean(api99_cap, na.rm = TRUE)) %>%
                   as.matrix(),
                 tolerance = 0.00001 # tolerance not supported for all.equal.tbl_svy
    )
  })

  test_that("Filter works - design", {
    skip_on_cran()
    expect_equal(svys$db %>%
                   filter(stype == "H") %>%
                   summarize(y = survey_median(api99, vartype = "se")) %>%
                   as.matrix(),
                 svys$local %>%
                   filter(stype == "H") %>%
                   summarize(y = survey_median(api99, vartype = "se")) %>%
                   as.matrix(),
                 tolerance = 0.00001 # tolerance not supported for all.equal.tbl_svy
    )
  })

  test_that("Filter works - rep", {
    skip_on_cran()
    expect_equal(svysrep$db %>%
                   filter(alive > 40) %>%
                   summarize(y = survey_median(arrests, vartype = "se")) %>%
                   as.matrix(),
                 svysrep$local %>%
                   filter(alive > 40) %>%
                   summarize(y = survey_median(arrests, vartype = "se")) %>%
                   as.matrix(),
                 tolerance = 0.00001 # tolerance not supported for all.equal.tbl_svy
    )
  })

  test_that("na.rm works - rep", {
    skip_on_cran()
    expect_equal(svysrep$db %>%
                   mutate(alive2 = ifelse(alive > 40, NA, alive)) %>%
                   summarize(y = survey_mean(alive2, vartype = "se", na.rm = TRUE)) %>%
                   as.matrix(),
                 svysrep$local %>%
                   mutate(alive2 = ifelse(alive > 40, NA, alive)) %>%
                   summarize(y = survey_mean(alive2, vartype = "se", na.rm = TRUE)) %>%
                   as.matrix(),
                 tolerance = 0.00001 # tolerance not supported for all.equal.tbl_svy
    )
  })

  test_that("grouped survey_mean and survey_total work", {
    skip_on_cran()
    expect_equal(suppressWarnings(svys$db %>%
                                    group_by(stype) %>%
                                    summarize(x = survey_median(api99, vartype = "se")) %>%
                                    select(-stype) %>%
                                    as.matrix()),
                 suppressWarnings(svys$local %>%
                                    group_by(stype) %>%
                                    summarize(x = survey_median(api99, vartype = "se")) %>%
                                    select(-stype) %>%
                                    as.matrix()),  # tolerance not supported for all.equal.tbl_svy
                 tolerance = 0.00001 # dbs aren't careful about factor vs char
    )
  })

  test_that("grouped survey_mean and survey_total work", {
    skip_on_cran()
    expect_equal(suppressWarnings(svys$db %>%
                                    group_by(stype) %>%
                                    summarize(x = survey_ratio(api99, api00, vartype = "se")) %>%
                                    select(-stype) %>%
                                    as.matrix()),
                 suppressWarnings(svys$local %>%
                                    group_by(stype) %>%
                                    summarize(x = survey_ratio(api99, api00, vartype = "se")) %>%
                                    select(-stype) %>%
                                    as.matrix()),  # tolerance not supported for all.equal.tbl_svy
                 tolerance = 0.00001 # dbs aren't careful about factor vs char
    )
  })

  test_that("twophase has error for dbs", {
    skip_on_cran()
    expect_error(mu284_1_db %>%
                   as_survey_twophase(id = list(id1, id), strata = list(NULL, id1),
                                      fpc = list(n1, NULL), subset = sub),
                 "Twophase(.+)database")
  })

  test_that("non-unique uid has error", {
    skip_on_cran()
    expect_error(api_db %>%
                   as_survey_design(strata = stype, weights = pw,
                                    uid = stype),
                 "unique")
  })

  test_that("Ungrouped summaries work - replicates", {
    skip_on_cran()
    expect_equal(svysrep$db %>%
                   summarize(x = survey_mean(arrests),
                             y = survey_median(arrests),
                             z = survey_ratio(arrests, alive)) %>%
                   as.matrix(),
                 svysrep$local %>%
                   summarize(x = survey_mean(arrests),
                             y = survey_median(arrests),
                             z = survey_ratio(arrests, alive)) %>%
                   as.matrix(),
                 tolerance = 0.00001 # tolerance not supported for all.equal.tbl_svy
    )
  })

}
