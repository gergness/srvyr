context("DB backed surveys work.")
suppressPackageStartupMessages({
  library(survey)
  library(srvyr)
  library(dplyr)
})

source("utilities.R")

if (suppressPackageStartupMessages(require(dbplyr))) {
  has_rsqlite <- suppressPackageStartupMessages(require(RSQLite))
  has_monetdb <- suppressPackageStartupMessages(require(MonetDBLite))
  data(api)

  dbs_to_run <- c("RSQLite", "MonetDBLite")

  for (db in dbs_to_run) {
    if (db == "RSQLite" && has_rsqlite) {
      con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
      cleaned <- dplyr::select(apistrat, -full)
      names(cleaned) <- gsub("\\.", "", names(cleaned))
      apistrat_db <- copy_to(con, cleaned)
      db_avail <- TRUE
    } else if (db == "RSQLite" && !has_rsqlite){
      db_avail <- FALSE
    } else if (db == "MonetDBLite" && has_monetdb) {
      con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), path = ":memory:")
      cleaned <- dplyr::select(apistrat, -full)
      names(cleaned) <- gsub("\\.", "", names(cleaned))
      apistrat_db <- copy_to(con, cleaned)
      db_avail <- TRUE
    } else if (db == "MonetDBLite" && !has_monetdb) {
      db_avail <- FALSE
    }

    test_that(paste0("DB backed survey tests - ", db), {
      skip_if_not(db_avail)
      # Can create a svy design from a tbl_lazy
      dstrata <- apistrat_db %>%
        as_survey_design(strata = stype, weights = pw)

      local_dstrata <- cleaned %>%
        as_survey_design(strata = stype, weights = pw)

      # Can do a basic summarize
      expect_df_equal(
        dstrata %>%
          summarize(
            api99_mn = survey_mean(api99),
            api99_mdn = survey_median(api99),
            api99_tot = survey_total(api99)),
        local_dstrata %>%
          summarize(
            api99_mn = survey_mean(api99),
            api99_mdn = survey_median(api99),
            api99_tot = survey_total(api99))
      )

      # Can do a summarize with a calculation in it
      expect_df_equal(
        dstrata %>%
          summarize(api_diff = survey_mean(api00 - api99)),
        local_dstrata %>%
          summarize(api_diff = survey_mean(api00 - api99))
      )

      # Can do a grouped summarize
      expect_df_equal(
        suppressWarnings(dstrata %>%
          group_by(stype = as.character(stype)) %>%
          summarize(api99 = survey_mean(api99))),
        local_dstrata %>%
          group_by(stype = as.character(stype)) %>%
          summarize(api99 = survey_mean(api99))
      )

      # Can filter and summarize
      expect_df_equal(
        suppressWarnings(dstrata %>%
          filter(stype == "E") %>%
          summarize(api99 = survey_mean(api99))),
        local_dstrata %>%
          filter(stype == "E") %>%
          summarize(api99 = survey_mean(api99))
      )

      # Can mutate and summarize
      expect_df_equal(
        suppressWarnings(dstrata %>%
          mutate(api_diff = api00 - api99) %>%
          summarize(api99 = survey_mean(api_diff))),
        local_dstrata %>%
          mutate(api_diff = api00 - api99) %>%
          summarize(api99 = survey_mean(api_diff))
      )

      # Can collect and then use survey functions
      expect_df_equal(
        suppressWarnings(dstrata %>%
                           select(api99, stype) %>%
                           collect() %>%
                           {survey::svyglm(api99 ~ stype, .)}) %>%
          coef(),
        survey::svyglm(api99 ~ stype, local_dstrata) %>%
          coef()
      )
    })

    test_that(paste0("Can get replicate weight surveys - ", db), {
      skip_if_not(db_avail)
      scd <- scd %>%
        mutate(rep1 = 2 * c(1, 0, 1, 0, 1, 0),
               rep2 = 2 * c(1, 0, 0, 1, 0, 1),
               rep3 = 2 * c(0, 1, 1, 0, 0, 1),
               rep4 = 2 * c(0, 1, 0, 1, 1, 0))

      names(scd) <- tolower(names(scd))
      scd_db <- copy_to(con, scd)

      scdrep <- suppressWarnings(scd_db %>%
                                   as_survey_rep(type = "BRR", repweights = starts_with("rep"),
                                                 combined_weights = FALSE))

      scdrep_local <- suppressWarnings(scd %>%
                                         as_survey_rep(type = "BRR", repweights = starts_with("rep"),
                                                       combined_weights = FALSE))

      # Can do a basic summarize
      expect_df_equal(
        scdrep %>%
          summarize(esa = survey_mean(esa)),
        scdrep_local %>%
          summarize(esa = survey_mean(esa))
      )
    })

    test_that(paste0("Can use as_survey"), {
      skip_if_not(db_avail)
      dstrata <- apistrat_db %>%
        as_survey(strata = stype, weights = pw)

      expect_equal(inherits(dstrata$variables, "tbl_lazy"), TRUE)
    })

    if (db_avail) DBI::dbDisconnect(con)
  }


}

db_avail <- (suppressPackageStartupMessages(require(RSQLite)))
test_that("Can convert from survey DB-backed surveys to srvyr ones", {
  skip_if_not(db_avail)
  dbclus1<-svydesign(id=~dnum, weights=~pw, fpc=~fpc,
                     data="apiclus1",dbtype="SQLite", dbname=system.file("api.db",package="survey"))

  mean_survey <- svymean(~api99, dbclus1)

  dbclus1_srvyr <- as_survey(dbclus1)
  mean_srvyr <- summarize(dbclus1_srvyr, x = survey_mean(api99))
  expect_equal(mean_survey[[1]], mean_srvyr$x)
  expect_equal(SE(mean_survey)[[1]], mean_srvyr$x_se)
  dbDisconnect(dbclus1$db$connection)

  db_rclus1<-svrepdesign(weights=~pw, repweights="wt[1-9]+", type="JK1", scale=(1-15/757)*14/15,
                         data="apiclus1rep",dbtype="SQLite", dbname=system.file("api.db",package="survey"), combined=FALSE)
  mean_survey <- svymean(~api99,db_rclus1)

  dbclus1_srvyr <- as_survey(db_rclus1)
  mean_srvyr <- summarize(dbclus1_srvyr, x = survey_mean(api99))

  expect_equal(mean_survey[[1]], mean_srvyr$x)
  expect_equal(SE(mean_survey)[[1]], mean_srvyr$x_se)
  dbDisconnect(db_rclus1$db$connection)

  # Updates from survey-db get converted to srvyr-db
  dbclus1<-svydesign(id=~dnum, weights=~pw, fpc=~fpc,
                     data="apiclus1",dbtype="SQLite", dbname=system.file("api.db",package="survey"))
  dbclus1 <- update(dbclus1, easy_update = api99 + 1)
  easy_mean_survey <- svymean(~easy_update, dbclus1)

  dbclus1_srvyr <- as_survey(dbclus1)
  easy_mean_srvyr <- summarize(dbclus1_srvyr, x = survey_mean(easy_update))

  expect_equal(easy_mean_survey[[1]], easy_mean_srvyr$x)
  expect_equal(SE(easy_mean_survey)[[1]], easy_mean_srvyr$x_se)

  # Subsers from survey-db get converted to srbyr-db
  dbclus1_subset <- subset(dbclus1, stype == "E")
  subset_mean_survey <- svymean(~api99, dbclus1_subset)
  dbclus1_srvyr <- as_survey(dbclus1_subset)
  subset_mean_srvyr <- summarize(dbclus1_srvyr, x = survey_mean(api99))
  expect_equal(subset_mean_survey[[1]], subset_mean_srvyr$x)
  expect_equal(SE(subset_mean_survey)[[1]], subset_mean_srvyr$x_se)

  # Decent warning when update is too complicated for srvyr
  plus_one <- function(x) x + 1
  dbclus1 <- update(dbclus1, hard_update = plus_one(api99))
  expect_warning(as_survey(dbclus1), "Could not convert variable 'hard_update'")

  dbDisconnect(dbclus1$db$connection)
})
