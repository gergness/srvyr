context("DB backed surveys work.")
suppressPackageStartupMessages({
  library(survey)
  library(srvyr)
})

if (suppressPackageStartupMessages(require(dbplyr))) {
  has_rsqlite <- suppressPackageStartupMessages(require(RSQLite))
  has_monetdb <- suppressPackageStartupMessages(require(MonetDBLite))
  data(api)

  dbs_to_run <- c("RSQLite", "MonetDBLite")[c(has_rsqlite, has_monetdb)]

  for (db in dbs_to_run) {
    if (db == "RSQLite") {
      con <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")
      cleaned <- dplyr::select(apistrat, -full)
      names(cleaned) <- stringr::str_replace_all(names(cleaned), "\\.", "")
      apistrat_db <- copy_to(con, cleaned)
    }
    if (db == "MonetDBLite") {
      con <- DBI::dbConnect(MonetDBLite::MonetDBLite(), path = ":memory:")
      cleaned <- dplyr::select(apistrat, -full)
      names(cleaned) <- stringr::str_replace_all(names(cleaned), "\\.", "")
      apistrat_db <- copy_to(con, cleaned)
    }

    test_that(paste0("DB backed survey tests - ", db), {
      # Can create a svy design from a tbl_lazy
      dstrata <- apistrat_db %>%
        as_survey_design(strata = stype, weights = pw)

      local_dstrata <- cleaned %>%
        as_survey_design(strata = stype, weights = pw)

      # Can do a basic summarize
      expect_equal(
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
      expect_equal(
        dstrata %>%
          summarize(api_diff = survey_mean(api00 - api99)),
        local_dstrata %>%
          summarize(api_diff = survey_mean(api00 - api99))
      )

      # Can do a grouped summarize
      expect_equal(
        dstrata %>%
          group_by(stype = as.character(stype)) %>%
          summarize(api99 = survey_mean(api99)),
        local_dstrata %>%
          group_by(stype = as.character(stype)) %>%
          summarize(api99 = survey_mean(api99))
      )

      # Can filter and summarize
      expect_equal(
        dstrata %>%
          filter(stype == "E") %>%
          summarize(api99 = survey_mean(api99)),
        local_dstrata %>%
          filter(stype == "E") %>%
          summarize(api99 = survey_mean(api99))
      )

      # Can mutate and summarize
      expect_equal(
        dstrata %>%
          mutate(api_diff = api00 - api99) %>%
          summarize(api99 = survey_mean(api_diff)),
        local_dstrata %>%
          mutate(api_diff = api00 - api99) %>%
          summarize(api99 = survey_mean(api_diff))
      )
    })

    # Can get replicate weight surveys
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
    expect_equal(
      scdrep %>%
        summarize(esa = survey_mean(esa)),
      scdrep_local %>%
        summarize(esa = survey_mean(esa))
    )

    DBI::dbDisconnect(con)
  }
}
