context("filtering joins (semi_join and anti_join) work")

suppressPackageStartupMessages({
  library(survey)
  library(srvyr)
  library(dplyr)
})

source("utilities.R")

# Set up example data ----

  data(api)

  ##_ Create simple stratified survey design object ----
    stratified_design <- apistrat %>%
      as_survey_design(strata = stype, weights = pw)

  ##_ Create clustered survey design object ----
    cluster_design <- as_survey_design(
      .data = apiclus1,
      id = dnum,
      weights = pw,
      fpc = fpc
    )

  ##_ Create survey design object with calibration weights ----
  ##_ NOTE: The survey package uses special behavior when subsetting such survey designs.
  ##_       Rows are never removed, the weights are simply set effectively to zero (technically, Inf)

    ### Add raking weights for school type
      pop.types <- data.frame(stype=c("E","H","M"), Freq=c(4421,755,1018))
      pop.schwide <- data.frame(sch.wide=c("No","Yes"), Freq=c(1072,5122))

      raked_design <- rake(
        cluster_design,
        sample.margins = list(~stype,~sch.wide),
        population.margins = list(pop.types, pop.schwide)
      )

# semi_join ----

      test_that(
        "semi_join works with `by = NULL`", {
        # Stratified design
        expect_equal(
          ## Calculate statistic, after using a filtering join
          object = stratified_design %>%
            semi_join(y = filter(apistrat, stype == "E")) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            pull("stat"),
          ## Calculate statistic after manually filtering
          expected = stratified_design %>%
            filter(stype == "E") %>%
            summarize(stat = survey_mean(pcttest)) %>%
            pull("stat")
        )

        # Cluster design
        expect_equal(
          ## Calculate statistic, after using a filtering join
          object = cluster_design %>%
            semi_join(y = filter(apiclus1, stype == "E")) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            pull("stat"),
          ## Calculate statistic after manually filtering
          expected = cluster_design %>%
            filter(stype == "E") %>%
            summarize(stat = survey_mean(pcttest)) %>%
            pull("stat")
        )

        # Calibration weighted design
        expect_equal(
          ## Calculate statistic, after using a filtering join
          object = raked_design %>%
            semi_join(y = filter(apiclus1, stype == "E")) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            pull("stat"),
          ## Calculate statistic after manually filtering
          expected = raked_design %>%
            filter(stype == "E") %>%
            summarize(stat = survey_mean(pcttest)) %>%
            pull("stat")
        )
      })

      test_that(
        "semi_join works with supplied `by` argument", {
          # Stratified design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = stratified_design %>%
              semi_join(y = filter(apistrat, stype == "E"),
                        by = "stype") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = stratified_design %>%
              filter(stype == "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )

          # Cluster design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = cluster_design %>%
              semi_join(y = filter(apiclus1, stype == "E"),
                        by = "stype") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = cluster_design %>%
              filter(stype == "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )

          # Calibration weighted design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = raked_design %>%
              semi_join(y = filter(apiclus1, stype == "E"),
                        by = "stype") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = raked_design %>%
              filter(stype == "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )
        })

# anti_join ----

      test_that(
        "anti_join works with `by = NULL`", {
          # Stratified design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = stratified_design %>%
              anti_join(y = filter(apistrat, stype == "E")) %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = stratified_design %>%
              filter(stype != "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )

          # Cluster design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = cluster_design %>%
              anti_join(y = filter(apiclus1, stype == "E")) %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = cluster_design %>%
              filter(stype != "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )

          # Calibration weighted design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = raked_design %>%
              anti_join(y = filter(apiclus1, stype == "E")) %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = raked_design %>%
              filter(stype != "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )
        })

      test_that(
        "anti_join works with supplied `by` argument", {
          # Stratified design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = stratified_design %>%
              anti_join(y = filter(apistrat, stype == "E"),
                        by = "stype") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = stratified_design %>%
              filter(stype != "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )

          # Cluster design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = cluster_design %>%
              anti_join(y = filter(apiclus1, stype == "E"),
                        by = "stype") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = cluster_design %>%
              filter(stype != "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )

          # Calibration weighted design
          expect_equal(
            ## Calculate statistic, after using a filtering join
            object = raked_design %>%
              anti_join(y = filter(apiclus1, stype == "E"),
                        by = "stype") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat"),
            ## Calculate statistic after manually filtering
            expected = raked_design %>%
              filter(stype != "E") %>%
              summarize(stat = survey_mean(pcttest)) %>%
              pull("stat")
          )
        })
