context("filter variants (_if, _at, _all), preserve options, and predicate options all work")

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

# filter (with `.preserve = TRUE`) ----

      test_that(
        "with `.preserve = TRUE`, summarize returns explicit results for zero-count groups", {
        # Stratified design
        expect_equal(
          ## Calculate statistic, after grouping by school type and filtering out high schools
          ## With `.preserve = TRUE`, summarize() should still output a row for high schools
          object = stratified_design %>%
            group_by(stype) %>%
            filter(stype != "H", .preserve = TRUE) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            nrow(),
          ## Calculate statistic, after grouping by school type but without filtering
          ## There should be one row for each school type: E, M, H
          expected = stratified_design %>%
            group_by(stype) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            nrow()
        )

        # Cluster design
        expect_equal(
          ## Calculate statistic, after grouping by school type and filtering out high schools
          ## With `.preserve = TRUE`, summarize() should still output a row for high schools
          object = cluster_design %>%
            group_by(stype) %>%
            filter(stype != "H", .preserve = TRUE) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            nrow(),
          ## Calculate statistic, after grouping by school type but without filtering
          ## There should be one row for each school type: E, M, H
          expected = cluster_design %>%
            group_by(stype) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            nrow()
        )

        # Calibration weighted design
        expect_equal(
          ## Calculate statistic, after grouping by school type and filtering out high schools
          ## With `.preserve = TRUE`, summarize() should still output a row for high schools
          object = raked_design %>%
            group_by(stype) %>%
            filter(stype != "H", .preserve = TRUE) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            nrow(),
          ## Calculate statistic, after grouping by school type but without filtering
          ## There should be one row for each school type: E, M, H
          expected = raked_design %>%
            group_by(stype) %>%
            summarize(stat = survey_mean(pcttest)) %>%
            nrow()
        )
      })

# filter_at ----

      test_that(
        "filter_at works with a character vector of variables", {
          # Stratified design
          expect_equal(
            object = stratified_design %>%
              filter_at(c("acs.k3", "acs.46"), is.na) %>%
              nrow(),
            expected = subset(stratified_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )

          # Cluster design
          expect_equal(
            object = cluster_design %>%
              filter_at(c("acs.k3", "acs.46"), is.na) %>%
              nrow(),
            expected = subset(cluster_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )

          # Calibration weighted design
            ## First check that the correct number of rows are retained
              expect_equal(
                object =  raked_design %>%
                  filter_at(c("acs.k3", "acs.46"), is.na) %>%
                  nrow(),
                expected = subset(raked_design,
                                  is.na(acs.k3) & is.na(acs.46)) %>%
                  nrow()
              )
            ## Next check that calculation results match behavior of survey package
              expect_equal(
                object = raked_design %>%
                  filter_at(c("acs.k3", "acs.46"), is.na) %>%
                  summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
                  dplyr::pull("api_ratio"),
                expected = svyratio(
                  numerator = ~ api00, denominator = ~ api99,
                  design = subset(raked_design,
                                  is.na(acs.k3) & is.na(acs.46))
                )[['ratio']][1]
              )
        })

      test_that(
        "filter_at works with a vars() specfication", {
          # Stratified design
          expect_equal(
            object = stratified_design %>%
              filter_at(vars(acs.k3, acs.46), is.na) %>%
              nrow(),
            expected = subset(stratified_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )

          # Cluster design
          expect_equal(
            object = cluster_design %>%
              filter_at(vars(acs.k3, acs.46), is.na) %>%
              nrow(),
            expected = subset(cluster_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )

          # Calibration weighted design
          ## First check that the correct number of rows are retained
          expect_equal(
            object =  raked_design %>%
              filter_at(vars(acs.k3, acs.46), is.na) %>%
              nrow(),
            expected = subset(raked_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )
          ## Next check that calculation results match behavior of survey package
          expect_equal(
            object = raked_design %>%
              filter_at(vars(acs.k3, acs.46), is.na) %>%
              summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
              dplyr::pull("api_ratio"),
            expected = svyratio(
              numerator = ~ api00, denominator = ~ api99,
              design = subset(raked_design,
                              is.na(acs.k3) & is.na(acs.46))
            )[['ratio']][1]
          )
        })

      test_that(
        "filter_at works with an `all_vars()` predicate", {
          # Stratified design
          expect_equal(
            object = stratified_design %>%
              filter_at(vars(acs.k3, acs.46), all_vars( is.na(.) )) %>%
              nrow(),
            expected = subset(stratified_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )

          # Cluster design
          expect_equal(
            object = cluster_design %>%
              filter_at(vars(acs.k3, acs.46), all_vars( is.na(.) )) %>%
              nrow(),
            expected = subset(cluster_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )

          # Calibration weighted design
          ## First check that the correct number of rows are retained
          expect_equal(
            object =  raked_design %>%
              filter_at(vars(acs.k3, acs.46), all_vars( is.na(.) )) %>%
              nrow(),
            expected = subset(raked_design,
                              is.na(acs.k3) & is.na(acs.46)) %>%
              nrow()
          )
          ## Next check that calculation results match behavior of survey package
          expect_equal(
            object = raked_design %>%
              filter_at(vars(acs.k3, acs.46), all_vars( is.na(.) )) %>%
              summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
              dplyr::pull("api_ratio"),
            expected = svyratio(
              numerator = ~ api00, denominator = ~ api99,
              design = subset(raked_design,
                              is.na(acs.k3) & is.na(acs.46))
            )[['ratio']][1]
          )
        })

      test_that(
        "filter_at works with an `any_vars()` predicate", {
          # Stratified design
          expect_equal(
            object = stratified_design %>%
              filter_at(vars(acs.k3, acs.46), any_vars( is.na(.) )) %>%
              nrow(),
            expected = subset(stratified_design,
                              is.na(acs.k3) | is.na(acs.46)) %>%
              nrow()
          )

          # Cluster design
          expect_equal(
            object = cluster_design %>%
              filter_at(vars(acs.k3, acs.46), any_vars( is.na(.) )) %>%
              nrow(),
            expected = subset(cluster_design,
                              is.na(acs.k3) | is.na(acs.46)) %>%
              nrow()
          )

          # Calibration weighted design
          ## First check that the correct number of rows are retained
          expect_equal(
            object =  raked_design %>%
              filter_at(vars(acs.k3, acs.46), any_vars( is.na(.) )) %>%
              nrow(),
            expected = subset(raked_design,
                              is.na(acs.k3) | is.na(acs.46)) %>%
              nrow()
          )
          ## Next check that calculation results match behavior of survey package
          expect_equal(
            object = raked_design %>%
              filter_at(vars(acs.k3, acs.46), any_vars( is.na(.) )) %>%
              summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
              dplyr::pull("api_ratio"),
            expected = svyratio(
              numerator = ~ api00, denominator = ~ api99,
              design = subset(raked_design,
                              is.na(acs.k3) | is.na(acs.46))
            )[['ratio']][1]
          )
        })

# filter_all ----

      test_that(
        "filter_all works with any_vars() predicate", {
          # Stratified design
          expect_equal(
            # Survey design object
            object = stratified_design %>%
              filter_all(
                .vars_predicate = any_vars(grepl("Ocean", .))
              ) %>% nrow(),
            # Underlying data frame (not a survey design object)
            expected = apistrat %>%
              filter_all(
                .vars_predicate = any_vars(grepl("Ocean", .))
              ) %>% nrow()
          )

          # Cluster design
          expect_equal(
            # Survey design object
            object = cluster_design %>%
              filter_all(
                .vars_predicate = any_vars(grepl("Ocean", .))
              ) %>% nrow(),
            # Underlying data frame (not a survey design object)
            expected = apiclus1 %>%
              filter_all(
                .vars_predicate = any_vars(grepl("Ocean", .))
              ) %>% nrow()
          )

          # Calibration weighted design

          ## First check that the correct number of rows are retained
          expect_equal(
            # Survey design object
            object = raked_design %>%
              filter_all(
                .vars_predicate = any_vars(grepl("Ocean", .))
              ) %>% nrow(),
            # Underlying data frame (not a survey design object)
            expected = raked_design %>%
              subset(
                grepl("Ocean", name) | grepl("Ocean", sname) | grepl("Ocean", dname)
              ) %>% nrow()
          )
          ## Next check that calculation results match behavior of survey package
          expect_equal(
            object = raked_design %>%
              filter_all(
                .vars_predicate = any_vars(grepl("Ocean", .))
              ) %>%
              summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
              dplyr::pull("api_ratio"),
            expected = svyratio(
              numerator = ~ api00, denominator = ~ api99,
              design = subset(raked_design,
                              grepl("Ocean", name) | grepl("Ocean", sname) | grepl("Ocean", dname))
            )[['ratio']][1]
          )
        })

      test_that(
        "filter_all works with all_vars() predicate", {
          # Stratified design
          expect_equal(
            # Survey design object
            object = stratified_design %>%
              mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
              filter_all(
                .vars_predicate = all_vars(!is.character(.) | !is.na(.))
              ) %>% nrow(),
            # Underlying data frame (not a survey design object)
            expected = apistrat %>%
              mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
              filter_all(
                .vars_predicate = all_vars(!is.character(.) | !is.na(.))
              ) %>% nrow()
          )

          # Cluster design
          expect_equal(
            # Survey design object
            object = cluster_design %>%
              mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
              filter_all(
                .vars_predicate = all_vars(!is.character(.) | !is.na(.))
              ) %>% nrow(),
            # Underlying data frame (not a survey design object)
            expected = apiclus1 %>%
              mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
              filter_all(
                .vars_predicate = all_vars(!is.character(.) | !is.na(.))
              ) %>% nrow()
          )

          # Calibration weighted design

          ## First check that the correct number of rows are retained
          expect_equal(
            # Survey design object
            object = raked_design %>%
              mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
              filter_all(
                .vars_predicate = all_vars(!is.character(.) | !is.na(.))
              ) %>% nrow(),
            # Underlying data frame (not a survey design object)
            expected = raked_design %>%
              mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
              subset(
                !is.na(incomplete_sname)
              ) %>% nrow()
          )
          ## Next check that calculation results match behavior of survey package
          expect_equal(
            object = raked_design %>%
              mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
              filter_all(
                .vars_predicate = all_vars(!is.character(.) | !is.na(.))
              ) %>%
              summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
              dplyr::pull("api_ratio"),
            expected = svyratio(
              numerator = ~ api00, denominator = ~ api99,
              design = raked_design %>%
                mutate(incomplete_sname = ifelse(nchar(sname) > 12, NA_character_, sname)) %>%
                subset( !is.na(incomplete_sname) )
            )[['ratio']][1]
          )
        })

# filter_if ----

      is_yes_no_factor <- function(x) {
        is.factor(x) && any(c("Yes", "No") %in% levels(x))
      }

      test_that(
        "filter_if works with all_vars() predicate", {
          # Stratified design
          expect_equal(
            # Survey design object
            object = stratified_design %>%
              filter_if(is_yes_no_factor,
                        all_vars(. == "Yes")) %>%
              nrow(),
            # Underlying data frame (not a survey design object)
            expected = apistrat %>%
              filter_if(is_yes_no_factor,
                        all_vars(. == "Yes")) %>%
              nrow()
          )

          # Cluster design
          expect_equal(
            # Survey design object
            object = cluster_design %>%
              filter_if(is_yes_no_factor,
                        all_vars(. == "Yes")) %>%
              nrow(),
            # Underlying data frame (not a survey design object)
            expected = apiclus1 %>%
              filter_if(is_yes_no_factor,
                        all_vars(. == "Yes")) %>%
              nrow()
          )

          # Calibration weighted design
          ## First check that the correct number of rows are retained
          expect_equal(
            object =  raked_design %>%
              filter_if(is_yes_no_factor,
                        all_vars(. == "Yes")) %>%
              nrow(),
            expected = subset(raked_design,
                              sch.wide == "Yes" & comp.imp == "Yes" & both == "Yes" & awards == "Yes" & yr.rnd == "Yes") %>%
              nrow()
          )
          ## Next check that calculation results match behavior of survey package
          expect_equal(
            object = raked_design %>%
              filter_if(is_yes_no_factor,
                        all_vars(. == "Yes")) %>%
              summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
              dplyr::pull("api_ratio"),
            expected = svyratio(
              numerator = ~ api00, denominator = ~ api99,
              design = subset(raked_design,
                              sch.wide == "Yes" & comp.imp == "Yes" & both == "Yes" & awards == "Yes" & yr.rnd == "Yes")
            )[['ratio']][1]
          )
        })

      test_that(
        "filter_if works with any_vars() predicate", {
          # Stratified design
          expect_equal(
            # Survey design object
            object = stratified_design %>%
              filter_if(is_yes_no_factor,
                        any_vars(. == "Yes")) %>%
              nrow(),
            # Underlying data frame (not a survey design object)
            expected = apistrat %>%
              filter_if(is_yes_no_factor,
                        any_vars(. == "Yes")) %>%
              nrow()
          )

          # Cluster design
          expect_equal(
            # Survey design object
            object = cluster_design %>%
              filter_if(is_yes_no_factor,
                        any_vars(. == "Yes")) %>%
              nrow(),
            # Underlying data frame (not a survey design object)
            expected = apiclus1 %>%
              filter_if(is_yes_no_factor,
                        any_vars(. == "Yes")) %>%
              nrow()
          )

          # Calibration weighted design
          ## First check that the correct number of rows are retained
          expect_equal(
            object =  raked_design %>%
              filter_if(is_yes_no_factor,
                        any_vars(. == "Yes")) %>%
              nrow(),
            expected = subset(raked_design,
                              sch.wide == "Yes" | comp.imp == "Yes" | both == "Yes" | awards == "Yes" | yr.rnd == "Yes") %>%
              nrow()
          )
          ## Next check that calculation results match behavior of survey package
          expect_equal(
            object = raked_design %>%
              filter_if(is_yes_no_factor,
                        any_vars(. == "Yes")) %>%
              summarize(api_ratio = survey_ratio(api00, api99, vartype = NULL)) %>%
              dplyr::pull("api_ratio"),
            expected = svyratio(
              numerator = ~ api00, denominator = ~ api99,
              design = subset(raked_design,
                              sch.wide == "Yes" | comp.imp == "Yes" | both == "Yes" | awards == "Yes" | yr.rnd == "Yes")
            )[['ratio']][1]
          )
        })
