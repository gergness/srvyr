context("Cascade works.")

suppressPackageStartupMessages(library(survey))
data(api)


dstrata_srvyr <- apistrat %>%
  as_survey(strata = stype, weights = pw)

# 1 group
cascade_results <- dstrata_srvyr %>%
  group_by(stype) %>%
  cascade(api99_mn = survey_mean(api99))

summarize_results <- dplyr::bind_rows(
  dstrata_srvyr %>%
    group_by(stype) %>%
    summarize(api99_mn = survey_mean(api99)),
  dstrata_srvyr %>%
    summarize(api99_mn = survey_mean(api99))
)

test_that("cascade works for 1 group",
          expect_equal(cascade_results, summarize_results))

# 2 groups
cascade_results <- dstrata_srvyr %>%
  group_by(stype, awards) %>%
  cascade(api99_mn = survey_mean(api99))

summarize_results <- dplyr::bind_rows(
  dstrata_srvyr %>%
    group_by(stype, awards) %>%
    summarize(api99_mn = survey_mean(api99)),
  dstrata_srvyr %>%
    group_by(stype) %>%
    summarize(api99_mn = survey_mean(api99)),
  dstrata_srvyr %>%
    summarize(api99_mn = survey_mean(api99))
) %>% dplyr::arrange(stype, awards)

test_that("cascade works for 1 group",
          expect_equal(cascade_results, summarize_results))


# .fill works
test_that(".fill works & respects factors",
          expect_equal(dstrata_srvyr %>%
                         group_by(stype) %>%
                         cascade(api99_mn = survey_mean(api99), .fill = "AAA") %>%
                         .$stype,
                       factor(c("E", "H", "M", "AAA"), levels = c("E", "H", "M", "AAA"))))
