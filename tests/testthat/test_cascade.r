context("Cascade works.")

suppressPackageStartupMessages(library(survey))
data(api)
source("utilities.R")

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
          expect_df_equal(cascade_results, summarize_results))


# .fill works
test_that(".fill works & respects factors",
          expect_equal(dstrata_srvyr %>%
                         group_by(stype) %>%
                         cascade(api99_mn = survey_mean(api99), .fill = "AAA") %>%
                         .$stype,
                       factor(c("E", "H", "M", "AAA"), levels = c("E", "H", "M", "AAA"))))


test_that("cascade works with non-standard names (#132)", {
  actual <- dstrata_srvyr %>%
    group_by(`1234` = stype) %>%
    cascade(x = survey_mean())

  expect_equal(names(actual)[1], "1234")
})


test_that("cascade can form groupings from interact column", {
  # regular 2 var
  expect_equal(
    dstrata_srvyr %>% group_by(stype, awards) %>% cascade_groupings(),
    list(
      list(rlang::sym("stype"), rlang::sym("awards")),
      list(rlang::sym("stype")),
      NULL
    )
  )


  # 2 var interaction
  expect_equal(
    dstrata_srvyr %>% group_by(interact(stype, awards)) %>% cascade_groupings(),
    list(
      list(rlang::sym("interact(stype, awards)")),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("stype")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("awards")))),
      NULL
    )
  )

  # 3 var interaction
  expect_equal(
    dstrata_srvyr %>% group_by(interact(stype, awards, yr.rnd)) %>% cascade_groupings(),
    list(
      list(rlang::sym("interact(stype, awards, yr.rnd)")),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards, yr.rnd)"), !!rlang::sym("stype"), !!rlang::sym("awards")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards, yr.rnd)"), !!rlang::sym("stype"), !!rlang::sym("yr.rnd")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards, yr.rnd)"), !!rlang::sym("awards"), !!rlang::sym("yr.rnd")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards, yr.rnd)"), !!rlang::sym("stype")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards, yr.rnd)"), !!rlang::sym("awards")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards, yr.rnd)"), !!rlang::sym("yr.rnd")))),
      NULL
    )
  )

  # mixed interact before regular
  expect_equal(
    dstrata_srvyr %>% group_by(stype, interact(awards, yr.rnd)) %>% cascade_groupings(),
    list(
      list(rlang::sym("stype"), rlang::sym("interact(awards, yr.rnd)")),
      list(rlang::sym("stype"), rlang::expr(recast_interact(!!rlang::sym("interact(awards, yr.rnd)"), !!rlang::sym("awards")))),
      list(rlang::sym("stype"), rlang::expr(recast_interact(!!rlang::sym("interact(awards, yr.rnd)"), !!rlang::sym("yr.rnd")))),
      list(rlang::sym("stype")),
      NULL
    )
  )

  # mixed interact after regular
  expect_equal(
    dstrata_srvyr %>% group_by(interact(stype, awards), yr.rnd) %>% cascade_groupings(),
    list(
      list(rlang::sym("interact(stype, awards)"), rlang::sym("yr.rnd")),
      list(rlang::sym("interact(stype, awards)")),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("stype")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("awards")))),
      NULL
    )
  )
})
