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
    dstrata_srvyr %>% group_by(stype, awards) %>% determine_cascade_groupings(),
    list(
      list(rlang::sym("stype"), rlang::sym("awards")),
      list(rlang::sym("stype")),
      NULL
    )
  )


  # 2 var interaction
  expect_equal(
    dstrata_srvyr %>% group_by(interact(stype, awards)) %>% determine_cascade_groupings(),
    list(
      list(rlang::sym("interact(stype, awards)")),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("stype")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("awards")))),
      NULL
    )
  )

  # 3 var interaction
  expect_equal(
    dstrata_srvyr %>% group_by(interact(stype, awards, yr.rnd)) %>% determine_cascade_groupings(),
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
    dstrata_srvyr %>% group_by(stype, interact(awards, yr.rnd)) %>% determine_cascade_groupings(),
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
    dstrata_srvyr %>% group_by(interact(stype, awards), yr.rnd) %>% determine_cascade_groupings(),
    list(
      list(rlang::sym("interact(stype, awards)"), rlang::sym("yr.rnd")),
      list(rlang::sym("interact(stype, awards)")),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("stype")))),
      list(rlang::expr(recast_interact(!!rlang::sym("interact(stype, awards)"), !!rlang::sym("awards")))),
      NULL
    )
  )
})


test_that("cascade accepts groupings", {
  expect_equal(
    dstrata_srvyr %>%
      cascade(
        x = survey_total(),
        .groupings = list(
          rlang::quos(stype, awards), rlang::quos(stype),
          rlang::quos(NULL)
        )
      ),
    dstrata_srvyr %>%
      group_by(stype, awards) %>%
      cascade(x = survey_total())
  )
})

test_that("cascade can fill parts - non-interacted factor and string default fill", {
  actual <- dstrata_srvyr %>%
    group_by(stype, awards = as.character(awards)) %>%
    cascade(x = survey_mean())

  expect_true(is.factor(actual$stype))
  expect_equal(levels(actual$stype), levels(dstrata_srvyr$variables$stype))
  expect_equal(
    sort(as.character(actual$stype), na.last = TRUE),
    c(rep("E", 3), rep("H", 3), rep("M", 3), NA)
  )

  expect_true(is.character(actual$awards))
  expect_equal(
    sort(as.character(actual$awards), na.last = TRUE),
    c(rep("No", 3), rep("Yes", 3), rep(NA, 4))
  )
})


test_that("cascade can fill parts - non-interacted factor and string with default fill", {
  actual <- dstrata_srvyr %>%
    group_by(interact(stype, awards = as.character(awards))) %>%
    cascade(x = survey_mean())

  expect_true(is.factor(actual$stype))
  expect_equal(levels(actual$stype), levels(dstrata_srvyr$variables$stype))
  expect_equal(
    sort(as.character(actual$stype), na.last = TRUE),
    c(rep("E", 3), rep("H", 3), rep("M", 3), rep(NA, 3))
  )

  expect_true(is.character(actual$awards))
  expect_equal(
    sort(as.character(actual$awards), na.last = TRUE),
    c(rep("No", 4), rep("Yes", 4), rep(NA, 4))
  )
})


test_that("cascade can fill parts - non-interacted factor and string with fill", {
  actual <- dstrata_srvyr %>%
    group_by(interact(stype, awards = as.character(awards))) %>%
    cascade(x = survey_mean(), .fill = "Total")

  expect_true(is.factor(actual$stype))
  expect_equal(levels(actual$stype), c(levels(dstrata_srvyr$variables$stype), "Total"))
  expect_equal(
    sort(as.character(actual$stype), na.last = TRUE),
    c(rep("E", 3), rep("H", 3), rep("M", 3), rep("Total", 3))
  )

  expect_true(is.character(actual$awards))
  expect_equal(
    sort(as.character(actual$awards), na.last = TRUE),
    c(rep("No", 4), rep("Total", 4), rep("Yes", 4))
  )
})


test_that("cascade can fill parts - ordered with fill", {
  actual <- dstrata_srvyr %>%
    group_by(awards = ordered(awards, c("Yes", "No"))) %>%
    cascade(x = survey_mean(), .fill = "Total")

  expect_true(is.ordered(actual$awards))
  expect_equal(levels(actual$awards), c("Yes", "No", "Total"))
  expect_equal(
    sort(as.character(actual$awards), na.last = TRUE),
    c(rep("No", 1), rep("Total", 1), rep("Yes", 1))
  )
})


test_that("cascade can fill parts - integer with fill", {
  actual <- dstrata_srvyr %>%
    group_by(awards = as.integer(awards)) %>%
    cascade(x = survey_mean(), .fill = 100L)

  expect_true(is.integer(actual$awards))
  expect_equal(
    sort(actual$awards, na.last = TRUE),
    c(rep(1, 1), rep(2, 1), rep(100, 1))
  )
})

