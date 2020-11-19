context('dplyr verbs behave with tbl_svy objects')
library('srvyr')
library('survey')
library('dplyr')

#set up data
data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

test_that('srvyr::pull works like dplyr::pull',{

  expect_equal(pull(dstrata, 'api00'), pull(apistrat, 'api00'))
  expect_equal(pull(dstrata, api99), pull(apistrat, api99))
  expect_equal(pull(dstrata, 1), pull(apistrat, 1))
  expect_equal(pull(dstrata, -4), pull(apistrat, -4))

})

test_that('transmute works',{
  expect_equal(
    dstrata %>% transmute(test = 1),
    dstrata %>% mutate(test = 1) %>% select(test)
  )
})

test_that('drop_na works',{
  expect_equal(
    dstrata %>% select(!flag) %>% drop_na(),
    dstrata %>% select(!flag) %>%
      filter(!is.na(acs.k3) & !is.na(acs.46) & !is.na(acs.core))
  )
  expect_equal(
    dstrata %>% drop_na(acs.core),
    dstrata %>% filter(!is.na(acs.core))
  )
  expect_equal(
    dstrata %>% drop_na(acs.core, acs.k3),
    dstrata %>% filter(!is.na(acs.core) & !is.na(acs.k3))
  )
})

test_that("summarize `.groups` argument matches dplyr behavior (0 groups case)", {
  lapply(c("keep", "drop_last", "drop"), function(group_type) {
    expect_equal(
      dstrata %>% summarize(x = unweighted(n()), .groups = !!group_type),
      as_tibble(apistrat) %>% summarize(x = n(), .groups = !!group_type)
    )
  })

  # rowwise doesn't match dplyr's behavior for 0 groups. for now assuming this will be changed in dplyr
  # https://github.com/tidyverse/dplyr/issues/5422
  expect_is(dstrata %>% summarize(x = unweighted(n()), .groups = "rowwise"), "rowwise_df")
})

test_that("summarize `.groups` argument matches dplyr behavior (1 groups case)", {
  lapply(c("keep", "drop_last", "drop"), function(group_type) {
    expect_equal(
      dstrata %>% group_by(stype) %>% summarize(x = unweighted(n()), .groups = !!group_type),
      as_tibble(apistrat) %>% group_by(stype) %>% summarize(x = n(), .groups = !!group_type),
      label = paste0("tbl_svy summarize - ", group_type),
      expected.label = paste0("tbl_df summarize - ", group_type)
    )
  })
})

test_that("summarize `.groups` argument matches dplyr behavior (3 groups case)", {
  lapply(c("keep", "drop_last", "drop"), function(group_type) {
    expect_equal(
      dstrata %>%
        group_by(stype, both, awards) %>%
        summarize(x = unweighted(n()), .groups = !!group_type),
      as_tibble(apistrat) %>%
        group_by(stype, both, awards) %>%
        summarize(x = n(), .groups = !!group_type),
      label = paste0("tbl_svy summarize - ", group_type),
      expected.label = paste0("tbl_df summarize - ", group_type)
    )
  })
})

test_that("ungrouped summarize accepts mix of 1 row & multi row results", {
  direct <- dstrata %>%
    summarize(
      w = survey_mean(api99),
      x = unweighted(quantile(api99, c(0.05, 0.5, 0.95))),
      y = survey_mean(api00),
      z = unweighted(quantile(api00, c(0.05, 0.5, 0.95)))
    )

  wide <- dstrata %>%
    summarize(
      w = survey_mean(api99),
      x1 = unweighted(quantile(api99, c(0.05))),
      x2 = unweighted(quantile(api99, c(0.5))),
      x3 = unweighted(quantile(api99, c(0.95))),
      y = survey_mean(api00),
      z1 = unweighted(quantile(api00, c(0.05))),
      z2 = unweighted(quantile(api00, c(0.5))),
      z3 = unweighted(quantile(api00, c(0.95)))
    )

  round_about <- bind_cols(
    wide %>% select(starts_with("w")) %>% slice(rep(1, 3)),
    bind_rows(
      wide %>% select(x = x1),
      wide %>% select(x = x2),
      wide %>% select(x = x3)
    ),
    wide %>% select(starts_with("y")) %>% slice(rep(1, 3)),
    bind_rows(
      wide %>% select(z = z1),
      wide %>% select(z = z2),
      wide %>% select(z = z3)
    )
  )

  expect_equal(direct, round_about)
})

test_that("ungrouped summarize errors when multi row results of different number of rows", {
  expect_error(
    direct <- dstrata %>%
      summarize(
        w = survey_mean(api99),
        x = unweighted(quantile(api99, c(0.05, 0.5, 0.95))),
        y = survey_mean(api00),
        z = unweighted(quantile(api00, c(0.05, 0.5, 0.75, 0.95)))
      ),
    "summarise results for argument `z` must be size 1 or 3 but it is 4"
  )
})

test_that("grouped summarize accepts mix of 1 row & multi row results", {
  direct <- dstrata %>%
    group_by(both) %>%
    summarize(
      w = survey_mean(api99),
      x = unweighted(quantile(api99, c(0.05, 0.5, 0.95))),
      y = survey_mean(api00),
      z = unweighted(quantile(api00, c(0.05, 0.5, 0.95)))
    )

  wide <- dstrata %>%
    group_by(both) %>%
    summarize(
      w = survey_mean(api99),
      x1 = unweighted(quantile(api99, c(0.05))),
      x2 = unweighted(quantile(api99, c(0.5))),
      x3 = unweighted(quantile(api99, c(0.95))),
      y = survey_mean(api00),
      z1 = unweighted(quantile(api00, c(0.05))),
      z2 = unweighted(quantile(api00, c(0.5))),
      z3 = unweighted(quantile(api00, c(0.95)))
    )

  round_about <- bind_cols(
    wide %>% select(both, starts_with("w")) %>% slice(rep(1:2, each = 3)),
    bind_rows(
      wide %>% ungroup() %>% select(x = x1) %>% slice(1),
      wide %>% ungroup() %>% select(x = x2) %>% slice(1),
      wide %>% ungroup() %>% select(x = x3) %>% slice(1),
      wide %>% ungroup() %>% select(x = x1) %>% slice(2),
      wide %>% ungroup() %>% select(x = x2) %>% slice(2),
      wide %>% ungroup() %>% select(x = x3) %>% slice(2)
    ),
    wide %>% ungroup() %>% select(starts_with("y")) %>% slice(rep(1:2, each = 3)),
    bind_rows(
      wide %>% ungroup() %>% select(z = z1) %>% slice(1),
      wide %>% ungroup() %>% select(z = z2) %>% slice(1),
      wide %>% ungroup() %>% select(z = z3) %>% slice(1),
      wide %>% ungroup() %>% select(z = z1) %>% slice(2),
      wide %>% ungroup() %>% select(z = z2) %>% slice(2),
      wide %>% ungroup() %>% select(z = z3) %>% slice(2)
    )
  ) %>%
    group_by(both)

  expect_equal(direct, round_about)
})

test_that("grouped summarize errors when multi row results of different number of rows", {
  expect_error(
    dstrata %>%
      group_by(both) %>%
      summarize(
        w = survey_mean(api99),
        x = unweighted(quantile(api99, c(0.05, 0.5, 0.95))),
        y = survey_mean(api00),
        z = unweighted(quantile(api00, c(0.05, 0.5, 0.75, 0.95)))
      ),
    "summarise results for argument `z` must be size 1 or 3 but it is 4 for group: both = 1"
  )
})
