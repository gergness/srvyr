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

test_that("mutate can handle survey summaries", {
  summarize_version <- dstrata %>%
    group_by(awards) %>%
    summarise(x = survey_mean(api99))

  mutate_version <- dstrata %>%
    group_by(awards) %>%
    mutate(x = survey_mean(api99)) %>%
    as_tibble() %>%
    select(awards, x, x_se) %>%
    distinct()

  expect_equal(mutate_version, summarize_version)
})

test_that("mutate can handle .by", {
  explicit_group_by <- dstrata %>%
    group_by(awards) %>%
    mutate(x = survey_mean(api99))

  .by_arg <- dstrata %>%
    mutate(x = survey_mean(api99), .by = awards)

  expect_equal(explicit_group_by, .by_arg)
})

test_that("mutate can handle multiple .by", {
  explicit_group_by <- dstrata %>%
    group_by(name, awards) %>%
    mutate(x = survey_mean(api99))

  .by_arg <- dstrata %>%
    mutate(x = survey_mean(api99), .by = c(name, awards))

  expect_equal(explicit_group_by, .by_arg)
})

test_that('transmute works',{
  expect_equal(
    dstrata %>% transmute(test = 1),
    dstrata %>% mutate(test = 1) %>% select(test)
  )
})

test_that('rename works', {
  new_names <- dstrata %>% `[[`("variables") %>% names
  new_names <- ifelse(new_names=="target", "tgt", new_names)
  expect_equal(
    dstrata %>% rename(tgt = target) %>% `[[`("variables") %>% names,
    new_names
  )
})

test_that('rename_with works without the .cols= argument', {
  new_names <- dstrata %>% tbl_vars() %>% paste0(".x")
  expect_equal(
    dstrata %>% rename_with(~paste0(., ".x")) %>% tbl_vars() %>% as.character(),
    new_names
  )

  expect_equal(
    dstrata %>% rename_with(\(x) paste0(x, ".x")) %>% tbl_vars() %>% as.character(),
    new_names
  )
})

test_that('rename_with works with the .cols= argument', {
  new_names <- dstrata %>%
    tbl_vars %>%
    {ifelse(endsWith(., "m"), paste0(., ".x"), .) }
  expect_equal(
    dstrata %>% rename_with(~paste0(., ".x"), ends_with("m")) %>% tbl_vars() %>% as.character(),
    new_names
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
  lapply(c("keep", "drop_last", "drop", "rowwise"), function(group_type) {
    expect_equal(
      dstrata %>% summarize(x = unweighted(n()), .groups = !!group_type),
      as_tibble(apistrat) %>% summarize(x = n(), .groups = !!group_type)
    )
  })
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

test_that("summarize can handle .by argument", {
  expect_equal(
      dstrata %>%
        group_by(awards) %>%
        summarise(api99 = survey_mean(api99)),
      dstrata %>%
        summarise(api99 = survey_mean(api99), .by = awards),
  )
})

test_that("ungrouped reframe accepts mix of 1 row & multi row results", {
  direct <- dstrata %>%
    reframe(
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

test_that("grouped reframe accepts mix of 1 row & multi row results", {
  direct <- dstrata %>%
    group_by(both) %>%
    reframe(
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
  )

  expect_equal(direct, round_about)
})

test_that("group_trim works", {
  before_trim <- dstrata %>%
    group_by(both, .drop = FALSE) %>%
    filter(both == "No")

  expect_equal(group_keys(before_trim)[[1]], factor(c("No", "Yes"), c("No", "Yes")))

  after_trim <- before_trim %>% group_trim()
  expect_equal(group_keys(after_trim)[[1]], factor(c("No"), c("No")))
})

test_that("summarize unpacks inside of across", {
  unnamed <- dstrata %>% summarize(across(
    matches("^api[0-9]+$"),
    list(mn = ~survey_mean(., vartype = "var"), prop_over_700 = ~survey_mean(. > 700))
  ))
  named_df <- dstrata %>% summarize(z = across(
    matches("^api[0-9]+$"),
    list(mn = ~survey_mean(., vartype = "var"), prop_over_700 = ~survey_mean(. > 700))
  ))

  expect_equal(names(unnamed), c(
    "api00_mn", "api00_mn_var", "api00_prop_over_700", "api00_prop_over_700_se",
    "api99_mn", "api99_mn_var", "api99_prop_over_700", "api99_prop_over_700_se"
  ))
  expect_equal(names(named_df), "z")

  expect_equal(unnamed, named_df$z)
})

test_that("summarize unpacks after on-the-fly expression", {
  actual <- dstrata %>%
    summarize(x = 100 * survey_mean(api99 > 700))
  expected <- dstrata %>%
    summarize(x = survey_mean(api99 > 700)) %>%
    mutate(across(everything(), .fns = ~. * 100))

  expect_equal(actual, expected)
})
