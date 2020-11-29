data(api, package = "survey")
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)


test_that("group_map works with groups", {
  group_map_way <- dstrata %>%
    group_by(both) %>%
    group_map(function(svy, group) {
      model <- survey::svyglm(api00~api99 + stype, svy)
      coef(model)
    })

  old_way <- lapply(
    c("No", "Yes"),
    function(x) {
      dstrata %>%
      filter(both == x) %>%
        survey::svyglm(api00~api99 + stype, .) %>%
        coef()
    }
  )

  expect_equal(group_map_way, old_way)
})

test_that("group_map works without groups", {
  group_map_way <- dstrata %>%
    group_map(function(svy, group) {
      model <- survey::svyglm(api00~api99 + stype, svy)
      coef(model)
    })

  old_way <- list(
    dstrata %>%
      survey::svyglm(api00~api99 + stype, .) %>%
      coef()
  )

  expect_equal(group_map_way, old_way)
})

test_that("group_map_dfr works", {
  group_map_way <- dstrata %>%
    group_by(both) %>%
    group_map_dfr(function(svy, group) {
      model <- survey::svyglm(api00~api99 + stype, svy)
      coef(model) %>%
        data.frame()
    })

  old_way <- lapply(
    c("No", "Yes"),
    function(x) {
      dstrata %>%
        filter(both == x) %>%
        survey::svyglm(api00~api99 + stype, .) %>%
        coef() %>%
        unname() %>%
        dplyr::tibble() %>%
        mutate(both = factor(x, levels(dstrata$variables$both)))
    }
  ) %>% dplyr::bind_rows() %>%
    select(both, everything())


  expect_equal(group_map_way, old_way)
})
