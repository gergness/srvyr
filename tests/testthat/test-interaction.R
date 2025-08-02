library(dplyr)
interact_data <- data.frame(
  int_col = c(1, 1:3, 1),
  char_col = letters[c(1, 3:1, 1)],
  fct_col = factor(letters[1:5]),
  dbl_col = c(1.1, 2.2, 3.3, 4.4, 5.5),
  ordered_col = ordered(letters[6:10]),
  stringsAsFactors = FALSE
)

data(api, package = "survey")
dstrata <- apistrat %>%
   as_survey_design(strata = stype, weights = pw)

test_that("Can make an interaction", {
  x <- interact_data |>
    dplyr::mutate(x = interact(int_col, char_col)) |>
    pull(x)

  expect_s3_class(x, "srvyr_interaction")
  expect_equal(vctrs::vec_data(x), c(1, 2, 3, 4, 1))
  expect_equal(
    attr(x, "crosswalk"),
    tibble::tibble(
      `int_col` = c(1, 1, 2, 3),
      `char_col` = c("a", "c", "b", "a"),
      `___srvyr_cw_id` = 1:4
    )
  )
})

test_that("Can unmake an interaction column", {
  x <- interact_data |>
    dplyr::mutate(x = interact(int_col = int_col, char_col = char_col)) |>
    pull(x)

  orig <- uninteract(x)
  expect_equal(orig, tibble::tibble(interact_data[c("int_col", "char_col")]))
})

test_that("Can unmake all interactions in a data.frame", {
  x <- interact_data |>
    dplyr::transmute(
      a = interact(int_col = int_col, char_col = char_col),
      b = interact(fct_col = fct_col, dbl_col = dbl_col)
    )

  orig <- uninteract(x)
  expect_equal(
    orig,
    tibble::tibble(interact_data[c("int_col", "char_col", "fct_col", "dbl_col")])
  )
})

test_that("Can make interactions in a srvyr pipeline", {
  srvyr_interact <- dstrata %>%
    group_by(x = interact(stype, awards)) %>%
    pull(x)

  expect_s3_class(srvyr_interact, "srvyr_interaction")
})

test_that("A wide variety of column types are preserved in an interaction roundtrip", {
  x <- interact_data |>
    dplyr::mutate(x = interact(dplyr::across(dplyr::everything()))) |>
    dplyr::pull(x)

  orig <- uninteract(x)
  expect_equal(orig, tibble::tibble(interact_data))
})

test_that("Can make new columns in interact", {
  x <- interact_data %>%
    dplyr::mutate(x = interact(int_col = int_col + 10, char_col = char_col)) |>
    dplyr::pull(x)

  orig <- uninteract(x)
  expected <- interact_data %>% mutate(int_col = int_col + 10) %>% select(int_col, char_col) %>% tibble::tibble()
  expect_equal(orig, expected)
})

test_that("Can combine interactions with identical crosswalks", {
  x <- interact_data |>
    dplyr::transmute(
    a = interact(int_col = interact_data$int_col, char_col = interact_data$char_col)
  )

  combined <- dplyr::bind_rows(x, x)
  expected <- dplyr::bind_rows(interact_data, interact_data) %>%
    select(int_col, char_col) %>%
    tibble::tibble()

  expect_equal(uninteract(combined$a), expected)
})

test_that("Can combine interactions with crosswalks that are superset-able", {
  x <- interact_data |>
    dplyr::slice(1:2) |>
    dplyr::transmute(
      a = interact(int_col = int_col, char_col = char_col)
    )
  y <- interact_data |>
    dplyr::transmute(
      a = interact(int_col = int_col, char_col = char_col)
    )

  combined <- dplyr::bind_rows(x, y)
  expected <- dplyr::bind_rows(interact_data[1:2, ], interact_data) %>%
    select(int_col, char_col) %>%
    tibble::tibble()

  expect_equal(uninteract(combined$a), expected)

  # Also reverse x & y
  combined <- dplyr::bind_rows(y, x)
  expected <- dplyr::bind_rows(interact_data, interact_data[1:2, ]) %>%
    select(int_col, char_col) %>%
    tibble::tibble()

  expect_equal(uninteract(combined$a), expected)
})

test_that("Get good error when crosswalks have different column names", {
  x <- interact_data |>
    dplyr::transmute(
      a = interact(int_col = int_col, char_col = char_col)
    )
  y <- interact_data |>
    dplyr::transmute(
      a = interact(int_col = int_col, fct_col = fct_col)
    )

  expect_error(
    dplyr::bind_rows(x, y),
    "Cannot cast .+ because interacted column names don't match"
  )
})

test_that("Get good error when crosswalks have different column types", {
  x <- interact_data |>
    dplyr::transmute(
      a = interact(int_col = int_col, char_col = char_col)
    )

  y <- interact_data |>
    dplyr::transmute(
      a = interact(int_col = int_col, char_col = dbl_col)
    )

  expect_error(
    dplyr::bind_rows(x, y),
    "Crosswalk columns for .+ are incompatible"
  )
})

test_that("Get good error when casting incompatible interaction croswalks", {
  x <- interact_data |>
    dplyr::slice(1:3) |>
    dplyr::mutate(x = interact(int_col = int_col, char_col = char_col)) |>
    pull(x)
  y <- interact_data |>
    dplyr::mutate(x = interact(int_col = int_col, char_col = char_col)) |>
    pull(x)

  expect_error(
    vec_cast(y, x, x_arg = "y", to_arg = "x"),
    "Cannot cast `y` to `x` because not all of crosswalk values in y are in x's crosswalk"
  )
})

test_that("Interaction gets automatically undone in a summarize", {
  actual <- dstrata %>%
    group_by(interact(stype, awards)) %>%
    summarize(x = survey_mean())

  alternate <- dstrata %>%
    group_by(a = interaction(stype, awards)) %>%
    summarize(stype = stype[1], awards = awards[1], x = survey_mean()) %>%
    select(-a) %>%
    arrange(stype, awards)

  expect_equal(actual, alternate)


  # Groups are recreated too
  actual <- dstrata %>%
    group_by(
      interact(yr.rnd, meals_hi = meals > median(meals)),
      interact(stype, awards)
    ) %>%
    summarize(x = survey_mean())

  alternate <- dstrata %>%
    mutate(meals_hi = meals > median(meals)) %>%
    group_by(b = interaction(yr.rnd, meals_hi), a = interaction(stype, awards)) %>%
    summarize(
      yr.rnd = yr.rnd[1],
      meals_hi = meals_hi[1],
      stype = stype[1],
      awards = awards[1],
      x = survey_mean()
    ) %>%
    ungroup() %>%
    select(-a, -b) %>%
    arrange(yr.rnd, meals_hi, stype, awards) %>%
    group_by(yr.rnd, meals_hi)


  expect_equal(actual, alternate)
})

test_that("Duplicate names are okay when unpacking in summarize", {
  expect_warning(
    actual <- dstrata %>%
      group_by(interact(stype, awards), stype) %>%
      summarize(x = survey_mean()),
    "Duplicate names found \\(stype\\) .+"
  )

  alternate <- dstrata %>%
    group_by(interact(stype, awards), stype2 = stype) %>%
    summarize(x = survey_mean()) %>%
    select(-stype2)

  expect_equal(actual, alternate)
})

test_that("Can recast interaction terms", {
  expect_equal(
    interact_data %>%
      transmute(interact(int_col, char_col) %>% recast_interact(int_col)) %>%
      uninteract(),
    interact_data %>%
      transmute(interact(int_col)) %>%
      uninteract()
  )

  expect_equal(
    interact_data %>%
      transmute(interact(int_col, char_col, fct_col) %>% recast_interact(int_col, fct_col)) %>%
      uninteract(),
    interact_data %>%
      transmute(interact(int_col, fct_col)) %>%
      uninteract()
  )
})

test_that("interact works programmatically", {
  myfun <- function(data, ...) {
    data |>
      dplyr::group_by(srvyr::interact(...)) |>
      dplyr::summarise(prop = srvyr::survey_prop())
  }

  non_programmatic <- mtcars |>
    srvyr::as_survey() |>
    dplyr::group_by(srvyr::interact(am, gear)) |>
    dplyr::summarise(prop = srvyr::survey_prop())

  programmatic <- mtcars |>
    srvyr::as_survey() |>
    myfun(am, gear)

  expect_equal(programmatic, non_programmatic)
})
