context("srvyr_select_vars working as expected.")

expect_rhs_equal_to <- function(x, y) {
  expect_equal(rlang::f_rhs(x), rlang::f_rhs(y))
}


test_that(
  "ssv works with a single column name", {
    single_col <- srvyr_select_vars(rlang::quo(cyl), mtcars)
    expect_rhs_equal_to(single_col, ~cyl)
  }
)

test_that(
  "ssv works with select style functions", {
    single_col <- srvyr_select_vars(rlang::quo(dplyr::one_of("cyl")), mtcars)
    expect_rhs_equal_to(single_col, ~cyl)

    multi_vars <- srvyr_select_vars(rlang::quo(c(dplyr::one_of("cyl"), dplyr::ends_with("pg"))), mtcars)
    expect_rhs_equal_to(multi_vars, ~cyl + mpg)
  }
)

test_that(
  "ssv: basic object retrieval from environment works", {
    x <- "cyl"
    single_col <- srvyr_select_vars(rlang::quo(dplyr::one_of(x)), mtcars)
    expect_rhs_equal_to(single_col, ~cyl)

    multi_vars <- srvyr_select_vars(rlang::quo(c(dplyr::one_of(x), dplyr::ends_with("pg"))), mtcars)
    expect_rhs_equal_to(multi_vars, ~cyl + mpg)
  }
)

test_that(
  "ssv: more complicated object retrieval from environment works", {
    test_f <- function() {
      x <- "cyl"
      srvyr_select_vars(rlang::quo(dplyr::one_of(x)), mtcars)
    }

    x <- "blah"
    single_col <- test_f()
    expect_rhs_equal_to(single_col, ~cyl)
  }
)

test_that(
  "ssv returns ~0 when check_ids = TRUE", {
    single_col <- srvyr_select_vars(rlang::quo(0), mtcars, check_ids = TRUE)
    expect_rhs_equal_to(single_col, ~0)
  }
)

test_that(
  "ssv can give ~0 programmatically when check_ids = TRUE", {
    x <- 0
    single_col <- srvyr_select_vars(rlang::quo(x), mtcars, check_ids = TRUE)
    expect_rhs_equal_to(single_col, ~0)
  }
)

test_that(
  "ssv defaults to column if it is given in isolation when check_ids = TRUE", {
    cyl <- 0
    single_col <- srvyr_select_vars(rlang::quo(cyl), mtcars, check_ids = TRUE)
    expect_rhs_equal_to(single_col, ~cyl)
  }
)

test_that(
  "ssv works with c()", {
    cyl <- 0
    multi_var <- srvyr_select_vars(rlang::quo(c(cyl, mpg)), mtcars, check_ids = TRUE)
    expect_rhs_equal_to(multi_var, ~cyl + mpg)
  }
)
