expect_df_equal <- function(x, y) {
  row.names(x) <- NULL
  row.names(y) <- NULL

  expect_equivalent(x, y)
}

expect_equal_or_warn <- function(...) {
  tryCatch(expect_equal(...),
           error = function(e) warning(e))
}

expect_identical_or_warn <- function(...) {
  tryCatch(expect_identical(...),
           error = function(e) warning(e))
}

expect_df_equal_or_warn <- function(...) {
  tryCatch(expect_df_equal(...),
           error = function(e) warning(e))
}
