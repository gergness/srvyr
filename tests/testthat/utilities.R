expect_df_equal <- function(x, y) {
  row.names(x) <- NULL
  row.names(y) <- NULL

  expect_equal(x, y)
}
