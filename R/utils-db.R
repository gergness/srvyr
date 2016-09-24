# Utility functions created when implementing db functions.

# Get the uid from a svy_tbl object
uid <- function(svy) {
  svy$uid
}

# Set the uid of a svy_tbl object
`uid<-` <- function(svy, value) {
  svy$uid <- value
  svy
}

get_uid_names <- function(num_vars) {
  paste0("SRVYR_ORDER", seq_len(num_vars))
}

uid_rename <- function(df, uid_vars, uid_names) {
  rename_text <- uid_vars
  names(rename_text) <- uid_names
  mutate_(df, .dots = rename_text)
}

# Collect after ordering on uid
ordered_collect <- function(x) {
  order_vars <-  attr(x, "order_var")
  x <- dplyr::arrange_(x, .dots = order_vars)
  x <- dplyr::collect(x, n = Inf)
  x <- dplyr::select(x, -dplyr::one_of(order_vars))
  x
}
