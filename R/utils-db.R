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

# Collect after ordering on uid
ordered_collect <- function(x) {
  x <- dplyr::arrange_(x, "SRVYR_ORDER")
  x <- dplyr::collect(x)
  x <- dplyr::select_(x, "-SRVYR_ORDER")
  x
}

# Current belief -> a final arrange could be created
# that behaves similarly to arrange_, but is translated slightly different
# so that it is valid MonetDB
# final_arrange_ <- function(x, dots) {
#
# }
#
# sql_build.op_final_arrange <- function(op, con, ...) {
#
# }
