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
  x <- arrange_(x, "`___SRVYR_ORDER`")
  x <- select_(x, "-`___SRVYR_ORDER`")
  dplyr::collect(x)
}
