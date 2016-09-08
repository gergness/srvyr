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
