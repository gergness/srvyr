#' @export
semi_join.tbl_svy <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  ...,
  na_matches = c("na", "never")
) {

  if (inherits(y, "tbl_svy")) {
    y <- y$variables
  }

  x <- mutate(x, `___row_number` = dplyr::row_number())

  filtered_vars <- semi_join(x = x$variables,
                             y = y,
                             by = by,
                             copy = copy,
                             na_matches = na_matches,
                             ...)

  x <- filter(x, .data[["___row_number"]] %in% filtered_vars[['___row_number']])
  if ("___row_number" %in% tbl_vars(x)) {
    x <- select(x, -.data[["___row_number"]])
  }

  x

}

#' @export
anti_join.tbl_svy <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  ...,
  na_matches = c("na", "never")
) {

  if (inherits(y, "tbl_svy")) {
    y <- y$variables
  }

  x <- mutate(x, `___row_number` = dplyr::row_number())

  filtered_vars <- anti_join(x = x$variables,
                             y = y,
                             by = by,
                             copy = copy,
                             na_matches = na_matches,
                             ...)

  x <- filter(x, .data[["___row_number"]] %in% filtered_vars[["___row_number"]])
  if ("___row_number" %in% tbl_vars(x)) {
    x <- select(x, -.data[["___row_number"]])
  }

  x

}

# Import + export generics from dplyr and tidyr
#' Filtering joins from dplyr
#'
#' These are data manipulation functions designed to work on a \code{tbl_svy} object
#' and another data frame or \code{tbl_svy} object.
#'
#' \code{semi_join} and \code{anti_join} filter certain observations from a \code{tbl_svy}
#' depending on the presence or absence of matches in another table.
#' See \code{\link[dplyr]{filter-joins}} for more details.
#'
#' Mutating joins (\code{full_join}, \code{left_join}, etc.) are not implemented
#' for any \code{tbl_svy} objects. These data manipulations
#' may require modifications to the survey variable specifications and so
#' cannot be done automatically. Instead, use dplyr to perform them while the
#' data is still stored in data.frames.
#' @name dplyr_filter_joins
NULL

#' @name semi_join
#' @export
#' @importFrom dplyr semi_join
#' @rdname dplyr_filter_joins
NULL

#' @name anti_join
#' @export
#' @importFrom dplyr anti_join
#' @rdname dplyr_filter_joins
NULL
