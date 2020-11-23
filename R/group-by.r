#' @export
group_by.tbl_svy <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  .data$variables <- group_by(.data$variables, ..., .add = .add, .drop = .drop)
  class(.data) <- c("grouped_svy", class(.data))
  .data
}

#' @export
group_by_.tbl_svy <- function(.data, ..., .dots, add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!!dots, add = add, .drop = .drop)
}

#' @export
ungroup.tbl_svy <- function(x, ...) {
  x
}

#' @export
ungroup.grouped_svy <- function(x, ...) {
  x$variables <- ungroup(x$variables)
  class(x) <- setdiff(class(x), "grouped_svy")
  x
}

#' @export
groups.tbl_svy <- function(x) {
  groups(x$variables)
}

#' @export
group_vars.tbl_svy <- function(x) {
  group_vars(x$variables)
}

#' @export
group_data.tbl_svy <- function(.data) {
  group_data(.data$variables)
}

#' @export
group_keys.tbl_svy <- function(.tbl, ...) {
  group_keys(.tbl$variables)
}

#' @export
group_indices.tbl_svy <- function(.data, ...) {
  group_indices(.data$variables)
}

#' @export
group_size.tbl_svy <- function(x) {
  group_size(x$variables)
}

#' @export
n_groups.tbl_svy <- function(x) {
  n_groups(x$variables)
}

#' Group a (survey) dataset by one or more variables.
#'
#' Most data operations are useful when done on groups defined by variables
#' in the dataset. The \code{group_by} function takes an existing table (or
#' svy_table) and converts it to a grouped version, where operations are
#' performed "by group".
#'
#' See \code{\link[dplyr]{group_by}} for more information about grouping
#' regular data tables.
#'
#' On \code{tbl_svy} objects, \code{group_by} sets up the object for
#' operations similar to those allowed in \code{\link[survey]{svyby}}.
#' @param .data A tbl
#' @param ... variables to group by. All tbls accept variable names, some will
#' also accept functions of variables. Duplicated groups will be silently dropped.
#' @param add By default, when \code{add = FALSE}, \code{group_by} will override
#' existing groups. To instead add to the existing groups, use \code{add = TRUE}
#' @param .dots Used to work around non-standard evaluation. See
#' \code{vignette("nse", package = "dplyr")} for details.
#' @examples
#' # Examples of svy_tbl group_by
#' library(survey)
#' data(api)
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw) %>%
#'   group_by(stype)
#'
#'   dstrata %>%
#'     summarise(api_diff = survey_mean(api00 - api99))
#' @seealso \code{\link[dplyr]{group_by}} for information about group_by on normal data tables.
#' @name group_by
#' @export
#' @importFrom dplyr group_by
NULL

#' @rdname group_by
#' @name group_by_
#' @export
#' @importFrom dplyr group_by_
NULL

#' Get/set the grouping variables for tbl.
#'
#' These functions do not perform non-standard evaluation, and
#' so are useful when programming against \code{tbl} objects.
#' \code{ungroup} is a convenient inline way of removing existing
#' grouping.
#'
#' @param x data \code{\link[dplyr]{tbl_df}} or \code{\link{tbl_svy}} object.
#' @seealso \code{\link[dplyr]{groups}} for information.

#' @name groups
#' @export
#' @importFrom dplyr groups
NULL


#' @rdname groups
#' @name ungroup
#' @export
#' @importFrom dplyr ungroup
NULL

#' @rdname groups
#' @name group_vars
#' @export
#' @importFrom dplyr group_vars
NULL


#' @rdname groups
#' @name group_data
#' @export
#' @importFrom dplyr group_data
NULL

#' @rdname groups
#' @name group_keys
#' @export
#' @importFrom dplyr group_keys
NULL

#' @rdname groups
#' @name group_rows
#' @export
#' @importFrom dplyr group_rows
NULL

#' @rdname groups
#' @name group_indices
#' @export
#' @importFrom dplyr group_indices
NULL

#' @rdname groups
#' @name group_size
#' @export
#' @importFrom dplyr group_size
NULL

#' @rdname groups
#' @name n_groups
#' @export
#' @importFrom dplyr n_groups
NULL


#' @name group_trim
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr group_trim
NULL

#' @export
group_trim.tbl_svy <- function(.tbl, .drop = dplyr::group_by_drop_default(.tbl)) {
  .tbl
}

#' @export
group_trim.grouped_svy <- function (.tbl, .drop = dplyr::group_by_drop_default(.tbl)) {
  vars <- group_vars(.tbl)
  ungrouped <- ungroup(.tbl)
  fgroups <- dplyr::tbl_vars(select_if(select_at(ungrouped, vars), is.factor))
  dropped <- mutate_at(ungrouped, fgroups, droplevels)
  group_by_at(dropped, vars, .drop = .drop)
}
