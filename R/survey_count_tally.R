#' Count/tally survey weighted observations by group
#'
#' Analogous to \code{\link[dplyr]{tally}} and count, calculates the survey weighted
#' count of observations. \code{survey_tally} will call \code{\link{survey_total}} empty (resulting
#' in the count of each group) or on \code{wt} if it is specified (resulting in the
#' survey weighted total of \code{wt}). \code{survey_count} is similar, but calls \code{group_by}
#' before calculating the count and then returns the data to the original groupings.
#'
#' If \code{n} already exists, \code{tally} will use it as the weight, but \code{count}
#' will not.
#'
#' @param x A tbl_svy object, as created by \code{as_survey} and related functions.
#' @param wt (Optional) A variable to weight on (in addition to the survey weights,
#'    which are always used). If left unspecified, \code{tally()} will use a variable
#'    named "n" if one exists, but \code{count()} will not. Override this behavior by
#'    specifying \code{wt = NULL}.
#' @param sort Whether to sort the results (defaults to \code{FALSE})
#' @param name Name of count variable created (defaults to n). If the variable already
#'   exists, will add "n" to the end until it does not.
#' @param vartype What types variation estimates to calculate, passed to
#' \code{\link{survey_total}}.
#' @param ... Variables to group by, passed to \code{group_by()}.
#' @param .drop When .drop = TRUE, empty groups are dropped, see \code{\link[dplyr]{group_by}}
#'   documentation for more details.
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   survey_tally()
#'
#' dstrata %>%
#'   survey_count(awards)
#'
#' @export
survey_tally <- function(
  x, wt, sort = FALSE, name = "n", vartype =  c("se", "ci", "var", "cv")
) {
  if (!inherits(x, "tbl_svy")) {
    stop("survey_tally can only be called on `tbl_svy` objects")
  }

  wt <- enquo(wt)
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }

  if (quo_is_missing(wt) && "n" %in% tbl_vars(x)) {
    inform("Using `n` as weighting variable")
    wt <- quo(n)
  }

  if (quo_is_missing(wt) || quo_is_null(wt)) {
    n <- quo(survey_total(vartype = vartype))
  } else {
    n <- quo(survey_total(!!wt, vartype = vartype, na.rm = TRUE))
  }

  n_name <- n_name(dplyr::group_vars(x), name)

  if (name != "n" && name %in% group_vars(x)) {
    abort(paste0("Column `", name, "` already exists in grouped variables"))
  }

  out <- dplyr::summarise(x, !!n_name := !!n)

  if (sort) {
    dplyr::arrange(out, dplyr::desc(!!sym(n_name)))
  } else {
    out
  }
}

#' @export
#' @rdname survey_tally
survey_count <- function(
  x, ..., wt = NULL, sort = FALSE, name = "n", .drop = dplyr::group_by_drop_default(x),
  vartype =  c("se", "ci", "var", "cv")
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  groups <- dplyr::group_vars(x)
  if (dots_n(...)) {
    x <- group_by(x, ..., .add = TRUE, .drop = .drop)
  }
  x <- survey_tally(x, wt = !!enquo(wt), sort = sort, name = name, vartype = vartype)
  x <- group_by(x, !!!syms(groups), .add = FALSE, .drop = .drop)
  x
}



# from dplyr https://github.com/tidyverse/dplyr/blob/master/R/compat-future-group_by.R
n_name <- function(x, name = "n") {
  while (name %in% x) {
    name <- paste0("n", name)
  }

  name
}
