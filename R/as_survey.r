#' Create a tbl_svy from a data.frame
#'
#' \code{as_survey} can be used to create a \code{tbl_svy} using design information
#' (\code{\link{as_survey_design}}), replicate weights (\code{\link{as_survey_rep}}),
#' or a two phase design (\code{\link{as_survey_twophase}}), or an object created by the
#' survey package. \code{as_survey_} is its standard evaluation counterpart.
#'
#' @param .data a data.frame or an object from the survey package
#' @param ... other arguments, see other functions for details
#'
#' @return a tbl_svy
#' @export
#' @examples
#' # Examples from ?survey::svydesign
#' library(survey)
#' library(dplyr)
#' data(api)
#'
#' # stratified sample
#' dstrata <- apistrat %>%
#'   as_survey(strata = stype, weights = pw)
#'
#' # Examples from ?survey::svrepdesign
#' data(scd)
#' # use BRR replicate weights from Levy and Lemeshow
#' scd$rep1 <- 2 * c(1, 0, 1, 0, 1, 0)
#' scd$rep2 <- 2 * c(1, 0, 0, 1, 0, 1)
#' scd$rep3 <- 2 * c(0, 1, 1, 0, 0, 1)
#' scd$rep4 <- 2 * c(0, 1, 0, 1, 1, 0)
#'
#' scdrep <- scd %>%
#'   as_survey(type = "BRR", repweights = starts_with("rep"),
#'                     combined_weights = FALSE)
#'
#' # Examples from ?survey::twophase
#' # two-phase simple random sampling.
#' data(pbc, package="survival")
#'
#' pbc <- pbc %>%
#'   mutate(randomized = !is.na(trt) & trt > 0,
#'          id = row_number())
#' d2pbc <- pbc %>%
#'   as_survey(id = list(id, id), subset = randomized)
#'
#' # as_survey_ uses standard evaluation
#' dstrata <- apistrat %>%
#'   as_survey_(strata = "stype", weights = "pw")
#'
as_survey <- function(.data, ...) {
  UseMethod("as_survey")
}

#' @export
#' @rdname as_survey
as_survey.data.frame <- function(.data, ...) {
  dots <- lazyeval::lazy_dots(...)
  if ("repweights" %in% names(dots)) {
    as_survey_rep(.data, ...)
  } else if ("id" %in% names(dots) | "ids" %in% names(dots)) {
    # twophase has a list of 2 groups for id, while regular id is just a
    # set of variables
    id_expr <- as.character(dots$id$expr)
    if ("list" %in% id_expr & length(id_expr) == 3) {
      as_survey_twophase(.data, ...)
    } else {
      as_survey_design(.data, ...)
    }
  } else {
    as_survey_design(.data, ...)
  }
}


#' @export
#' @rdname as_survey
as_survey.survey.design2 <- function(.data, ...) {
  as_tbl_svy(.data)
}


#' @export
#' @rdname as_survey
as_survey.svyrepdesign <- function(.data, ...) {
  as_tbl_svy(.data)
}

#' @export
#' @rdname as_survey
as_survey.twophase2 <- function(.data, ...) {
  as_tbl_svy(.data)
}

#' @export
#' @rdname as_survey
as_survey_ <- function(.data, ...) {
  dots <- lazyeval::lazy_dots(...)
  if ("repweights" %in% names(dots)) {
    as_survey_rep_(.data, ...)
  } else if ("id" %in% names(dots) | "ids" %in% names(dots)) {
    # twophase has a list of 2 groups for id, while regular id is
    # just a set of variables
    id_expr <- as.character(dots$id$expr)
    if ("list" %in% id_expr & length(id_expr) == 3) {
      as_survey_twophase_(.data, ...)
    } else {
      as_survey_design_(.data, ...)
    }
  } else {
    as_survey_design_(.data, ...)
  }
}
