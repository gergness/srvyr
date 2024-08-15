#' Create a tbl_svy from a data.frame
#'
#' \code{as_survey} can be used to create a \code{tbl_svy} using design information
#' (\code{\link{as_survey_design}}), replicate weights (\code{\link{as_survey_rep}}),
#' or a two phase design (\code{\link{as_survey_twophase}}), or an object created by the
#' survey package.
#'
#' See \code{vignette("databases", package = "dplyr")}
#' for more information on setting up databases in dplyr.
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
#' # dplyr 0.7 introduced new style of NSE called quosures
#' # See `vignette("programming", package = "dplyr")` for details
#' st <- quo(stype)
#' wt <- quo(pw)
#' dstrata <- apistrat %>%
#'   as_survey(strata = !!st, weights = !!wt)
as_survey <- function(.data, ...) {
  UseMethod("as_survey")
}

#' @export
#' @rdname as_survey
as_survey.tbl_svy <- function(.data, ...) {
  if(dots_n(...) > 0) warn("Object is already a survey design and will be returned unchanged. The extra design arguments will be ignored. To reinitialise the design, convert to a tibble first.")
  .data
}

#' @export
#' @rdname as_survey
as_survey.data.frame <- function(.data, ...) {
  dots <- rlang::quos(...)
  if ("repweights" %in% names(dots)) {
    as_survey_rep(.data, ...)
  } else if ("id" %in% names(dots) | "ids" %in% names(dots)) {
    # twophase has a list of 2 groups for id, while regular id is just a
    # set of variables
    id_expr <- rlang::f_rhs(dots$id)
    if (length(id_expr) == 3 && id_expr[[1]] == "list") {
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
as_survey.tbl_lazy <- as_survey.data.frame

#' @export
#' @rdname as_survey
as_survey.survey.design2 <- function(.data, ...) {
  as_tbl_svy(.data)
}


#' @rdname as_survey
#' @export
as_survey.svyrep.design <- function(.data, ...) {
  as_tbl_svy(.data)
}

#' @export
#' @rdname as_survey
as_survey.twophase2 <- function(.data, ...) {
  as_tbl_svy(.data)
}

preserve_groups <- function(svy, data) {
  incoming_group_vars <- group_vars(data)
  if (length(incoming_group_vars) > 0) {
    if (all(incoming_group_vars %in% tbl_vars(svy))) {
      svy <- group_by(svy, across(all_of(incoming_group_vars)))
    } else {
      warning("Not all grouping variables exist in survey so groups were dropped")
    }
  }
  svy

}

#' Deprecated SE versions of main srvyr verbs
#'
#' srvyr has updated it's standard evaluation semantics to match dplyr 0.7, so
#' these underscore functions are no longer required (but are still supported
#' for backward compatibility reasons). See \code{\link[dplyr]{se-deprecated}} or the
#' dplyr vignette on programming (\code{vignette("programming", package =
#' "dplyr")}) for more details.
#' @name srvyr-se-deprecated
#' @inheritParams as_survey
#' @param .dots Used to work around non-standard evaluation. See
#' \code{vignette("nse", package = "dplyr")} for details.
#' @export
as_survey_ <- function(.data, ...) {
  warn_underscored()
  dots <- rlang::quos(...)
  if (inherits(.data, "tbl_svy")) {
    if(dots_n(...) > 0) warn("Object is already a survey design and will be returned unchanged. The extra design arguments will be ignored. To reinitialise the design, convert to a tibble first.")
    .data
  } else if ("repweights" %in% names(dots)) {
    as_survey_rep_(.data, ...)
  } else if ("id" %in% names(dots) | "ids" %in% names(dots)) {
    # twophase has a list of 2 groups for id, while regular id is
    # just a set of variables
    id_expr <- rlang::f_rhs(dots$id)
    if(is.character(id_expr)) id_expr <- rlang::parse_expr(id_expr)
    if (length(id_expr) == 3 && id_expr[[1]] == "list") {
      as_survey_twophase_(.data, ...)
    } else {
      as_survey_design_(.data, ...)
    }
  } else {
    as_survey_design_(.data, ...)
  }
}
