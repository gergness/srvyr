#' @export
summarise.tbl_svy <- function(.data, ...) {
  .dots <- rlang::quos(...)

  if (is_lazy_svy(.data)) .data <- localize_lazy_svy(.data, .dots)

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(.data)
  on.exit(set_current_svy(old), add = TRUE)

  # use the argument names to name the output
  out <- lapply(seq_along(.dots), function(x) {
    out <- rlang::eval_tidy(.dots[[x]], .data$variables)
    var_names <- names(out)
    vname_is_coef <- var_names == "__SRVYR_COEF__"
    if (any(vname_is_coef)) var_names[vname_is_coef] <- ""
    stats::setNames(out, paste0(names(.dots)[x], var_names))
  })

  out <- dplyr::bind_cols(out)
  tibble::as_tibble(out)
}

#' @export
summarise_.tbl_svy <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

#' @export
summarise.grouped_svy <- function(.data, ...) {
  .dots <- rlang::quos(...)

  if (is_lazy_svy(.data)) .data <- localize_lazy_svy(.data, .dots)

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(.data)
  on.exit(set_current_svy(old), add = TRUE)

  groups <- group_vars(.data)
  # use the argument names to name the output
  calculations <- lapply(seq_along(.dots), function(x) {
    out <- rlang::eval_tidy(.dots[[x]], .data$variables)
    unchanged_names <- groups
    changed_names <- setdiff(names(out), groups)
    changed_names_is_coef <- changed_names == "__SRVYR_COEF__"
    changed_names[which(changed_names_is_coef)] <- ""
    results <- stats::setNames(out, c(unchanged_names, paste0(names(.dots)[x], changed_names)))
    results <- dplyr::arrange(results, !!!rlang::syms(unchanged_names))

    results
  })

  # Create a skeleton of a summary using dplyr:::summarize.tbl_df
  # So that we handle the .drop cases. See https://github.com/gergness/srvyr/issues/49
  out <- dplyr::summarize((.data$variables), `___SRVYR_DROP___` = 1)
  out[["___SRVYR_DROP___"]] <- NULL
  for (ccc in calculations) {
    out <- dplyr::left_join(out, ccc, by = groups)
  }

  dplyr::tbl_df(out)
}

#' @export
summarise_.grouped_svy <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

#' Summarise multiple values to a single value.
#'
#' Summarise multiple values to a single value.
#'
#' @usage summarise(.data, ...)
#' summarize(.data, ...)
#'
#' @param .data, tbl A \code{tbl_svy} object
#' @param ... Name-value pairs of summary functions
#'
#' @details
#' Summarise for \code{tbl_svy} objects accepts several specialized functions.
#' Each of the functions a variable (or two, in the case of
#' \code{survey_ratio}), from the data.frame and default to providing the measure
#' and its standard error.
#'
#' The argument \code{vartype} can choose one or more measures of uncertainty,
#' \code{se} for standard error, \code{ci} for confidence interval, \code{var}
#' for variance, and \code{cv} for coefficient of variation. \code{level}
#' specifies the level for the confidence interval.
#'
#' The other arguments correspond to the analagous function arguments from the
#' survey package.
#'
#' The available functions from srvyr are:
#'
#'\describe{
#' \item{\code{\link{survey_mean}}}{
#'    Calculate the survey mean of the entire population or by \code{groups}.
#'    Based on \code{\link[survey]{svymean}}.}
#' \item{\code{\link{survey_total}}}{
#'    Calculate the survey total of the entire population or by \code{groups}.
#'    Based on \code{\link[survey]{svytotal}}.}
#'  \item{\code{\link{survey_ratio}}}{
#'    Calculate the ratio of 2 variables in the entire population or by \code{groups}.
#'    Based on \code{\link[survey]{svyratio}}.}
#' \item{\code{\link{survey_quantile}}}{
#'    Calculate quantiles in the entire population or by \code{groups}. Based on
#'    \code{\link[survey]{svyquantile}}.}
#'  \item{\code{\link{survey_median}}}{
#'    Calculate the median in the entire population or by \code{groups}.
#'    \code{\link[survey]{svyquantile}}.}
#'  \item{\code{\link{unweighted}}}{
#'    Calculate an unweighted estimate as you would on a regular \code{tbl_df}.
#'    Based on dplyr's \code{\link[dplyr]{summarise}}.}
#'}
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99 = survey_mean(api99),
#'             api00 = survey_mean(api00),
#'             api_diff = survey_mean(api00 - api99))
#'
#' dstrata_grp <- dstrata %>%
#'   group_by(stype)
#'
#' dstrata_grp %>%
#'   summarise(api99 = survey_mean(api99),
#'             api00 = survey_mean(api00),
#'             api_diff = survey_mean(api00 - api99))
#'
#' @name summarise
#' @export
#' @importFrom dplyr summarise
NULL

#' @name summarise_
#' @export
#' @importFrom dplyr summarise_
#' @rdname srvyr-se-deprecated
#' @inheritParams summarise
NULL

#' @name summarize
#' @export
#' @importFrom dplyr summarize
#' @rdname summarise
NULL

#' @name summarize_
#' @export
#' @importFrom dplyr summarize_
#' @rdname srvyr-se-deprecated
#' @inheritParams summarize
NULL

