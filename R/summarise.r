#' @export
summarise_.tbl_svy <- function(.data, ..., .dots) {
  .dots <- lazyeval::all_dots(.dots, ...)

  survey_funs <- list(
    survey_mean = function(...) survey_mean(..., .svy = .data),
    survey_total = function(...) survey_total(..., .svy = .data),
    survey_ratio = function(...) survey_ratio(..., .svy = .data),
    survey_quantile = function(...) survey_quantile(..., .svy = .data),
    survey_median = function(...) survey_median(..., .svy = .data),
    unweighted = function(...) unweighted(..., .svy = .data)
  )

  out <- lazyeval::lazy_eval(.dots, c(survey_funs, .data$variables))
  # use the argument names to name the output
  out <- lapply(seq_along(out), function(x) {
    stats::setNames(out[[x]], paste0(names(out[x]), names(out[[x]])))
  })

  out <- dplyr::bind_cols(out)
  dplyr::tbl_df(out)
}

#' @export
summarise_.grouped_svy <- function(.data, ..., .dots) {
  .dots <- lazyeval::all_dots(.dots, ...)

  survey_funs <- list(
    survey_mean = function(...) survey_mean(..., .svy = .data),
    survey_total = function(...) survey_total(..., .svy = .data),
    survey_ratio = function(...) survey_ratio(..., .svy = .data),
    survey_quantile = function(...) survey_quantile(..., .svy = .data),
    survey_median = function(...) survey_median(..., .svy = .data),
    unweighted = function(...) unweighted(..., .svy = .data)
  )

  groups <- as.character(groups(.data))

  out <- lazyeval::lazy_eval(.dots, c(survey_funs, .data$variables))
  # use the argument names to name the output
  out <- lapply(seq_along(out), function(x) {
    unchanged_names <- groups
    changed_names <- setdiff(names(out[[x]]), groups)
    results <- stats::setNames(out[[x]], c(unchanged_names, paste0(names(out[x]),
                                                            changed_names)))
    results <- dplyr::arrange_(results, unchanged_names)

    # Only keep stratifying vars in first calculation so they're not repeated
    if (x > 1) results <- results[, !(names(results) %in% groups)]
    results
  })

  out <- dplyr::bind_cols(out)
  dplyr::tbl_df(out)
}

#' Summarise multiple values to a single value.
#'
#' Summarise multiple values to a single value.
#'
#' @usage summarise(.data, ...)
#' summarize(.data, ...)
#' summarise_(.data, ..., .dots)
#' summarize_(.data, ..., .dots)
#'
#' @param .data, tbl A \code{tbl_svy} object
#' @param ... Name-value pairs of summary functions
#' @param .dots Used to work around non-standard evaluation. See
#' \code{vignette("nse", package = "dplyr")} for details.
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
#' The available functions are:
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
#' @rdname summarise
NULL

#' @name summarize
#' @export
#' @importFrom dplyr summarize
#' @rdname summarise
NULL

#' @name summarize_
#' @export
#' @importFrom dplyr summarize_
#' @rdname summarise
NULL
