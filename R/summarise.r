#' @export
summarise_.tbl_svy <- function(.data, ..., .dots) {
  .dots <- lazyeval::all_dots(.dots, ...)

  survey_funs <- list(
    survey_mean = function(...) survey_mean(.data, ...),
    survey_total = function(...) survey_total(.data, ...),
    survey_ratio = function(...) survey_ratio(.data, ...),
    survey_quantile = function(...) survey_quantile(.data, ...),
    survey_median = function(...) survey_median(.data, ...),
    unweighted = function(...) unweighted(.data, ...)
  )

  out <- lazyeval::lazy_eval(.dots, c(survey_funs, .data$variables))
  # use the argument names to name the output
  out <- lapply(seq_along(out), function(x) {
    setNames(out[[x]], paste0(names(out[x]), names(out[[x]])))
  })

  dplyr::bind_cols(out)
}

#' @export
summarise_.grouped_svy <- function(.data, ..., .dots) {
  .dots <- lazyeval::all_dots(.dots, ...)

  survey_funs <- list(
    survey_mean = function(...) survey_mean(.data, ...),
    survey_total = function(...) survey_total(.data, ...),
    survey_ratio = function(...) survey_ratio(.data, ...),
    survey_quantile = function(...) survey_quantile(.data, ...),
    survey_median = function(...) survey_median(.data, ...),
    unweighted = function(...) unweighted(.data, ...)
  )

  groups <- as.character(groups(.data))

  out <- lazyeval::lazy_eval(.dots, c(survey_funs, .data$variables))
  # use the argument names to name the output
  out <- lapply(seq_along(out), function(x) {
    unchanged_names <- groups
    changed_names <- setdiff(names(out[[x]]), groups)
    results <- setNames(out[[x]], c(unchanged_names, paste0(names(out[x]),
                                                            changed_names)))
    results <- dplyr::arrange_(results, unchanged_names)

    # Only keep stratifying vars in first calculation so they're not repeated
    if (x > 1) results <- results[, !(names(results) %in% groups)]
    results
  })


  dplyr::bind_cols(out)
}

#' Summarise multiple values to a single value.
#'
#' Summarise multiple values to a single value.
#'
#' \describe{
#' Summarise for \code{tbl_svy} objects accepts several specialized functions:
#' \item{\code{survey_mean(x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95, proportion = FALSE, prop_method = NULL)}}{
#'    Calculate the survey mean of the entire population or by \code{groups}.}
#' \item{\code{survey_total(x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95)}}{
#'    Calculate the survey total of the entire population or by \code{groups}.}
#'  \item{\code{survey_ratio(numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95)}}{
#'    Calculate the ratio of 2 variables in the entire population or by \code{groups}.}
#' \item{\code{survey_quantile(x, quantiles, na.rm = FALSE, vartype = c("se", "ci"), level = 0.95, q_method = "linear", f = 1, interval_type = c("Wald", "score", "betaWald"), ties = c("discrete", "rounded"))}}{
#'    Calculate quantiles in the entire population or by \code{groups}.}
#'  \item{\code{survey_median(x, na.rm = FALSE, vartype = c("se", "ci"), level = 0.95)}}{
#'    Calculate the median in the entire population or by \code{groups}.}
#'  \item{\code{unweighted(x)}}{
#'    Calculate an unweighted estimate as you would on a regular \code{tbl_df}.}
#' }
#'
#' @usage summarise(.data, ...)
#' @usage summarize(.data, ...)
#' @usage summarise_(.data, ..., .dots)
#' @usage summarize_(.data, ..., .dots)
#'
#' @param .data A \code{tbl_svy} object
#' @param ... Name-value paris of summary functions
#'
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
#' @aliases survey_mean survey_total survey_ratio survey_quantile survey_median unweighted
#' @export
#' @importFrom dplyr summarise
NULL

#' @name summarise_
#' @export
#' @importFrom dplyr summarise_
#' @rdname summarise
NULL

#' @name summarize
#' @rdname summarise
#' @export
#' @importFrom dplyr summarize
NULL

#' @name summarize_
#' @rdname summarise
#' @export
#' @importFrom dplyr summarize_
NULL
