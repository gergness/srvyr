#' @export
summarise_.tbl_svy <- function(.data, ..., .dots) {
  .dots <- lazyeval::all_dots(.dots, ...)

  survey_funs <- list(survey_mean = function(...) survey_mean(.data, ...),
                      survey_total = function(...) survey_total(.data, ...),
                      survey_ratio = function(...) survey_ratio(.data, ...),
                      survey_quantile = function(...) survey_quantile(.data, ...),
                      survey_median = function(...) survey_median(.data, ...))

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

  survey_funs <- list(survey_mean = function(...) survey_mean(.data, ...),
                      survey_total = function(...) survey_total(.data, ...),
                      survey_ratio = function(...) survey_ratio(.data, ...),
                      survey_quantile = function(...) survey_quantile(.data, ...),
                      survey_median = function(...) survey_median(.data, ...))

  groups <- as.character(groups(.data))

  out <- lazyeval::lazy_eval(.dots, c(survey_funs, .data$variables))
  # use the argument names to name the output
  out <- lapply(seq_along(out), function(x) {
    unchanged_names <- groups
    changed_names <- setdiff(names(out[[x]]), groups)
    results <- setNames(out[[x]], c(unchanged_names, paste0(names(out[x]), changed_names)))

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
#' \item{\code{survey_mean(..., na.rm = FALSE, vartype = c("se", "ci", "var")}}{
#'    Calculate the survey mean of the entire population or by \code{groups}.}
#' \item{\code{survey_total(..., na.rm = FALSE, vartype = c("se", "ci", "var")}}{
#'    Calculate the survey total of the entire population or by \code{groups}.}
#'  \item{\code{survey_mean(..., na.rm = FALSE, vartype = c("se", "ci", "var")}}{
#'    Calculate the ratio of 2 variables in the entire population or by \code{groups}.}
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
#'   design_survey(strata = stype, weights = pw)
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
#' @aliases survey_mean survey_total survey_ratio
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

