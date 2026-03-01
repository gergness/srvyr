#' @export
summarise.tbl_svy <- function(.data, ..., .by = NULL, .groups = NULL, .unpack = TRUE) {
  .dots <- rlang::quos(...)
  .by <- rlang::enquos(.by)

  # Can't just pass `.by` to dplyr because we need to calculate survey statistics per group
  if (!all(sapply(.by, rlang::quo_is_null))) {
    .data <- group_by(.data, across(!!!.by))
    return(ungroup(summarise(.data, !!!.dots, .groups = .groups, .unpack = .unpack)))
  }

  if (is_lazy_svy(.data)) .data <- localize_lazy_svy(.data, .dots)

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(list(full = .data, split = split_for_context(.data)))
  on.exit(set_current_svy(old), add = TRUE)

  out <- dplyr::summarise(.data$variables, ..., .groups = .groups)

  # srvyr predates dplyr's data.frame columns so default to unpacking
  # them wide
  if (.unpack) out <- unpack_cols(out)
  out
}

#' @export
summarise_.tbl_svy <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

#' @export
summarise.grouped_svy <- function(.data, ..., .groups = NULL, .unpack = TRUE) {
  .dots <- rlang::quos(...)
  if (is_lazy_svy(.data)) .data <- localize_lazy_svy(.data, .dots)

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(list(full = .data, split = split_for_context(.data)))
  on.exit(set_current_svy(old), add = TRUE)

  out <- dplyr::summarise(.data$variables, !!!.dots, .groups = .groups)

  # Remove interaction variables if present
  out <- uninteract(out)

  # srvyr predates dplyr's data.frame columns so default to unpacking
  # them wide
  if (.unpack) out <- unpack_cols(out)
  out
}

unpack_cols <- function(results) {
  old_groups <- group_vars(results)
  is_rowwise <- inherits(results, "rowwise_df")

  # Top level renames
  var_names <- names(results)[vapply(results, is_srvyr_result_df, logical(1))]
  out <- tidyr::unpack(
    results,
    dplyr::all_of(var_names),
    # ugly regex hack to get around https://github.com/tidyverse/tidyr/issues/1161
    # __SRVYR_COEF__ is to allow the possibility of legacy srvyr extensions
    names_sep = "___SRVYR_SEP___",
    names_repair = ~gsub("___SRVYR_SEP___(coef)?(__SRVYR_COEF__)?", "", .)
  )

  # Also check if there are some nested srvyr results (recursively)
  var_names <- names(out)[vapply(out, is.data.frame, logical(1))]
  out <- dplyr::mutate(out, dplyr::across(dplyr::all_of(var_names), unpack_cols))

  # restore grouping/rowwise (dplyr unpacking can remove rowwise sometimes)
  if (length(old_groups) > 0 & !is_rowwise) {
    out <- group_by(out, !!!rlang::syms(old_groups))
  } else if (length(old_groups) > 0 & is_rowwise) {
    out <- dplyr::rowwise(out, !!!rlang::syms(old_groups))
  } else if (is_rowwise) {
    out <- dplyr::rowwise(out)
  }

  out
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
#'
#' @param .data tbl A \code{tbl_svy} object
#' @param ... Name-value pairs of summarizing expressions, see details
#' @param .groups Defaults to "drop_last" in srvyr meaning that the last group is peeled
#' off, but if there are more groups they will be preserved. Other options are "drop", which
#' drops all groups, "keep" which keeps all of them and "rowwise" which converts the object
#' to a rowwise object (meaning calculations will be performed on each row).
#' @param .unpack Whether to "unpack" named \code{data.frame} columns. \code{srvyr} predates
#' \code{dplyr}'s support for data.frame columns so it does not treat them the same way by
#' default.
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
#' The other arguments correspond to the analogous function arguments from the
#' survey package.
#'
#' The available functions from srvyr are:
#'
#'\describe{
#' \item{\code{\link{survey_mean}}}{
#'    Calculate the mean of a numeric variable or the proportion falling into \code{groups}
#'    for the entire population or by \code{groups}. Based on \code{\link[survey]{svymean}}
#'    and \code{\link[survey]{svyciprop}}.}.
#' \item{\code{\link{survey_total}}}{
#'    Calculate the survey total of the entire population or by \code{groups}.
#'    Based on \code{\link[survey]{svytotal}}.}
#' \item{\code{\link{survey_prop}}}{
#'    Calculate the proportion of the entire population or by \code{groups}.
#'    Based on \code{\link[survey]{svyciprop}}.}
#'  \item{\code{\link{survey_ratio}}}{
#'    Calculate the ratio of 2 variables in the entire population or by \code{groups}.
#'    Based on \code{\link[survey]{svyratio}}.}
#' \item{\code{\link{survey_quantile}} & \code{\link{survey_median}}}{
#'    Calculate quantiles in the entire population or by \code{groups}. Based on
#'    \code{\link[survey]{svyquantile}}.}
#'  \item{\code{\link{unweighted}}}{
#'    Calculate an unweighted estimate as you would on a regular \code{tbl_df}.
#'    Based on dplyr's \code{\link[dplyr]{summarise}}.}
#'}
#'
#' You can use expressions both in the \code{...} of \code{summarize} and also
#' in the arguments to the summarizing functions. Though this is valid syntactically
#' it can also allow you to calculate incorrect results (for example if you multiply
#' the mean by 100, the standard error is also multiplied by 100, but the variance
#' is not).
#'
#' @examples
#' data(api, package = "survey")
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99_mn = survey_mean(api99),
#'             api00_mn = survey_mean(api00),
#'             api_diff = survey_mean(api00 - api99))
#'
#' dstrata_grp <- dstrata %>%
#'   group_by(stype)
#'
#' dstrata_grp %>%
#'   summarise(api99_mn = survey_mean(api99),
#'             api00_mn = survey_mean(api00),
#'             api_diff = survey_mean(api00 - api99))
#'
#' # `dplyr::across` can be used to programmatically summarize multiple columns
#' # See https://dplyr.tidyverse.org/articles/colwise.html for details
#' # A basic example of working on 2 columns at once and then calculating the total
#' # the mean
#' total_vars <- c("enroll", "api.stu")
#' dstrata %>%
#'   summarize(across(c(all_of(total_vars)), survey_total))
#'
#' # Expressions are allowed in summarize arguments & inside functions
#' # Here we can calculate binary variable on the fly and also multiply by 100 to
#' # get percentages
#' dstrata %>%
#'   summarize(api99_over_700_pct = 100 * survey_mean(api99 > 700))
#'
#' # But be careful, the variance doesn't scale the same way, so this is wrong!
#' dstrata %>%
#'   summarize(api99_over_700_pct = 100 * survey_mean(api99 > 700, vartype = "var"))
#' # Wrong variance!
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

