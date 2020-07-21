#' @export
summarise.tbl_svy <- function(.data, ..., .groups = NULL) {
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
    if (!is.data.frame(out)) {
      dplyr::tibble(!!names(.dots[x]) := out)
    } else {
      stats::setNames(out, paste0(names(.dots)[x], var_names))
    }
  })
  summarise_result_nrow_check(out, names(.dots))
  # since there are no groups, .groups="drop_last" is equivalent to .groups="keep"
  if (is.null(.groups)) {
    .groups <- "drop_last"
  }
  end_grouping_func <- finalize_grouping(.data, .groups)

  out <- dplyr::bind_cols(out)
  out <- tibble::as_tibble(out)
  end_grouping_func(out)
}

#' @export
summarise_.tbl_svy <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

#' @export
summarise.grouped_svy <- function(.data, ..., .groups = NULL) {
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
    # In case there are multi-row results, make a within group ID
    results <- dplyr::group_by_at(results, groups)
    results <- dplyr::mutate(results, `__SRVYR_WITHIN_GRP_ID__` = dplyr::row_number())
    results <- dplyr::ungroup(results)

    results
  })

  # if all arguments return length 1 data.frames for each group then we drop_last,
  # otherwise we keep
  if (is.null(.groups)) {
    if (all(vapply(calculations, function(x) max(x[["__SRVYR_WITHIN_GRP_ID__"]]) == 1, logical(1)))) {
      .groups <- "drop_last"
    } else {
      .groups <- "keep"
    }
  }
  end_grouping_func <- finalize_grouping(.data, .groups)

  # Create a skeleton of a summary using dplyr:::summarize.tbl_df
  # So that we handle the .drop cases. See https://github.com/gergness/srvyr/issues/49
  out <- dplyr::summarize((.data$variables), `___SRVYR_DROP___` = 1)
  out[["___SRVYR_DROP___"]] <- NULL
  out <- dplyr::ungroup(out)

  # In order to handle multi-row returns, go row by row in the skeleton so we
  # can check that the return sizes are valid for each group
  out <- lapply(seq_len(nrow(out)), function(grp_id) {
    grp_slice <- dplyr::slice(out, grp_id)
    merged_slice <- Reduce(
      function(merged_slice, calc_num) {
        calc <- calculations[[calc_num]]

        calc_slice <- dplyr::semi_join(calc, merged_slice, by = groups)
        if (nrow(calc_slice) == 1) {
          calc_slice[["__SRVYR_WITHIN_GRP_ID__"]] <- NULL
          merged_slice <- dplyr::left_join(merged_slice, calc_slice, by = groups)
        } else if (nrow(merged_slice) == 1) {
          merged_slice <- dplyr::left_join(merged_slice, calc_slice, by = groups)
        } else if (nrow(calc_slice) == nrow(merged_slice)) {
          merged_slice <- dplyr::left_join(merged_slice, calc_slice, by = c(groups, "__SRVYR_WITHIN_GRP_ID__"))
        } else {
          arg_name <- names(.dots)[calc_num]
          grp_ids <- paste0(names(grp_slice), " = ", unname(grp_slice), collapse = ", ")
          stop(paste0(
            "summarise results for argument `", arg_name, "` must be size 1 or ",
            nrow(merged_slice), " but it is ", nrow(calc_slice), " for group: ",
            grp_ids
          ))
        }
      },
      seq_along(calculations),
      grp_slice
    )
    merged_slice[["__SRVYR_WITHIN_GRP_ID__"]] <- NULL
    merged_slice
  })

  out <- dplyr::bind_rows(out)
  end_grouping_func(out)
}

#' @export
summarise_.grouped_svy <- function(.data, ..., .dots) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  summarise(.data, !!!dots)
}

finalize_grouping <- function(x, .groups) {
  orig_groups <- group_vars(x)

  switch(
    .groups,
    "drop" = identity,
    "drop_last" = function(x) group_by_at(x, orig_groups[-length(orig_groups)]),
    "keep" = function(x) group_by_at(x, orig_groups),
    "rowwise" = dplyr::rowwise
  )
}

summarise_result_nrow_check <- function(results, names) {
  allowed_row_nums <- 1
  lapply(seq_along(results), function(iii) {
    res_name <- names[[iii]]
    cur_rows <- nrow(results[[iii]])
    if (!cur_rows %in% c(1, allowed_row_nums)) {
      if (allowed_row_nums != 1) {
        stop(paste0(
          "summarise results for argument `", res_name, "` must be size 1 or ",
          allowed_row_nums, " but it is ", cur_rows
        ))
      } else {
        allowed_row_nums <<- cur_rows
      }
    }
  })
}

#' Summarise multiple values to a single value.
#'
#' Summarise multiple values to a single value.
#'
#'
#' @param .data tbl A \code{tbl_svy} object
#' @param ... Name-value pairs of summary functions
#' @param .groups Defaults to "drop_last" in srvyr meaning that the last group is peeled
#' off, but if there are more groups they will be preserved. Other options are "drop", which
#' drops all groups, "keep" which keeps all of them and "rowwise" which converts the object
#' to a rowwise object (meaning calculations will be performed on each row).
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

