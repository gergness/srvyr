#' @export
cascade_.tbl_svy <- function(.data, ..., .dots, .fill = NA) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  cascade.tbl_svy(.data, !!!dots, .fill = .fill)
}

#' @export
cascade_.grouped_svy <- function(.data, ..., .dots, .fill = NA) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  cascade.grouped_svy(.data, !!!dots, .fill = .fill)
}

#' Summarise multiple values into cascading groups
#'
#' \code{cascade} is similar to \code{\link{summarise}}, but calculates
#' a summary statistics for the total of a group in addition to each group.
#'
#' @param .data, tbl A \code{tbl_svy} object
#' @param ... Name-value pairs of summary functions
#' @param .fill Value to fill in for group summaries
#' @param .dots Used to work around non-standard evaluation. See
#' \code{vignette("nse", package = "dplyr")} for details.
#'
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata_grp <- dstrata %>%
#'   group_by(stype)
#'
#' dstrata_grp %>%
#'   cascade(api99 = survey_mean(api99),
#'             api00 = survey_mean(api00),
#'             api_diff = survey_mean(api00 - api99))
#'
#' @export
cascade <- function(.data, ..., .dots, .fill = NA) {
  UseMethod("cascade")
}

#' @export
cascade.tbl_svy <- function(.data, ..., .fill = NA) {
  summarise.tbl_svy(.data, !!!quos(...))
}


#' @export
cascade.grouped_svy <- function(.data, ..., .dots, .fill = NA) {
  dots <- rlang::quos(...)

  groups <- as.character(groups(.data))
  group_cascade <- lapply(rev(seq_along(groups)), function(x) groups[seq_len(x)])
  group_cascade[length(group_cascade) + 1] <- ""

  out <- lapply(group_cascade,
                function(ggg) {
                  if (!identical(ggg, "")) {
                    casc <- summarise(group_by(.data, !!!rlang::syms(ggg)), !!!dots)
                  } else {
                    casc <- summarise(ungroup(.data), !!!dots)
                  }

                  missing_vars <- setdiff(groups, ggg)
                  if (length(missing_vars) > 0) casc[missing_vars] <- .fill

                  casc
                })

  # Add .fill to factor level where necessary
  for (ggg in groups) {
    if (class(.data$variables[[ggg]]) == "factor" & !is.na(.fill)) {
      for (iii in seq_along(out)) {
        out[[iii]][[ggg]] <- factor(out[[iii]][[ggg]],
                                    levels = c(levels(.data$variables[[ggg]]), .fill))
      }
    }
  }

  out <- dplyr::bind_rows(out)

  out <- dplyr::arrange(out, !!!rlang::syms(groups))
  out
}


#' @export
#' @rdname srvyr-se-deprecated
#' @inheritParams cascade
cascade_ <- function(.data, ..., .dots, .fill = NA) {
  UseMethod("cascade_")
}
