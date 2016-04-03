#' @export
cascade_.tbl_svy <- function(.data, ..., .dots, .fill = NA) {
  summarise_.tbl_svy(.data, ..., .dots)
}

#' @export
cascade_.grouped_svy <- function(.data, ..., .dots, .fill = NA) {
  .dots <- lazyeval::all_dots(.dots, ...)

  groups <- as.character(groups(.data))
  group_cascade <- lapply(rev(seq_along(groups)), function(x) groups[seq_len(x)])
  group_cascade[length(group_cascade) + 1] <- ""

  out <- lapply(group_cascade,
                function(ggg) {
                  if (ggg[1] != "") {
                    casc <- summarise_(group_by_(.data, .dots = ggg), .dots = .dots)
                  } else {
                    casc <- summarise_(ungroup(.data), .dots = .dots)
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

  out <- dplyr::arrange_(out, groups)
  out
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
cascade <- function(.data, ..., .fill = NA) {
  cascade_(.data, .dots = lazyeval::lazy_dots(...), .fill = .fill)
}

#' @export
#' @rdname cascade
cascade_ <- function(.data, ..., .dots, .fill = NA) {
  UseMethod("cascade_")
}
