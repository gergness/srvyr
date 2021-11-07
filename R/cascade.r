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
#'   cascade(api99_mn = survey_mean(api99),
#'             api00_mn = survey_mean(api00),
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
cascade.grouped_svy <- function(.data, ..., .dots, .fill = NA, .groupings = NULL) {
  dots <- rlang::quos(...)
  .groupings <- rlang::enquo(.groupings)

  if (rlang::quo_is_null(.groupings)) .groupings <- cascade_groupings(.data)

  out <- lapply(.groupings, function(ggg) {
      summarise(group_by(.data, !!!ggg), !!!dots)
  })

  # TODO: take .fill into account

  out <- dplyr::bind_rows(out)
  # TODO: arrange (taking the interact cols into account?)
  out
}


# Figure out which groupings to use in cascade from a grouped survey's groups
cascade_groupings <- function(tbl) {
  group_vars <- groups(tbl)
  group_vars_expanded <- lapply(group_vars, function(gv_sym) {
    var <- dplyr::pull(tbl, gv_sym)
    if (!is.interaction(var)) {
      return(gv_sym)
    }
    # get all combinations of interaction terms
    c(
      list(gv_sym),
      all_term_combos(gv_sym, var)
    )
  })

  out <- lapply(seq(length(group_vars), 0), function(end) {
    if (end == 0) return(list(NULL))
    lapply(group_vars_expanded[[end]], function(gv) c(group_vars[seq_len(end - 1)], gv))
  })
  unlist(out, recursive = FALSE)
}


all_term_combos <- function(var_sym, var) {
  terms <- interact_terms(var)

  out <- lapply(
    seq(length(terms) - 1, 1), # subtract 1 because don't need to recast to get
    function(num) {
      combn(terms, num, simplify = FALSE, function(term_syms) {
        rlang::expr(recast_interact(!!var_sym, !!!rlang::syms(term_syms)))
      })
    })
  unlist(out, recursive = FALSE)
}


#' @export
#' @rdname srvyr-se-deprecated
#' @inheritParams cascade
cascade_ <- function(.data, ..., .dots, .fill = NA) {
  UseMethod("cascade_")
}
