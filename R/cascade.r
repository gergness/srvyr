#' @export
cascade_.tbl_svy <- function(.data, ..., .dots, .fill = NA) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  cascade.tbl_svy(.data, !!!dots, .fill = .fill)
}

#' Summarise multiple values into cascading groups
#'
#' \code{cascade} is similar to \code{\link{summarise}}, but calculates
#' a summary statistics for the total of a group in addition to each group.
#' The groupings are chosen by "unpeeling" from the end of the groupings,
#' and also expanding out interactions to all terms (eg the interactions of
#' all combinations of subsets of variables as well as each variable on
#' it's own).
#'
#' @param .data, tbl A \code{tbl_svy} object
#' @param ... Name-value pairs of summary functions
#' @param .fill Value to fill in for group summaries
#' @param .fill_level_top When filling factor variables, whether to put the
#' value `.fill` in the first position (defaults to FALSE, placing it in
#' the bottom).
#' @param .groupings (Experimental) A list of lists of quosures to manually
#' specify the groupings to use, rather than the default.
#'
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' # Calculates the means by stype and also for the whole
#' # sample
#' dstrata %>%
#'   group_by(stype) %>%
#'   cascade(api99_mn = survey_mean(api99),
#'             api00_mn = survey_mean(api00),
#'             api_diff = survey_mean(api00 - api99))
#'
#' # Calculates the proportions by the interaction of stype & awards
#' # as well as by each of those variable's groups alone, and finally
#' # the total as well
#' dstrata %>%
#'   group_by(interact(stype, awards)) %>%
#'   cascade(prop = survey_mean())
#'
#' # Can also specify the .groupings manually, though this interface
#' # is a little ugly, as it requires passing a list of quosures or
#' # symbols you've created, rather than the usual syntax
#' dstrata %>%
#'   cascade(
#'     prop = survey_mean(),
#'     .groupings = list(rlang::quos(stype, awards), rlang::quos(NULL))
#'   )
#'
#' @export
cascade <- function(
  .data, ..., .fill = NA, .fill_level_top = FALSE, .groupings = NULL
) {
  UseMethod("cascade")
}


#' @export
cascade.tbl_svy <- function(
  .data, ..., .fill = NA, .groupings = NULL, .fill_level_top = FALSE
) {
  dots <- rlang::quos(...)
  if (is.null(.groupings)) .groupings <- determine_cascade_groupings(.data)

  out <- lapply(.groupings, function(ggg) {
      summarise(group_by(.data, !!!ggg), !!!dots)
  })

  out <- fill_cascade_parts(out, .fill, .fill_level_top)
  out <- dplyr::bind_rows(out)
  # arrange columns in the order they appear in final dataset
  out <- dplyr::arrange(out, dplyr::across(names(out)))
  out
}

# Helper to add missing variables with the fill and handle factor
# levels for these changed variables
fill_cascade_parts <- function(data_list, .fill, .fill_level_top) {
  if (is.function(.fill)) {
    fill_func <- .fill
  } else if (rlang::is_formula(.fill)) {
    fill_func <- rlang::as_function(.fill)
  } else {
    fill_func <- function(.x) return(.fill)
  }


  vname_types <- lapply(data_list, function(x) {
    dplyr::tibble(
      vname = names(x),
      factor = vapply(x, is.factor, logical(1)),
      ordered = vapply(x, is.ordered, logical(1)),
      levels = lapply(x, function(var) levels(var))
    )
  })
  vname_types <- dplyr::bind_rows(vname_types)
  # Potentially inconsistent behavior when different cascades have different
  # variable types, but this seems rare enough to ignore
  vname_types <- dplyr::distinct(vname_types, .data$vname, .keep_all = TRUE)

  # Add missing columns to the parts of the cascade that are missing them
  names_to_add <- lapply(data_list, function(x) setdiff(vname_types$vname, names(x)))
  out <- lapply(seq_along(data_list), function(iii) {
    dplyr::mutate(
      data_list[[iii]],
      !!!stats::setNames(lapply(names_to_add[[iii]], fill_func), names_to_add[[iii]])
    )
  })

  # Modify factors (and ordered) to have new levels if they were ever changed
  added_factors <- intersect(
    unique(unlist(names_to_add)),
    unique(vname_types[vname_types$factor | vname_types$ordered, ][["vname"]])
  )
  out <- lapply(out, function(x) {
    dplyr::mutate(
      x, dplyr::across(
        dplyr::any_of(added_factors),
        function(var) {
          cvar_info <- vname_types[vname_types$vname == dplyr::cur_column(), ]
          fill_val <- fill_func(dplyr::cur_column())
          if (.fill_level_top) {
            new_levels <-  c(fill_val, cvar_info$levels[[1]])
          } else {
            new_levels <-  c(cvar_info$levels[[1]], fill_val)
          }
          factor(var, levels = new_levels, ordered = cvar_info$ordered)
        }
      )
    )
  })

  out
}

# Figure out which groupings to use in cascade from a grouped survey's groups
determine_cascade_groupings <- function(tbl) {
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
      utils::combn(terms, num, simplify = FALSE, function(term_syms) {
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
