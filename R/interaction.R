#' Create interaction terms to group by when summarizing
#'
#' Allows multiple grouping by multiple variables as if they were a single
#' variable, which allows calculating proportions that sum to 100% across
#' more than a single grouping variable with \code{survey_mean}.
#'
#' Behind the scenes, this function creates a special column type that is
#' split back into the component columns automatically by \code{summarize}.
#'
#' @param ... variables to group by. All types of tbls accept variable names, and
#' most will also accept functions of variables (though some database-backed
#' tbls do not allow creating variables).
#'
#' @return A vector of type \code{\link{srvyr_interaction}}, which is generally
#' expected to be automatically split apart.
#' @export
#'
#' @examples
#' data(api, package = "survey")
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' # The sum of the whole prop column is equal to 100%
#' dstrata %>%
#'   group_by(interact(stype, awards)) %>%
#'   summarize(prop = survey_mean())
#'
#' # But if you didn't interact, the sum of each stype's prop is 100%
#' dstrata %>%
#'   group_by(stype, awards) %>%
#'   summarize(prop = survey_mean())
interact <- function(...) {
  # Capture dots
  cols <- rlang::enquos(..., .named = TRUE)

  # TODO: Should duplicated groups be silently dropped?
  # (this is what srvyr::group_by says it does)

  # Get df of unique combinations seen in data and add an id
  crosswalk <- tidyr::nesting(!!!cols)
  crosswalk[["___srvyr_cw_id"]] <- seq_len(nrow(crosswalk))

  # Get encoding of original data
  x <- dplyr::left_join(
    dplyr::tibble(!!!cols), crosswalk, by = names(cols)
  )[["___srvyr_cw_id"]]

  # And create the interaction vector
  new_interaction(x, crosswalk)
}

#' Break interaction vectors back into component columns
#'
#' This function will not generally be needed by users because \code{summarise}
#' automatically un-interacts interaction columns for you.
#'
#' @param x Either a \code{srvyr_interaction} column or a \code{data.frame}
#'
#' @return A \code{data.frame}
#' @export
uninteract <- function(x) {
  UseMethod("uninteract")
}

#' @export
#' @rdname uninteract
uninteract.srvyr_interaction <- function(x) {
  crosswalk <- crosswalk(x)
  out <- crosswalk[match(vctrs::vec_data(x), crosswalk[["___srvyr_cw_id"]]), ]
  select(out, -dplyr::one_of("___srvyr_cw_id"))
}

#' @export
#' @rdname uninteract
#' @importFrom tidyselect all_of
uninteract.data.frame <- function(x) {
  # Helper to expand out varnames in correct order
  interaction_varname_expander <- function(vnames, x) {
    out <- lapply(vnames, function(vname) {
      if (!is.interaction(x[[vname]])) {
        vname
      } else {
        setdiff(names(crosswalk(x[[vname]])), "___srvyr_cw_id")
      }
    })
    # unique to do something reasonable when there are duplicated columns
    unique(unlist(out))
  }

  new_col_order <- interaction_varname_expander(names(x), x)
  new_grps <- interaction_varname_expander(group_vars(x), x)
  is_rowwise <- inherits(x, "rowwise_df")

  interaction_vars <- names(x)[vapply(x, is.interaction, logical(1))]
  out <- dplyr::ungroup(x)
  out <- dplyr::mutate(out, dplyr::across(all_of(interaction_vars), uninteract))
  # minimal repair so we don't choke on duplicates (which are taken care of by select)
  out <- tidyr::unpack(out, all_of(interaction_vars), names_repair = "minimal")
  dup_names <- duplicated(names(out))
  if (any(dup_names)) {
    warning(paste0(
      "Duplicate names found (",
      paste0(names(out)[dup_names], collapse = ", "),
      ") keeping first instance."
    ))
    out <- out[, !dup_names]
  }
  out <- dplyr::select(out, dplyr::all_of(new_col_order))

  # restore grouping/rowwise
  if (length(new_grps) > 0 & !is_rowwise) {
    out <- group_by(out, !!!rlang::syms(new_grps))
  } else if (length(new_grps) > 0 & is_rowwise) {
    out <- dplyr::rowwise(out, !!!rlang::syms(new_grps))
  } else if (is_rowwise) {
    out <- dplyr::rowwise(out)
  }

  out
}


# Get the names of the terms used in the crosswalk
interact_terms <- function(col) {
  setdiff(names(crosswalk(col)), "___srvyr_cw_id")
}

# Starting from an interaction, change it so that it has a subset of the terms
# (used during cascade)
recast_interact <- function(col, ...) {
  .dots <- rlang::quos(...)
  old_crosswalk <- crosswalk(col)

  conversion <- dplyr::select(old_crosswalk, !!!.dots, "old_cw" = dplyr::one_of("___srvyr_cw_id"))
  conversion <- dplyr::group_by(conversion, !!!.dots)
  conversion[["___srvyr_cw_id"]] <- group_indices(conversion)
  conversion <- ungroup(conversion)

  new_crosswalk <- dplyr::select(conversion, -dplyr::one_of("old_cw"))
  new_crosswalk <- dplyr::distinct(new_crosswalk)

  new_interaction(
    conversion[["___srvyr_cw_id"]][match(unclass(col), conversion$old_cw)],
    crosswalk = new_crosswalk
  )
}


new_interaction <- function(x = integer(), crosswalk = NULL, ...) {
  vctrs::new_vctr(
    x,
    crosswalk = crosswalk,
    ...,
    class = "srvyr_interaction"
  )
}


#' srvyr interaction column
#'
#' \code{srvyr_interaction} columns help calculate proportions of the interaction of 2
#' or more variables. They are created by \code{\link{interact}}, generally
#' used as grouping variables in \code{\link{group_by}} and then automatically split
#' apart by \code{\link{summarise}}.
#'
#' @name srvyr_interaction
NULL

# TODO: Formatting? (may not be necessary because user is unlikely to ever see it)

# Type system -------------------------------------------------------------

# Import to avoid R CMD check NOTE
#' @importFrom methods setOldClass
setOldClass(c("srvyr_interaction", "vctrs_vctr"))

#' @export
#' @rdname uninteract
is.interaction <- function(x) inherits(x, "srvyr_interaction")

crosswalk <- function(x) attr(x, "crosswalk")

# --- Casting

#' @export
vec_ptype2.srvyr_interaction.srvyr_interaction <- function(x, y, ..., x_arg = "", y_arg = "") {
  x_cw <- crosswalk(x)
  y_cw <- crosswalk(y)

  cw_names <- crosswalk_col_names(x_cw, y_cw, x_arg = x_arg, y_arg = y_arg)

  out_cw <- dplyr::bind_rows(
    x_cw[cw_names],
    dplyr::anti_join(y_cw[cw_names], x_cw[cw_names], by = cw_names)
  )
  out_cw <- dplyr::arrange(out_cw, dplyr::across(all_of(cw_names)))
  out_cw[["___srvyr_cw_id"]] <- seq_len(nrow(out_cw))

  new_interaction(crosswalk = out_cw)
}

#' @export
vec_cast.srvyr_interaction.srvyr_interaction <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  x_cw <- crosswalk(x)
  to_cw <- crosswalk(to)

  if (identical(x_cw, to_cw)) {
    return(x)
  }

  cw_names <- crosswalk_col_names(x_cw, to_cw, x_arg = x_arg, y_arg = to_arg)

  crosswalk_cw <- dplyr::left_join(
    x_cw,
    to_cw,
    by = cw_names,
    suffix = c("_old", "_new")
  )

  if (any(is.na(crosswalk_cw[["___srvyr_cw_id_new"]]))) {
    abort(sprintf(
      "Cannot cast `%s` to `%s` because not all of crosswalk values in %s are in %s's crosswalk",
      x_arg, to_arg, x_arg, to_arg
    ))
  }

  new_x <- dplyr::left_join(
    tibble::tibble(`___srvyr_cw_id_old` = vec_data(x)),
    crosswalk_cw,
    by = "___srvyr_cw_id_old"
  )[["___srvyr_cw_id_new"]]

  new_interaction(new_x, to_cw)
}

crosswalk_col_names <- function(x_cw, y_cw, x_arg = "x", y_arg = "y") {
  # Check if names are compatible
  if (!dplyr::setequal(names(x_cw), names(y_cw))) {
    x_error_names <- paste0("'", setdiff(names(x_cw), "___srvyr_cw_id"), "'", collapse = ", ")
    y_error_names <- paste0("'", setdiff(names(y_cw), "___srvyr_cw_id"), "'", collapse = ", ")
    abort(sprintf(
      "Cannot cast `%s` to `%s` because interacted column names don't match:\n- %s: %s\n- %s: %s",
      x_arg, y_arg, x_arg, x_error_names, y_arg, y_error_names
    ))
  }

  # Check if types are compatible
  common_ptypes <- lapply(names(x_cw), function(nm) {
    attempt <- try(vctrs::vec_ptype_common(x_cw[[nm]], y_cw[[nm]]), silent = TRUE)
    if (inherits(attempt, "try-error")) return(nm) else return(NULL)
  })
  failed_ptypes <- Filter(is.character, common_ptypes)
  if (length(failed_ptypes) > 0) {
    abort(sprintf(
      "Crosswalk columns for %s and %s are incompatible:\n %s",
      x_arg, y_arg, paste0("'", failed_ptypes, "'", collapse = ", ")
    ))
  }

  # Return the common column names
  setdiff(names(x_cw), "___srvyr_cw_id")
}

