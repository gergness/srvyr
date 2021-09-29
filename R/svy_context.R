# Functions modeled on the tidyselect functions for telling functions within
# summarize what the survey is. I think they are a little overly complex - I
# can't really imagine having one already set and and setting another, but just
# in case, I keep that code.
#
# As of Sept 2020 tidyselect functions no longer use this idiom, may be
# worth investigating what they do instead at some point
cur_svy_env <- rlang::child_env(NULL)

set_current_svy <- function(x) {
  stopifnot(inherits(x$full, "tbl_svy") || is_null(x$full))
  stopifnot(is.list(x$split) || is_null(x$split))

  old <- list(full = cur_svy_env$full, split = cur_svy_env$split)

  cur_svy_env$full <- x$full
  cur_svy_env$split <- x$split
  cur_svy_env$peel_groups <- NULL

  invisible(old)
}

peeled_cur_group_id <- function(svy, cur_group) {
  # TODO: This is significantly slower than survey package
  # because it performs survey calculation on each group
  # whereas survey can do one for the final peel. Maybe
  # srvyr could store the `svyby` results and use that?
  if (is.null(cur_svy_env$peel_groups)) {
    grp_names <- group_vars(svy)
    # no groups, so just return 1s (so eg cascade will have proportion=1)
    if (length(grp_names) == 0) {
      return(rep(1, nrow(svy)))
    }
    peel <- grp_names[length(grp_names)]

    peel_groups <- group_data(svy)
    peel_groups <- group_by_at(peel_groups, setdiff(grp_names, peel))
    peel_groups <- summarize(
      peel_groups,
      grp_rows = list(unlist(.data[[".rows"]])),
      peel = list(data.frame(peel_name = .data[[peel]], .rows = .data[[".rows"]]))
    )
    cur_svy_env$peel_groups <- peel_groups
  } else {
    peel_groups <- cur_svy_env$peel_groups
  }
  cur_group <- cur_group()

  cur_peel_group <- dplyr::inner_join(
    peel_groups,
    cur_group[, -ncol(cur_group)],
    by = names(cur_group[, -ncol(cur_group)])
  )
  cur_peel_all <- cur_peel_group$grp_rows[[1]]
  # x == y doesn't work for NAs, so use this awkward vapply
  cur_group_pos <- equal_or_both_na(
    cur_peel_group$peel[[1]]$peel_name,
    cur_group[[ncol(cur_group)]]
  )

  cur_peel_sel <- cur_peel_group$peel[[1]]$.rows[[which(cur_group_pos)]]

  out <- rep(NA_integer_, nrow(svy))
  out[cur_peel_all] <- 0L
  out[cur_peel_sel] <- 1L
  out
}


#' Get the survey data for the current context
#'
#' This is a helper to allow srvyr's syntactic style. In particular, it tells
#' functions inside of a summarize call what survey to use (for the current
#' group with \code{cur_svy()} or the complete survey for \code{cur_svy_full()}.
#' In general, users will not have to worry about getting (or setting) the current
#' context's survey, unless they are trying to extend srvyr.
#' See \code{vignette("extending-srvyr")} for more details. \code{current_svy()}
#' is deprecated, but returns the same value as \code{cur_svy()}.
#'
#' @return a tbl_svy (or error if called with no survey context)
#' @export
cur_svy <- function() {
  cur_svy_env$split[[dplyr::cur_group_id()]] %||% rlang::abort("Survey context not set")
}

#' @export
#' @rdname cur_svy
cur_svy_full <- function() {
  cur_svy_env$full %||% rlang::abort("Survey context not set")
}

#' @export
#' @rdname cur_svy
current_svy <- function() {
  warning("`current_svy()` is deprecated, use `cur_svy()` instead")
  cur_svy()
}

#' @export
#' @importFrom dplyr n
dplyr::n

#' @export
#' @importFrom dplyr cur_data
dplyr::cur_data

#' @export
#' @importFrom dplyr cur_group
dplyr::cur_group

#' @export
#' @importFrom dplyr cur_group_id
dplyr::cur_group_id

#' @export
#' @importFrom dplyr cur_column
dplyr::cur_column

#' @export
#' @importFrom dplyr across
dplyr::across

#' @export
#' @importFrom dplyr c_across
dplyr::c_across
