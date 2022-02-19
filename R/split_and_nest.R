# group_split ----
#' @export
#' @importFrom dplyr group_split
dplyr::group_split

#' @export
group_split.tbl_svy <- function(.tbl, ..., .keep = TRUE) {
  data <- group_by(.tbl, ...)
  group_split_impl(data, .keep = .keep)
}

#' @export
group_split.grouped_svy <- function(.tbl, ..., .keep = TRUE) {
  if (rlang::dots_n(...)) {
    warn(paste0(
      "... is ignored in group_split(<grouped_svy>), please use",
      "group_by(..., .add = TRUE) %>% group_split()"
    ))
  }

  group_split_impl(.tbl, .keep = .keep)
}

group_split_impl <- function(data, .keep) {
  out <- ungroup(data)
  indices <- group_rows(data)

  if (!isTRUE(.keep)) {
    out <- select(out, !dplyr::one_of(group_vars(data)))
  }

  # twophase has bug where you can't subset by 1:nrow,
  # and we might as well just not do the subsetting on anything
  if (length(indices) == 1) {
    return(list(out))
  }

  lapply(indices, function(ind) out[ind, ])
}


# group_nest ----
#' @export
#' @importFrom dplyr group_nest
dplyr::group_nest

#' @export
group_nest.tbl_svy <- function(.tbl, ..., .key = "data", keep = FALSE) {
  if (rlang::dots_n(...)) {
    group_nest_impl(group_by(.tbl, ...), .key = .key, keep = keep)
  }
  else {
    dplyr::tibble(`:=`(!!.key, list(.tbl)))
  }
}

#' @export
group_nest.grouped_svy <- function(.tbl, ..., .key = "data", keep = FALSE) {
  if (rlang::dots_n(...)) {
    warn(paste0(
      "... is ignored in group_nest(<grouped_svy>), please use ",
      "group_by(..., .add = TRUE) %>% group_nest()"
    ))
  }
  group_nest_impl(.tbl, .key = .key, keep = keep)
}

group_nest_impl <- function(.tbl, .key, keep = FALSE) {
  mutate(group_keys(.tbl), !!.key := group_split(.tbl, .keep = keep))
}


# nest_by ----
#' @export
#' @importFrom dplyr nest_by
dplyr::nest_by

#' @export
nest_by.tbl_svy <- function(.data, ..., .key = "data", .keep = FALSE) {
  .data <- group_by(.data, ...)
  nest_by(.data, .key = .key, .keep = .keep)
}

#' @export
nest_by.grouped_svy <- function(.data, ..., .key = "data", .keep = FALSE) {
  if (!missing(...)) {
    rlang::abort(c(
      "Can't re-group while nesting",
      i = "Either `ungroup()` first or don't supply arguments to `nest_by()"
    ))
  }
  vars <- group_vars(.data)
  keys <- group_keys(.data)
  keys <- mutate(keys, `:=`(!!.key, group_split(.env$.data, .keep = .keep)))
  dplyr::rowwise(keys, tidyselect::all_of(vars))
}
