#' @export
#' @importFrom dplyr group_map
dplyr::group_map

#' @export
#' @importFrom dplyr group_modify
dplyr::group_modify

#' @export
#' @importFrom dplyr group_walk
dplyr::group_walk

#' Apply a function to each group
#'
#' \code{group_map()}, \code{group_walk} and \code{group_map_dfr} are purrr-style
#' functions that can be used to iterate on grouped survey objects (note that
#' \code{group_map_dfr} replaces \code{dplyr::group_modify} because we are changing
#' the data from a \code{tbl_svy} to a regular tibble).
#'
#' @param .data A \code{tbl_svy object}
#' @param .f A function or purrr-style formula to apply to each group
#' @param ... Other arguments passed to \code{.f}
#' @param .keep Whether the grouping variables are kept when passed into \code{.f}
#'
#' @return For \code{group_map} a list, for \code{group_map_dfr} a `tbl_df`, and for
#'   \code{group_walk} invisibly the original \code{tbl_svy}.
#' @export
#'
#' @examples
#' data(api, package = "survey")
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' results <- dstrata %>%
#'   group_by(both) %>%
#'   group_map(~survey::svyglm(api00~api99 + stype, .))
#'
#' # group_map_dfr calls `bind_rows` on the list returned and includes
#' # grouping variables. This is most useful with a package like `broom`
#' # but could also be used with survey package functions.
#' result_coef <- dstrata %>%
#'   group_by(both) %>%
#'   group_map_dfr(
#'     ~data.frame(
#'       api99_coef = coef(survey::svyglm(api00~api99 + stype, .))[["api99"]]
#'     )
#'   )
#'
group_map_dfr <- function(.data, .f, ..., .keep = FALSE) {
  UseMethod("group_map_dfr", .data)
}

#' @export
#' @rdname group_map_dfr
group_map.tbl_svy <- function(.data, .f, ..., .keep = FALSE) {
  .f <- as_group_map_function(.f)

  # call the function on each group
  chunks <- if (inherits(.data, "grouped_svy")) {
    group_split(.data, .keep = isTRUE(.keep))
  } else {
    group_split(.data)
  }
  keys  <- group_keys(.data)
  group_keys <- faux_map(seq_len(nrow(keys)), function(i) keys[i, , drop = FALSE])

  if (length(chunks)) {
    faux_map2(chunks, group_keys, .f, ...)
  } else {
    # calling .f with .x and .y set to prototypes
    structure(list(), ptype = .f(attr(chunks, "ptype"), keys[integer(0L), ], ...))
  }
}

#' @export
#' @rdname group_map_dfr
group_map_dfr <- function(.data, .f, ..., .keep = FALSE) {
  out <- group_keys(.data)
  out[["__srvyr__data__"]] <- group_map(.data, .f, ..., .keep = .keep)
  tidyr::unnest(out, .data[["__srvyr__data__"]])
}

#' @export
group_modify.tbl_svy <- function(.data, .f, ..., .keep = FALSE) {
  stop("`group_modify()` not implemented for srvyr objects, use `group_map_dfr()`")
}

# Copied from dplyr
faux_map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}

faux_map2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    rlang::set_names(out, names(.x))
  }
  else {
    rlang::set_names(out, NULL)
  }
}

as_group_map_function <- function(.f) {
  .f <- rlang::as_function(.f)
  if (length(form <- formals(.f)) < 2 && !"..." %in% names(form)) {
    stop("The function must accept at least two arguments. You can use ... to absorb unused components")
  }
  .f
}
