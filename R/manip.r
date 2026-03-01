#' @export
mutate.tbl_svy <- function(
  .data,
  ...,
  .by = NULL,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL,
  .unpack = TRUE
) {
  dots <- rlang::quos(...)

  if (any(names2(dots) %in% as.character(survey_vars(.data)))) {
    stop("Cannot modify survey variable")
  }

  .by <- rlang::enquos(.by)
  # Can't just pass `.by` to dplyr because we need to calculate survey statistics per group
  if (!all(sapply(.by, rlang::quo_is_null))) {
    .data <- group_by(.data, across(!!!.by))
    return(ungroup(mutate(.data, !!!dots, .keep = .keep, .before = {{.before}}, .after = {{.after}}, .unpack = .unpack)))
  }

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(list(full = .data, split = split_for_context(.data)))
  on.exit(set_current_svy(old), add = TRUE)

  .data$variables <- mutate(
    .data$variables,
    !!!dots,
    .keep = .keep,
    .before = {{.before}},
    .after = {{.after}}
  )

  if (.unpack && !is_lazy_svy(.data)) .data$variables <- unpack_cols(.data$variables)
  .data
}

#' @export
transmute.tbl_svy <- function(.data, ...) {
  mutate(.data, ..., .keep = "none")
}

#' @export
select.tbl_svy <- function(.data, ...) {
  dots <- rlang::quos(...)
  .data$variables <- select(.data$variables, !!!dots)

  .data
}

#' @export
rename.tbl_svy <- function(.data, ...) {
  dots <- rlang::quos(...)
  .data$variables <- rename(.data$variables, !!!dots)

  .data
}

#' @method rename_with tbl_svy
#' @importFrom tidyselect everything
#' @export
rename_with.tbl_svy <- function(.data, .fn, .cols = everything(), ...) {
  .data$variables <- rename_with(.data$variables, .fn = .fn, .cols = {{.cols}}, ...)

  .data
}

#' @export
filter.tbl_svy <- function(.data, ..., .preserve = FALSE) {
  dots <- rlang::quos(...)

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(list(full = .data, split = split_for_context(.data)))
  on.exit(set_current_svy(old), add = TRUE)

  if (is_lazy_svy(.data)) {
    lazy_subset_svy_vars(.data, !!!dots, .preserve = .preserve)
  } else {
    subset_svy_vars(.data, !!!dots, .preserve = .preserve)
  }
}

#' @export
drop_na.tbl_svy <- function(data, ...) {
  vars <- tidyselect::eval_select(expr(c(...)), data$variables)
  if (is_empty(vars)) {
    f <- complete_cases(data$variables)
  }
  else {
    f <- complete_cases(data$variables[vars])
  }
  filter(data, f)
}

# from tidyr:::complete_cases
complete_cases <- function (x, fun) {
  ok <- vapply(x, is_complete, logical(nrow(x)))
  if (is.vector(ok)) {
    all(ok)
  }
  else {
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

# from tidyr:::is_complete
is_complete <- function (x) {
  if (typeof(x) == "list") {
    !vapply(x, rlang::is_empty, logical(1))
  }
  else {
    !is.na(x)
  }
}

# Import + export generics from dplyr and tidyr
#' Single table verbs from dplyr and tidyr
#'
#' These are data manipulation functions designed to work on \code{tbl_svy} objects.
#'
#' \code{mutate} and \code{transmute} can add or modify variables. See
#' \code{\link[dplyr]{mutate}} for more details.
#'
#' \code{select}, \code{rename}, and \code{rename_with} keep or rename variables. See
#' \code{\link[dplyr]{select}} for more details.
#'
#' \code{pull} extracts a variable as a vector (whereas \code{select} returns a \code{tbl_svy}).
#' See \code{\link[dplyr]{pull}} for more details.
#'
#' \code{filter} keeps certain observations. See \code{\link[dplyr]{filter}}
#' for more details.
#'
#' #' \code{drop_na} drops observations containing missing values.
#' See \code{\link[tidyr]{drop_na}} for more details.
#'
#' \code{arrange} is not implemented for \code{tbl_svy} objects. Nor are any
#' two table verbs such as \code{bind_rows}, \code{bind_cols} or any of the
#' joins (\code{full_join}, \code{left_join}, etc.). These data manipulations
#' may require modifications to the survey variable specifications and so
#' cannot be done automatically. Instead, use dplyr to perform them while the
#' data is still stored in data.frames.
#'@name dplyr_single
NULL

#' @name mutate
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr mutate
NULL

#' @name transmute
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr transmute
NULL

#' @name select
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr select
NULL

#' @name pull
#' @export
#' @importFrom dplyr pull
#' @rdname dplyr_single
NULL


#' @name rename
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr rename
NULL

#' @name rename_with
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr rename_with
NULL

#' @name filter
#' @export
#' @importFrom dplyr filter
#' @rdname dplyr_single
NULL

#' @name drop_na
#' @export
#' @importFrom tidyr drop_na
#' @rdname dplyr_single
NULL

#' Manipulate multiple columns.
#'
#' See \code{\link[dplyr]{summarize_all}} for more details. *_each functions will be deprecated
#' in favor of *_all/*_if/*_at functions.
#'
#' @name summarise_all
#' @export
#' @importFrom dplyr summarise_all
NULL

#' @name summarize_all
#' @export
#' @importFrom dplyr summarize_all
#' @rdname summarise_all
NULL

#' @name summarise_if
#' @export
#' @importFrom dplyr summarise_if
#' @rdname summarise_all
NULL

#' @name summarize_if
#' @export
#' @importFrom dplyr summarize_if
#' @rdname summarise_all
NULL

#' @name summarise_at
#' @export
#' @importFrom dplyr summarise_at
#' @rdname summarise_all
NULL

#' @name summarize_at
#' @export
#' @importFrom dplyr summarize_at
#' @rdname summarise_all
NULL

#' @name mutate_all
#' @export
#' @importFrom dplyr mutate_all
#' @rdname summarise_all
NULL

#' @name mutate_if
#' @export
#' @importFrom dplyr mutate_if
#' @rdname summarise_all
NULL

#' @name mutate_at
#' @export
#' @importFrom dplyr mutate_at
#' @rdname summarise_all
NULL

#' @name filter_all
#' @export
#' @importFrom dplyr filter_all
#' @rdname summarise_all
NULL

#' @name filter_at
#' @export
#' @importFrom dplyr filter_at
#' @rdname summarise_all
NULL

#' @name filter_if
#' @export
#' @importFrom dplyr filter_if
#' @rdname summarise_all
NULL

#' @name select_all
#' @export
#' @importFrom dplyr select_all
#' @rdname summarise_all
NULL

#' @name select_at
#' @export
#' @importFrom dplyr select_at
#' @rdname summarise_all
NULL

#' @name select_if
#' @export
#' @importFrom dplyr select_if
#' @rdname summarise_all
NULL

#' @name rename_all
#' @export
#' @importFrom dplyr rename_all
#' @rdname summarise_all
NULL

#' @name rename_at
#' @export
#' @importFrom dplyr rename_at
#' @rdname summarise_all
NULL

#' @name rename_if
#' @export
#' @importFrom dplyr rename_if
#' @rdname summarise_all
NULL

#' @name group_by_all
#' @export
#' @importFrom dplyr group_by_all
#' @rdname summarise_all
NULL

#' @name group_by_at
#' @export
#' @importFrom dplyr group_by_at
#' @rdname summarise_all
NULL

#' @name group_by_if
#' @export
#' @importFrom dplyr group_by_if
#' @rdname summarise_all
NULL

#' @name mutate_each
#' @export
#' @importFrom dplyr mutate_each
#' @rdname summarise_all
NULL

#' @name summarise_each
#' @export
#' @importFrom dplyr summarise_each
#' @rdname summarise_all
NULL

#' @name summarize_each
#' @export
#' @importFrom dplyr summarize_each
#' @rdname summarise_all
NULL

#' @name funs
#' @export
#' @importFrom dplyr funs
#' @rdname summarise_all
NULL

#' @name vars
#' @export
#' @importFrom dplyr vars
#' @rdname summarise_all
NULL

#' @name all_vars
#' @export
#' @importFrom dplyr all_vars
#' @rdname summarise_all
NULL

#' @name any_vars
#' @export
#' @importFrom dplyr any_vars
#' @rdname summarise_all
NULL


#' @export
pull.tbl_svy <- function(.data, var = -1, name = NULL, ...) {
  var <- rlang::enquo(var)
  name <- rlang::enquo(name)
  dplyr::pull(.data$variables, !!var, !!name)
}
