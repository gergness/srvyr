#' @export
mutate_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  if (any(names2(dots) %in% as.character(survey_vars(.data)))) {
    stop("Cannot modify survey variable")
  }

  .data$variables <- mutate_(.data$variables, .dots = dots)
  .data
}

#' @export
select_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::select_vars_(dplyr::tbl_vars(.data$variables), dots,
                       include = c(attr(.data, "group_vars"),
                                   attr(.data$variables, "order_var")))

  .data$variables <- select_(.data$variables, .dots = vars)

  .data
}

#' @export
rename_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::rename_vars_(dplyr::tbl_vars(.data$variables), dots)
  .data$variables <- rename_(.data$variables, .dots = vars)

  .data
}

#' @export
filter_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  subset_svy_vars(.data, dots)
}


# Import + export generics from dplyr
#' Single table verbs from dplyr
#'
#' These are data manipulation functions designed to work on \code{tbl_svy} objects.
#'
#' \code{mutate} and \code{transmute} can add or modify variables. See
#' \code{\link[dplyr]{mutate}} for more details.
#'
#' \code{select} and \code{rename} keep or rename variables. See
#' \code{\link[dplyr]{select}} for more details.
#'
#' \code{filter} keeps certain observaions. See \code{\link[dplyr]{filter}}
#' for more details.
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

#' @name mutate_
#' @export
#' @importFrom dplyr mutate_
#' @rdname dplyr_single
NULL

#' @name transmute
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr transmute
NULL

#' @name transmute_
#' @export
#' @importFrom dplyr transmute_
#' @rdname dplyr_single
NULL

#' @name select
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr select
NULL

#' @name select_
#' @export
#' @importFrom dplyr select_
#' @rdname dplyr_single
NULL

#' @name rename
#' @rdname dplyr_single
#' @export
#' @importFrom dplyr rename
NULL

#' @name rename_
#' @export
#' @importFrom dplyr rename_
#' @rdname dplyr_single
NULL

#' @name filter
#' @export
#' @importFrom dplyr filter
#' @rdname dplyr_single
NULL

#' @name filter_
#' @export
#' @importFrom dplyr filter_
#' @rdname dplyr_single
NULL


#' Summarise and mutate multiple columns.
#'
#' See \code{\link[dplyr]{summarize_all}} for more details. *_each functions will be depracated
#' in favor of *_all/*_if/*_at functions, but the latter do not yet work on database backed tables.
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

#' @name mutate_each
#' @export
#' @importFrom dplyr mutate_each
#' @rdname summarise_all
NULL

#' @name mutate_each_
#' @export
#' @importFrom dplyr mutate_each_
#' @rdname summarise_all
NULL

#' @name summarise_each
#' @export
#' @importFrom dplyr summarise_each
#' @rdname summarise_all
NULL

#' @name summarise_each_
#' @export
#' @importFrom dplyr summarise_each_
#' @rdname summarise_all
?NULL

#' @name summarize_each
#' @export
#' @importFrom dplyr summarize_each
#' @rdname summarise_all
NULL

#' @name summarize_each_
#' @export
#' @importFrom dplyr summarize_each_
#' @rdname summarise_all
NULL

#' @name funs
#' @export
#' @importFrom dplyr funs
#' @rdname summarise_all
NULL


#' @name funs_
#' @export
#' @importFrom dplyr funs_
#' @rdname summarise_all
NULL
