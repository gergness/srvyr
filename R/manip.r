#' @export
mutate_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = FALSE)

  if (any(names2(dots) %in% as.character(survey_vars(.data)))) {
    stop("Cannot modify survey variable")
  }

  .data$variables <- mutate_(.data$variables, .dots = dots)
  .data
}

#' @export
select_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::select_vars_(names(.data$variables), dots,
                       include = attr(.data, "group_vars"))

  .data$variables <- select_(.data$variables, .dots = vars)

  # Also rename survey_vars, group_vars, and the structures in the
  # svydesign2 object
  .data <- rename_special_vars(.data, vars)
  .data
}

#' @export
rename_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::rename_vars_(names(.data$variables), dots)
  .data$variables <- rename_(.data$variables, .dots = vars)

  # Also rename survey_vars, group_vars, and the structures in the
  # svydesign2 object
  .data <- rename_special_vars(.data, vars)

  .data
}

#' @export
filter_.tbl_svy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  # There's probably a better way to do this... But I need to use
  # survey::subset because I want to make sure that I recalculate the
  # survey_vars correctly. Create a variable with the row numbers, run dplyr
  # on the variables data.frame and then pass the row_numbers that are kept
  # into survey::svydesign2 `[`
  row_numbers <- .data$variables
  total_num_rows <- nrow(row_numbers)
  row_numbers <-
    dplyr::mutate(row_numbers, `___row_nums` = seq(1, total_num_rows))
  row_numbers <- dplyr::filter_(row_numbers, .dots = dots)
  row_numbers <- row_numbers$`___row_nums`

  .data[row_numbers, ]
}

# Helper to rename variables stored in survey_vars and svydesign2 stucture
rename_special_vars <- function(svy, var_list) {
  renamed_vars <- var_list[var_list != names(var_list)]
  svars <- as.character(survey_vars(svy))

  for (iii in seq_along(renamed_vars)) {
    this_var <- renamed_vars[iii]

    # Make changes in the survey_vars structure
    svars <- lapply(svars, function(x) {
      x[x == this_var] <- names(this_var)
      x
    })

    # Make changes in actual sydesign2 object's structures
    names(svy$cluster)[names(svy$cluster) == this_var] <- names(this_var)
    if(svy$has.strata) {
      names(svy$strata)[names(svy$strata) == this_var] <- names(this_var)
    }
    names(svy$allprob)[names(svy$allprob) == this_var] <- names(this_var)
    attr(svy$fpc$popsize, "dimnames")[[2]][
      attr(svy$fpc$popsize, "dimnames")[[2]] == this_var
      ] <- names(this_var)
  }

  survey_vars(svy) <- svars
  svy
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
#' See \code{\link[dplyr]{mutate_each}} for more details.
#'
#' @name mutate_each
#' @export
#' @importFrom dplyr mutate_each
NULL

#' @name mutate_each_
#' @export
#' @importFrom dplyr mutate_each_
#' @rdname mutate_each
NULL

#' @name summarise_each
#' @export
#' @importFrom dplyr summarise_each
#' @rdname mutate_each
NULL

#' @name summarise_each_
#' @export
#' @importFrom dplyr summarise_each_
#' @rdname mutate_each
?NULL

#' @name summarize_each
#' @export
#' @importFrom dplyr summarize_each
#' @rdname mutate_each
NULL

#' @name summarize_each_
#' @export
#' @importFrom dplyr summarize_each_
#' @rdname mutate_each
NULL

#' @name funs
#' @export
#' @importFrom dplyr funs
#' @rdname mutate_each
NULL


#' @name funs_
#' @export
#' @importFrom dplyr funs_
#' @rdname mutate_each
NULL
