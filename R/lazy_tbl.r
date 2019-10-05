get_lazy_vars <- function(data, id, ...) {
  id <- enquo(id)
  dots <- quos(...)

  if (rlang::is_formula(check_for_id_quo(id, dplyr::tbl_vars(data)))) {
    id <- quo(NULL)
  }


  # Can't figure out a return value that will work as an argument to select
  # but return nothing, so I need to remove NULLs when they exist
  dots <- dots[!vapply(dots, rlang::quo_is_null, logical(1))]

  if (rlang::quo_is_null(id)) {
    query <- dplyr::select(data, !!!dots)
  } else {
    query <- dplyr::select(data, !!id, !!!dots)
  }

  dplyr::collect(query)
}

lazy_subset_svy_vars <- function(svy, ...) {
  dots <- rlang::quos(...)
  if (!"__SRVYR_SUBSET_VAR__" %in% tbl_vars(svy$variables)) {
    svy$variables <- dplyr::mutate(svy$variables, `__SRVYR_SUBSET_VAR__` = TRUE)
  }

  svy$variables <- dplyr::mutate(
    svy$variables,
    `__SRVYR_SUBSET_VAR__` = !!!dots & !!rlang::sym("__SRVYR_SUBSET_VAR__")
  )

  svy
}

is_lazy_svy <- function(x) {
  inherits(x, "tbl_lazy_svy")
}

# Called from summarize, need to subset based on filters and also
# collect all relevant variables from dots
localize_lazy_svy <- function(svy, dots = NULL) {
  if (!is.null(dots)) {
    vars_to_collect <- find_vars_to_collect_in_dots(svy$variables, dots)
  } else {
    vars_to_collect <- tbl_vars(svy$variables)
  }
  needs_subset <- "__SRVYR_SUBSET_VAR__" %in% dplyr::tbl_vars(svy$variables)
  if (needs_subset) {
    vars_to_collect <- c(vars_to_collect, "__SRVYR_SUBSET_VAR__")
  }

  svy$variables <- dplyr::collect(
    dplyr::select(svy$variables, !!!rlang::syms(vars_to_collect))
  )

  if (needs_subset) {
    svy <- subset_svy_vars(svy, as.logical(svy$variables$`__SRVYR_SUBSET_VAR__`))
  }
  svy
}

# This is a hacky way to find what variables will be
# needed to calculate the results in a summarize.
# It definitely won't get it right 100% of the time
# but most of the problems I can think of will only
# cause extra variables to be collected from the
# database, which doesn't seem like the end of the
# world...
find_vars_to_collect_in_dots <- function(data, dots) {
  # All dots must be a function for srvyr summarize, so we can
  # go down to each dot's expression arguments
  all_args <- lapply(dots, function(x) rlang::call_args(x))
  all_args <- rlang::squash(unname(all_args))

  used_vars <- lapply(all_args, find_vars_to_collect, var_names = dplyr::tbl_vars(data))
  used_vars <- rlang::flatten_chr(used_vars)
  unique(used_vars)
}

find_vars_to_collect <- function(var_names, expr) {
  all_expr_text <- vapply(expr, rlang::expr_text, "")
  var_names[var_names %in% all_expr_text]
}

#' Force computation of a database query
#'
#' \code{collect} retrieves data from a database query (and when run
#' on a tbl_svy object adjusts weights accordingly). Use collect when
#' you want to run a function from the survey package on a srvyr db
#' backed object. \code{compute} stores results in a remote temporary
#' table.
#' @export
#' @name collect
#' @importFrom dplyr collect
NULL

#' @name compute
#' @export
#' @importFrom dplyr compute
#' @rdname collect
NULL

#' @export
compute.tbl_lazy_svy <- function(x, name = dplyr::random_table_name(), ...) {
  x$variables <- compute(x$variables, name = name, ...)
  x
}

#' @export
collect.tbl_lazy_svy <- function(x, ...) {
  localize_lazy_svy(x)
}

capture_survey_db_updates <- function(svy) {
  for (uuu in svy$updates) {
    for (iii in seq_along(uuu)) {
      update_name <- names(uuu)[iii]
      update_name_sym <- rlang::sym(update_name)
      update_expression <- uuu[[iii]]$expression
      new_vars <- dplyr::mutate(
        svy$variables,
        !!update_name_sym := !!update_expression
      )
      error <- tryCatch(dplyr::collect(utils::head(new_vars, n = 1)), error = function(e) e)
      if (!is.data.frame(error)) {
        warning(paste0(
          "Could not convert variable '", update_name, "' from survey database to srvyr database.\n",
          "  Reason: ", error
        ), call. = FALSE)
      } else {
        svy$variables <- new_vars
      }
    }
  }
  return(svy)
}
