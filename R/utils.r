# Import all of rlang so we can use pronouns and compat files
# In general, though, I'm still trying to use namespace declarations
#'@import rlang
NULL


# For functions that use select style syntax, gets the variable names
# from the user's input and data, and then returns a formula that
# can be used in a survey package function. If NULL, return NULL.
#
# Note that the formulas returned by this function  don't have the right
# environment by design. This is because we are only interested
# in names that resolve to variables in a dataframe. See
# https://github.com/hadley/rlang/issues/73 for some details about how
# the env isn't used properly within survey's use of stats::model.frame.
#
# Argument check_ids checks for 0, and 1, which supposed to be
# returned as is for functions like svydesign. Usually they would return
# an error and the first column, respectively (because select works on ints).
# I want to make this programmable, but still return the right thing, so
# the logic is a little complicated:
# 1) If `vars` evaluates to a single column name, it will use the column
# 2) If it is not a column name and it evaluates to 0 or 1 then return ~0/~1
# 3) If it is anything else, use tidyselect::vars_select to find columns
srvyr_select_vars <- function(vars, data, check_ids = FALSE) {
  vars <- vars
  var_names <- dplyr::tbl_vars(data)
  if (check_ids) {
    id_quo_check <- check_for_id_quo(vars, var_names)
    if (rlang::is_formula(id_quo_check)) {
      return(id_quo_check)
    }
  }
  if (is.null(rlang::f_rhs(vars))) return(NULL)

  out_vars <- tidyselect::vars_select(var_names, !!vars)
  survey::make.formula(out_vars)
}

# Retruns FALSE if x is a quo(0) or quo(1), or
# ~0, ~1 if it is.
check_for_id_quo <- function(x, var_names) {
  var_rhs <- as.character(rlang::f_rhs(x))
  if (!(length(var_rhs) == 1 && var_rhs %in% var_names)) {
    var_eval <- try(rlang::eval_tidy(x), silent = TRUE)
    if (identical(var_eval, 0) || identical(var_eval, 1)) {
      return(survey::make.formula(var_eval))
    }
  }
  return(FALSE)
}

# Need to turn bare variable to variable names inside list (for 2phase)
# NULLS are allowed in the list and should be carried forward.
srvyr_select_vars_list <- function(x, .data) {
  if (rlang::quo_is_null(x)) return(NULL)
  rhs <- rlang::f_rhs(x)

  if (rhs[[1]] != "list" || length(rhs) != 3) {
    stop("as_survey_twophase requies a list of 2 sets of variables")
  }
  name1 <- srvyr_select_vars(split_list_quosure(x, 2), .data)
  name1 <- if (length(name1) == 0) NULL else name1
  name2 <- srvyr_select_vars(split_list_quosure(x, 3), .data)
  name2 <- if (length(name2) == 0) NULL else name2
  list(name1, name2)
}

split_list_quosure <- function(q, index) {
  rlang::new_quosure(rlang::f_rhs(q)[[index]], rlang::f_env(q))
}

nullable <- function(f, x, ...) {
  if (is.null(x)) NULL
  else f(x, ...)
}

n_compat_lazy <- function(x) {
  nullable(compat_lazy, x, env = caller_env(2))
}

# From dplyr (utils.r)
"%||%" <- function(x, y) if(is.null(x)) y else x

# From dplyr (utils.r)
names2 <- function (x) {
  names(x) %||% rep("", length(x))
}

# from dplyr (utils-format.r)
wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2,
                     width = getOption("width"))
  paste0(wrapped, collapse = "\n")
}

# from dplyr (utils.r)
dots <- function(...) {
  eval(substitute(alist(...)))
}

# from dplyr (utils.r)
deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}

# From http://stackoverflow.com/questions/7963898
# Not very safe, so only use with known strings
substr_right <- function(x, n){
  x <- as.character(x)
  substr(x, nchar(x)-n+1, nchar(x))
}

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


# survey::twophase doesn't work with values, needs to be formula of
# variable names
# Change list of variable names to formulas
list_to_formula <- function(x) {
  if (!is.null(x)) {
    lapply(x, function(y) nullable(survey::make.formula, y))
  } else NULL
}

# From survey:::is.calibrated()
is.calibrated <- function(design)
{
  !is.null(design$postStrata)
}

# From survey:::is.pps()
is.pps <- function(x) {
  if (is.null(x$pps)) FALSE else (x$pps != FALSE)
}

# From base (for backward compatibility)
isFALSE <- function (x) is.logical(x) && length(x) == 1L && !is.na(x) && !x
