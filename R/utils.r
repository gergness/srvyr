lazy_parent <- function(expr) {
  # Need to go up twice, because lazy_parent creates an environment for itself
  e1 <- substitute(expr)
  e2 <- do.call("substitute", list(e1), envir = parent.frame(1))

  lazyeval::lazy_(e2, parent.frame(2))
}

nullable <- function(f, x) {
  if (is.null(x)) NULL
  else f(x)
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
named_dots <- function(...) {
  lazyeval::auto_name(dots(...))
}

# from dplyr (utils.r)
deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}

# Need to turn bare variable to variable names inside list (for 2phase)
# NULLS are allowed in the list and should be carried forward.
helper_list <- function(x, .data) {
  x <- x[["expr"]]
  if(x[[1]] != "list" || length(x) > 3) {
    stop("as_survey_twophase requies a list of 2 sets of variables")
  }
  name1 <- unname(dplyr::select_vars_(dplyr::tbl_vars(.data), x[[2]]))
  name1 <- if (length(name1) == 0) NULL else name1
  name2 <- unname(dplyr::select_vars_(dplyr::tbl_vars(.data), x[[3]]))
  name2 <- if (length(name2) == 0) NULL else name2
  list(name1, name2)
}

# Need to turn bare variable to variable names (when not in list)
helper <- function(x, .data) {
  unname(dplyr::select_vars_(dplyr::tbl_vars(.data), x))
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

# Need to convert to data.frame to appease survey package and also not
# send NULL to dplyr::select
survey_selector <- function(.data, x) {
  if (!is.null(x)) {

    if (inherits(.data, "tbl_lazy")) {
      out <- dplyr::select_(.data, .dots = c(attr(.data, "order_var"), x))
      out <- ordered_collect(out)
    } else {
      out <- dplyr::select_(.data, .dots = x)
    }
    out <- data.frame(out)
  } else {
    out <- NULL
  }
  out
}

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
