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
names2 <- function (x)
{
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
