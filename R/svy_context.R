# Functions modelled on the tidyselect functions for telling functions within
# summarize what the survey is. I think they are a little overly complex - I
# can't really imagine having one already set and and setting another, but just
# in case, I keep that code.

cur_svy_env <- rlang::child_env(NULL)

set_current_svy <- function(x) {
  stopifnot(inherits(x, "tbl_svy") || is_null(x))

  old <- cur_svy_env$selected
  cur_svy_env$selected <- x

  invisible(old)
}

#' Get the survey data for the current context
#'
#' This is a helper to allow srvyr's syntactic style. In particular, it tells
#' functions inside of a summarize call what survey to use. In general, users
#' will not have to worry about getting (or setting) the current conext's survey,
#' unless they are trying to extend srvyr. See \code{vignette("extending-srvyr")}
#' for more details.
#'
#' @return a tbl_svy (or error if called with no survey context)
#' @export
current_svy <- function() {
  cur_svy_env$selected %||% rlang::abort("Survey context not set")
}
