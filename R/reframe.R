#' @export
#' @importFrom dplyr reframe
dplyr::reframe


#' @export
reframe.tbl_svy <- function(.data, ..., .groups = NULL, .unpack = TRUE) {
  .dots <- rlang::quos(...)
  if (is_lazy_svy(.data)) .data <- localize_lazy_svy(.data, .dots)

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(list(full = .data, split = split_for_context(.data)))
  on.exit(set_current_svy(old), add = TRUE)

  out <- dplyr::reframe(.data$variables, ..., .groups = .groups)

  # srvyr predates dplyr's data.frame columns so default to unpacking
  # them wide
  if (.unpack) out <- unpack_cols(out)
  out
}

#' @export
reframe.grouped_svy <- function(.data, ..., .groups = NULL, .unpack = TRUE) {
  .dots <- rlang::quos(...)
  if (is_lazy_svy(.data)) .data <- localize_lazy_svy(.data, .dots)

  # Set current_svy so available to svy stat functions
  old <- set_current_svy(list(full = .data, split = split_for_context(.data)))
  on.exit(set_current_svy(old), add = TRUE)

  out <- dplyr::reframe(.data$variables, !!!.dots, .groups = .groups)

  # Remove interaction variables if present
  out <- uninteract(out)

  # srvyr predates dplyr's data.frame columns so default to unpacking
  # them wide
  if (.unpack) out <- unpack_cols(out)
  out
}
