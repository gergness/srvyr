set_survey_vars <- function(.svy, x, name = "SRVYR_TEMP_VAR", add = FALSE) {
  out <- .svy
  if (!add) {
    out$variables <- dplyr::data_frame(!!name := x)
  } else {
    out$variables[[name]] <- x
  }
  out
}


