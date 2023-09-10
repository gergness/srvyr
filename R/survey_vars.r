# survey_vars are a convenient way of storing the variables
# used to define the survey

# Get the survey vars from a svy_tbl object
survey_vars <- function(svy) {
  attr(svy, "survey_vars")
}

# Set the survey vars of a svy_tbl object
`survey_vars<-` <- function(svy, value) {
  value <- lapply(value, function(x) {
    if (length(x) > 0) {
      lapply(x, function(y) {
        if (is.null(y)) y <- "NULL"
        as.name(y)
      })
    }
  })
  class(value) <- "survey_vars"
  attr(svy, "survey_vars") <- value
  svy
}

#' @export
as.character.survey_vars <- function(x, ...) {
  lapply(x, function(y) as.character(y))
}

#' @export
print.survey_vars <- function(x, all = FALSE, ...) {
  cat("Sampling variables:\n")
  lapply(seq_along(x), function(y) {
    if (!is.null(x[[y]])) {
      if (!is.null(x[[y]])) {
        fixcarriage <- function(x){
          # remove carriage return in repweights and extra space
          out <- gsub("\\n    ", "", x)
          if (is.symbol(x)){
            as.symbol(out)
          } else{
            out
          }
        }
        cat(wrap(paste0(" - ", names(x[y]), ": ", paste(lapply(x[[y]], fixcarriage), collapse=", ")), indent=2), "\n")
      }
    }
  })
  invisible(x)
}

