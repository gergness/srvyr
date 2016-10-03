#' Chisquared tests of association for survey data.
#' @param formula Model formula specifying margins for the table (using + only)
#' @param design survey object
#' @param formula See details in \code{\link[survey]{svychisq}}
#' @param design See details in \code{\link[survey]{svychisq}}
#' @param na.rm See details in \code{\link[survey]{svychisq}}
#' @param ... See details in \code{\link[survey]{svychisq}}
#' @name svychisq
#' @export
#' @importFrom survey svychisq
NULL

#' @export
svychisq.tbl_svy <- function(formula, design,
                             statistic = c("F", "Chisq", "Wald",
                                           "adjWald", "lincom",
                                           "saddlepoint"),
                             na.rm = TRUE, ...) {
  if (inherits(design$variables, "tbl_lazy")) {
    stop("survey functions not implemented on database backed srvyr objects")
  }
  design$variables <- as.data.frame(design$variables)
  NextMethod()
}
