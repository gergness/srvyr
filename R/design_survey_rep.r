#' Create a tbl_svy survey object using replicate weights
#'
#' A wrapper around \code{\link[survey]{svrepdesign}}. All survey variables must be included
#' in the data.frame itself. Select variables by using bare column names, or convenience
#' functions described in \code{\link[dplyr]{select}}. \code{design_survey_rep_} is the
#' standard evaluation counterpart to \code{design_survey_rep}
#'
#' @export
#' @param .data A data frame (which contains the variables specified below)
#' @param variables Variables to include in the design (default is all)
#' @param repweights Variables specifying the replication weight varibles
#' @param weights Variables specifying sampling weights
#' @param type Type of replication weights
#' @param combined_weights \code{TRUE} if the \code{repweights} already
#' include the sampling weights. This is usually the case.
#' @param rho Shrinkage factor fo rweights in Fay's method
#' @param bootstrap_average For \code{type = "bootstrap"}, if the bootstrap
#' weights have been averaged, gives the number of iterations averaged over.
#' @param scale,rscales Scaling constant for variance, see
#' \code{\link[survey]{svrepdesign}} for more information.
#' @param fpc,fpctype Finite population correction information
#' @param mse if \code{TRUE}, compute varainces based on sum of squares
#' around the point estimate, rather than the mean of the replicates
#' @return An object of class \code{tbl_svy}
#' @examples
#' # Examples from ?survey::svrepdesign()
#' library(survey)
#' data(scd)
#' # use BRR replicate weights from Levy and Lemeshow
#' scd$rep1 <- 2 * c(1, 0, 1, 0, 1, 0)
#' scd$rep2 <- 2 * c(1, 0, 0, 1, 0, 1)
#' scd$rep3 <- 2 * c(0, 1, 1, 0, 0, 1)
#' scd$rep4 <- 2 * c(0, 1, 0, 1, 1, 0)
#'
#' scdrep <- scd %>%
#'   design_survey_rep(type = "BRR", repweights = starts_with("rep"),
#'                     combined_weights = FALSE)
#'
#' svyratio(~alive, ~arrests, scdrep)
#'
design_survey_rep <-
  function(.data, variables = NULL, repweights = NULL, weights = NULL,
           type = c("BRR", "Fay", "JK1", "JKn", "bootstrap",
                    "other"), combined_weights = TRUE,
           rho = NULL, bootstrap_average = NULL, scale = NULL,
           rscales = NULL, fpc = NULL, fpctype = c("fraction", "correction"),
           mse = getOption("survey.replicates.mse")) {
  # Need to turn bare variable to variable names, NSE makes looping difficult
  helper <- function(x) unname(dplyr::select_vars_(names(.data), x))
  if (!missing(variables)) variables <- helper(lazy_parent(variables))
  if (!missing(repweights)) repweights <- helper(lazy_parent(repweights))
  if (!missing(weights)) weights <- helper(lazy_parent(weights))
  if (!missing(fpc)) fpc <- helper(lazy_parent(fpc))

  design_survey_rep_(.data, variables, repweights,
                     weights, type, combined_weights,
                     rho, bootstrap_average, scale, rscales,
                     fpc, fpctype, mse)
}


#' @export
#' @rdname design_survey_rep
design_survey_rep_ <-
  function(.data, variables = NULL, repweights = NULL, weights = NULL,
           type = c("BRR", "Fay", "JK1", "JKn", "bootstrap",
                    "other"), combined_weights = TRUE,
           rho = NULL, bootstrap_average = NULL, scale = NULL,
           rscales = NULL, fpc = NULL, fpctype = c("fraction", "correction"),
           mse = getOption("survey.replicates.mse")) {


  # Need to convert to data.frame to appease survey package and also not
  # send NULL to dplyr::select
  survey_selector <- function(x) {
    if (!is.null(x)) data.frame(dplyr::select_(.data, .dots = x)) else NULL
  }
  out <- survey::svrepdesign(
    data = .data,
    variables = survey_selector(variables),
    repweights = survey_selector(repweights),
    weights = nullable(as.matrix, survey_selector(weights)[[1]]),
    type = match.arg(type),
    combined.weights = combined_weights,
    rho = rho,
    bootstrap.average = bootstrap_average,
    scale = scale,
    rscales = rscales,
    fpc = survey_selector(fpc),
    fpctype = fpctype,
    mse = mse)

  class(out) <- c("tbl_svy", class(out))
  out$variables <- dplyr::tbl_df(out$variables)

  # Make a list of names that have the survey vars.
  survey_vars(out) <- list(repweights = repweights,  weights = weights,
                           fpc = fpc)

  # To make printing better, change call
  out$call <- "called via srvyr"
  out
}
