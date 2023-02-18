#' Create a tbl_svy survey object using replicate weights
#'
#' Create a survey object with replicate weights.
#'
#' If provided a data.frame, it is a wrapper around \code{\link[survey]{svrepdesign}}.
#' All survey variables must be included in the data.frame itself. Variables are
#' selected by using bare column names, or convenience functions described in
#' \code{\link[dplyr]{select}}.
#'
#' If provided a \code{svyrep.design} object from the survey package,
#' it will turn it into a srvyr object, so that srvyr functions will work with it
#'
#' If provided a survey design (\code{survey.design2} or \code{tbl_svy}), it is a wrapper
#' around \code{\link[survey]{as.svrepdesign}}, and will convert from a survey design to
#' replicate weights.
#'
#' @export
#' @param .data A data frame (which contains the variables specified below)
#' @param variables Variables to include in the design (default is all)
#' @param repweights Variables specifying the replication weight variables
#' @param weights Variables specifying sampling weights
#' @param type Type of replication weights
#' @param combined_weights \code{TRUE} if the \code{repweights} already
#' include the sampling weights. This is usually the case.
#' @param rho Shrinkage factor for weights in Fay's method
#' @param bootstrap_average For \code{type = "bootstrap"}, if the bootstrap
#' weights have been averaged, gives the number of iterations averaged over.
#' @param scale,rscales Scaling constant for variance, see
#' \code{\link[survey]{svrepdesign}} for more information.
#' @param fpc 	Variables specifying a finite population correction, see
#' \code{\link[survey]{svrepdesign}} for more details.
#' @param fpctype Finite population correction information
#' @param mse if \code{TRUE}, compute variances based on sum of squares
#' around the point estimate, rather than the mean of the replicates
#' @param ... ignored
#' @param compress if \code{TRUE}, store replicate weights in compressed form
#' (if converting from design)
#'
#' @return An object of class \code{tbl_svy}
#' @examples
#' # Examples from ?survey::svrepdesign()
#' library(survey)
#' library(dplyr)
#' data(scd)
#' # use BRR replicate weights from Levy and Lemeshow
#' scd <- scd %>%
#'   mutate(rep1 = 2 * c(1, 0, 1, 0, 1, 0),
#'          rep2 = 2 * c(1, 0, 0, 1, 0, 1),
#'          rep3 = 2 * c(0, 1, 1, 0, 0, 1),
#'          rep4 = 2 * c(0, 1, 0, 1, 1, 0))
#'
#' scdrep <- scd %>%
#'   as_survey_rep(type = "BRR", repweights = starts_with("rep"),
#'                 combined_weights = FALSE)
#'
#' # dplyr 0.7 introduced new style of NSE called quosures
#' # See `vignette("programming", package = "dplyr")` for details
#' repwts <- quo(starts_with("rep"))
#' scdrep <- scd %>%
#'   as_survey_rep(type = "BRR", repweights = !!repwts,
#'                 combined_weights = FALSE)
#'
as_survey_rep <- function(.data, ...) {
  UseMethod("as_survey_rep")
}

#' @export
#' @rdname as_survey_rep
as_survey_rep.data.frame <-
  function(.data, variables = NULL, repweights = NULL, weights = NULL,
           type = c("BRR", "Fay", "JK1", "JKn", "bootstrap",
                    "successive-difference", "ACS",
                    "other"), combined_weights = TRUE,
           rho = NULL, bootstrap_average = NULL, scale = NULL,
           rscales = NULL, fpc = NULL, fpctype = c("fraction", "correction"),
           mse = getOption("survey.replicates.mse"), ...) {
    variables <- srvyr_select_vars(rlang::enquo(variables), .data)
    repweights <- srvyr_select_vars(rlang::enquo(repweights), .data)
    weights <- srvyr_select_vars(rlang::enquo(weights), .data)
    fpc <- srvyr_select_vars(rlang::enquo(fpc), .data)
    type <- if (missing(type)) type[1] else type

    out <- survey::svrepdesign(
      variables = variables,
      repweights = repweights,
      weights = weights,
      data = .data,
      type = type,
      combined.weights = combined_weights,
      rho = rho,
      bootstrap.average = bootstrap_average,
      scale = scale,
      rscales = rscales,
      fpc = fpc,
      fpctype = fpctype,
      mse = mse
    )

    as_tbl_svy(
      out,
      list(repweights = repweights,  weights = weights, fpc = fpc)
    )
  }


#' @export
#' @rdname as_survey_rep
as_survey_rep.tbl_lazy <-
  function(.data, variables = NULL, repweights = NULL, weights = NULL,
           type = c("BRR", "Fay", "JK1", "JKn", "bootstrap",
                    "successive-difference", "ACS",
                    "other"), combined_weights = TRUE,
           rho = NULL, bootstrap_average = NULL, scale = NULL,
           rscales = NULL, fpc = NULL, fpctype = c("fraction", "correction"),
           mse = getOption("survey.replicates.mse"), ...) {

    variables <- rlang::enquo(variables)
    repweights <- rlang::enquo(repweights)
    weights <- rlang::enquo(weights)
    fpc <- rlang::enquo(fpc)

    survey_vars_local <- get_lazy_vars(
      data = .data, !!variables, !!repweights, !!weights, !!fpc
    )

    variables <- srvyr_select_vars(variables, .data)
    repweights <- srvyr_select_vars(repweights, .data)
    weights <- srvyr_select_vars(weights, .data)
    fpc <- srvyr_select_vars(fpc, .data)
    type <- if (missing(type)) type[1] else type

    out <- survey::svrepdesign(
      variables = variables,
      repweights = repweights,
      weights = weights,
      data = survey_vars_local,
      type = type,
      combined.weights = combined_weights,
      rho = rho,
      bootstrap.average = bootstrap_average,
      scale = scale,
      rscales = rscales,
      fpc = fpc,
      fpctype = fpctype,
      mse = mse
    )

    out$variables <- .data

    as_tbl_svy(
      out,
      list(repweights = repweights,  weights = weights, fpc = fpc)
    )
  }


#' @export
#' @rdname as_survey_rep
as_survey_rep.svyrep.design <- function(.data, ...) {
  as_tbl_svy(.data)
}

#' @export
#' @rdname as_survey_rep
as_survey_rep.survey.design2 <-
  function(.data, type=c("auto", "JK1", "JKn", "BRR", "bootstrap",
                         "subbootstrap","mrbbootstrap","Fay"),
           rho = 0, fpc = NULL, fpctype = NULL, ..., compress = TRUE,
           mse=getOption("survey.replicates.mse")) {

    .data <- survey::as.svrepdesign(.data, type = type, fay.rho = rho,
                            fpc = fpc, fpctype = fpctype, ...,
                            compress = compress, mse = mse)

    class(.data) <- c("tbl_svy", class(.data))
    .data$variables <- tibble::as_tibble(.data$variables)

    as_tbl_svy(.data)
  }


#' @export
#' @rdname as_survey_rep
as_survey_rep.tbl_svy <-
  function(.data, type=c("auto", "JK1", "JKn", "BRR", "bootstrap",
                         "subbootstrap","mrbbootstrap","Fay"),
           rho = 0, fpc=NULL,fpctype=NULL,..., compress=TRUE,
           mse=getOption("survey.replicates.mse")) {

    if (inherits(.data, "svyrep.design")) {
      return(.data)
    } else {
      NextMethod()
    }
  }


#' @export
#' @rdname srvyr-se-deprecated
#' @inheritParams as_survey_rep
#' @param fpctype Finite population correction information
as_survey_rep_ <-
  function(.data, variables = NULL, repweights = NULL, weights = NULL,
           type = c("BRR", "Fay", "JK1", "JKn", "bootstrap",
                    "successive-difference", "ACS",
                    "other"), combined_weights = TRUE,
           rho = NULL, bootstrap_average = NULL, scale = NULL,
           rscales = NULL, fpc = NULL, fpctype = c("fraction", "correction"),
           mse = getOption("survey.replicates.mse")) {

    type <- if (missing(type)) type[1] else type
    as_survey_rep(
      .data,
      variables = !!n_compat_lazy(variables),
      repweights = !!n_compat_lazy(repweights),
      weights = !!n_compat_lazy(weights),
      type = type,
      combined_weights = combined_weights,
      rho = rho,
      bootstrap_average = bootstrap_average,
      scale = scale,
      rscales = rscales,
      fpc = !!n_compat_lazy(fpc),
      fpctype = fpctype,
      mse = mse
    )
  }
