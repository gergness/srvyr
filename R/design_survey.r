#' Create a tbl_svy survey object
#'
#' A wrapper around \code{\link[survey]{svydesign}}. All survey variables must be included
#' in the data.frame itself. Select variables by using bare column names, or convenience
#' functions described in \code{\link[dplyr]{select}}. \code{design_survey_} is the
#' standard evaluation counterpart to \code{design_survey}
#'
#' @export
#' @param .data A data frame (which contains the variables specified below)
#' @param ids Variables specifying cluster ids from largest level to smallest level
#' (or NULL / nothing if no clusters).
#' @param probs Variables specifying cluster sampling probabilities.
#' @param strata Variables specifying strata.
#' @param variables Variables specifying variables to be included in survey.
#' Defaults to all variables in .data
#' @param fpc Variables specifying a finite population correct see
#' \code{\link[survey]{svydesign}} for more details.
#' @param nest If \code{TRUE}, relabel cluster ids to enforce nesting within strata.
#' @param check.strata If \code{TRUE}, check that clusters are nested in strata.
#' @param weights Variables specifying weights (inverse of probability).
#' @param pps "brewer" to use Brewer's approximation for PPS sampling without replacement.
#' "overton" to use Overton's approximation. An object of class HR to use the Hartley-Rao
#' approximation. An object of class ppsmat to use the Horvitz-Thompson estimator.
#' @return An object of class \code{tbl_svy}
#' @examples
#' # Examples from ?survey::svydesign
#' library(survey)
#' data(api)
#'
#' # stratified sample
#' library(survey)
#' data(api)
#' dstrata <- apistrat %>%
#'   design_survey(strata = stype, weights = pw)
#'
#' # one-stage cluster sample
#' dclus1 <- apiclus1 %>%
#'   design_survey(dnum, weights = pw, fpc = fpc)
#'
#' # two-stage cluster sample: weights computed from population sizes.
#' dclus2 <- apiclus2 %>%
#'   mutate(fpc2 = as.vector(fpc2)) %>% # work around dplyr's dislike of attributes
#'   design_survey(c(dnum, snum), fpc = c(fpc1, fpc2))
#'
#' ## multistage sampling has no effect when fpc is not given, so
#' ## these are equivalent.
#' dclus2wr <- apiclus2 %>%
#'   dplyr::mutate(weights = weights(dclus2)) %>%
#'   design_survey(c(dnum, snum), weights = weights)
#'
#' dclus2wr2 <- apiclus2 %>%
#'   dplyr::mutate(weights = weights(dclus2)) %>%
#'   design_survey(c(dnum), weights = weights)
#'
#' ## syntax for stratified cluster sample
#' ## (though the data weren't really sampled this way)
#' apistrat %>% design_survey(dnum, strata = stype, weights = pw,
#'                            nest = TRUE)
#'
#' ## PPS sampling without replacement
#' data(election)
#' dpps <- election_pps %>%
#'   design_survey(fpc = p, pps = "brewer")
#'
#' ## design_survey_() uses standard evaluation
#' strata_var <- "stype"
#' weights_var <- "pw"
#' dstrata2 <- apistrat %>%
#'   design_survey_(strata = strata_var, weights = weights_var)
#'

design_survey <- function(.data, ...) {
  # Lazyeval hack (related to http://stackoverflow.com/questions/32143395/)
  # Use lazy_dots(...) instead of named arguments because otherwise it would
  # follow promises to functions defined in other packages.
  arg_names <- c("ids", "probs", "strata", "variables", "fpc", "nest", "check.strata",
                 "weights", "pps")
  dots <- match_dot_args(lazyeval::lazy_dots(...), arg_names)

  # Turn other variable specifications into character strings of variable names
  var_args <- lapply(list(ids = "ids", probs = "probs", strata = "strata", fpc = "fpc",
                          weights = "weights", variables = "variables"),
                     function(var) {
                       nullable(function(x) unname(dplyr::select_vars_(names(.data), x)), dots[[var]])
                     })

  # nest, check.strata, and pps are all logical values. Get values if passed in, otherwise default
  # values
  nest <- if(!is.null(dots[["nest"]])) lazyeval::lazy_eval(dots[["nest"]]) else FALSE
  check.strata <- if(!is.null(dots[["check.strata"]])) lazyeval::lazy_eval(dots[["check.strata"]]
                                                                           ) else !nest
  pps <- if(!is.null(dots[["pps"]])) lazyeval::lazy_eval(dots[["pps"]]) else FALSE

  design_survey_(.data, var_args$ids, probs = var_args$probs, strata = var_args$strata,
                 variables = var_args$variables, fpc = var_args$fpc, nest = nest,
                 check.strata = check.strata, weights = var_args$weights, pps = pps)
}


#' @export
#' @rdname design_survey
design_survey_ <- function(.data, ids = NULL, probs = NULL, strata = NULL, variables = NULL,
                           fpc = NULL, nest = FALSE, check.strata = !nest,
                           weights = NULL, pps = FALSE) {


  # svydesign expects ~0 instead of NULL if no ids are included
  ids_call <- if (missing(ids) || is.null(ids)) ~0 else dplyr::select_(.data, .dots = ids)

  # Need to convert to data.frame to appease survey package and also not
  # send NULL to dplyr::select
  survey_selector <- function(x) {
    if (!is.null(x)) data.frame(dplyr::select_(.data, .dots = x)) else NULL
  }

  out <- survey::svydesign(data = .data,
                           ids = ids_call,
                           probs = survey_selector(probs),
                           strata = survey_selector(strata),
                           variables = survey_selector(variables),
                           fpc = survey_selector(fpc),
                           weights = survey_selector(weights),
                           nest = nest, check.strata = check.strata, pps = pps)

  class(out) <- c("tbl_svy", class(out))
  out$variables <- dplyr::tbl_df(out$variables)

  # Make a list of names that have the survey vars.
  survey_vars(out) <- list(ids = ids, probs = probs, strata = strata, fpc = fpc, weights = weights)

  out
}
