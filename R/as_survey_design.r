#' Create a tbl_svy survey object using sampling design
#'
#' Create a survey object with a survey design.
#'
#' If provided a data.frame, it is a wrapper
#' around \code{\link[survey]{svydesign}}. All survey variables must be included
#' in the data.frame itself. Variables are selected by using bare column names, or
#' convenience functions described in \code{\link[dplyr]{select}}.
#' \code{as_survey_design_} is the standard evaluation counterpart to
#' \code{as_survey_design}.
#'
#' If provided a \code{survey.design2} object from the survey package,
#' it will turn it into a srvyr object, so that srvyr functions will work with it
#'
#' @export
#' @param .data A data frame (which contains the variables specified below)
#' @param ids Variables specifying cluster ids from largest level to smallest level
#' (leaving the argument empty, NULL, 1, or 0 indicate no clusters).
#' @param probs Variables specifying cluster sampling probabilities.
#' @param strata Variables specifying strata.
#' @param variables Variables specifying variables to be included in survey.
#' Defaults to all variables in .data
#' @param fpc Variables specifying a finite population correct, see
#' \code{\link[survey]{svydesign}} for more details.
#' @param nest If \code{TRUE}, relabel cluster ids to enforce nesting within strata.
#' @param check_strata If \code{TRUE}, check that clusters are nested in strata.
#' @param weights Variables specifying weights (inverse of probability).
#' @param pps "brewer" to use Brewer's approximation for PPS sampling without replacement.
#' "overton" to use Overton's approximation. An object of class HR to use the Hartley-Rao
#' approximation. An object of class ppsmat to use the Horvitz-Thompson estimator.
#' @param variance For pps without replacement, use variance="YG" for the Yates-Grundy estimator
#' instead of the Horvitz-Thompson estimator
#' @param ... ignored
#' @return An object of class \code{tbl_svy}
#' @examples
#' # Examples from ?survey::svydesign
#' library(survey)
#' data(api)
#'
#' # stratified sample
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' # one-stage cluster sample
#' dclus1 <- apiclus1 %>%
#'   as_survey_design(dnum, weights = pw, fpc = fpc)
#'
#' # two-stage cluster sample: weights computed from population sizes.
#' dclus2 <- apiclus2 %>%
#'   as_survey_design(c(dnum, snum), fpc = c(fpc1, fpc2))
#'
#' ## multistage sampling has no effect when fpc is not given, so
#' ## these are equivalent.
#' dclus2wr <- apiclus2 %>%
#'   dplyr::mutate(weights = weights(dclus2)) %>%
#'   as_survey_design(c(dnum, snum), weights = weights)
#'
#' dclus2wr2 <- apiclus2 %>%
#'   dplyr::mutate(weights = weights(dclus2)) %>%
#'   as_survey_design(c(dnum), weights = weights)
#'
#' ## syntax for stratified cluster sample
#' ## (though the data weren't really sampled this way)
#' apistrat %>% as_survey_design(dnum, strata = stype, weights = pw,
#'                            nest = TRUE)
#'
#' ## PPS sampling without replacement
#' data(election)
#' dpps <- election_pps %>%
#'   as_survey_design(fpc = p, pps = "brewer")
#'
#' ## as_survey_design_ uses standard evaluation
#' strata_var <- "stype"
#' weights_var <- "pw"
#' dstrata2 <- apistrat %>%
#'   as_survey_design_(strata = strata_var, weights = weights_var)
#'
as_survey_design <- function(.data, ...) {
  UseMethod("as_survey_design")
}

#' @export
#' @rdname as_survey_design
as_survey_design.data.frame <-
  function(.data, ids = NULL, probs = NULL, strata = NULL,
           variables = NULL, fpc = NULL, nest = FALSE,
           check_strata = !nest,weights = NULL, pps = FALSE,
           variance = c("HT", "YG"), ...) {

  if (!missing(ids)) {
    ids <- lazy_parent(ids)
    ids <- if (ids$expr == 1 || ids$expr == 0) NULL else helper(ids, .data)
  }
  if (!missing(probs)) probs <- helper(lazy_parent(probs), .data)
  if (!missing(strata)) strata <- helper(lazy_parent(strata), .data)
  if (!missing(fpc)) fpc <- helper(lazy_parent(fpc), .data)
  if (!missing(weights)) weights <- helper(lazy_parent(weights), .data)
  if (!missing(variables)) variables <- helper(lazy_parent(variables), .data)

  as_survey_design_(.data, ids, probs = probs, strata = strata,
                    variables = variables, fpc = fpc, nest = nest,
                    check_strata = check_strata, weights = weights, pps = pps,
                    variance = variance)
}


#' @export
#' @rdname as_survey_design
as_survey_design.survey.design2 <- function(.data, ...) {
  as_tbl_svy(.data)
}


#' @export
#' @rdname as_survey_design
as_survey_design_ <- function(.data, ids = NULL, probs = NULL, strata = NULL,
                              variables = NULL, fpc = NULL, nest = FALSE,
                              check_strata = !nest,weights = NULL, pps = FALSE,
                              variance = c("HT", "YG")) {


  # svydesign expects ~0 instead of NULL if no ids are included
  if (missing(ids) || is.null(ids) || ids == 1 || ids == 0) {
    ids_call <- ~0
    ids <- NULL
  } else {
    ids_call <- dplyr::select_(.data, .dots = ids)
  }
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
                           nest = nest, check.strata = check_strata, pps = pps,
                           variance = variance)

  as_tbl_svy(out, list(ids = ids, probs = probs, strata = strata, fpc = fpc,
                       weights = weights))
}
