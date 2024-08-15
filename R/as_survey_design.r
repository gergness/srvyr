#' Create a tbl_svy survey object using sampling design
#'
#' Create a survey object with a survey design.
#'
#' If provided a data.frame, it is a wrapper
#' around \code{\link[survey]{svydesign}}. All survey variables must be included
#' in the data.frame itself. Variables are selected by using bare column names, or
#' convenience functions described in \code{\link[dplyr]{select}}.
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
#' # dplyr 0.7 introduced new style of NSE called quosures
#' # See `vignette("programming", package = "dplyr")` for details
#' st <- quo(stype)
#' wt <- quo(pw)
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = !!st, weights = !!wt)
#'
as_survey_design <- function(.data, ...) {
  UseMethod("as_survey_design")
}

#' @export
#' @rdname as_survey_design
as_survey_design.data.frame <-
  function(.data, ids = NULL, probs = NULL, strata = NULL,
           variables = NULL, fpc = NULL, nest = FALSE,
           check_strata = !nest, weights = NULL, pps = FALSE,
           variance = c("HT", "YG"), ...) {

  ids <- srvyr_select_vars(rlang::enquo(ids), .data, check_ids = TRUE)
  probs <- srvyr_select_vars(rlang::enquo(probs), .data)
  strata <- srvyr_select_vars(rlang::enquo(strata), .data)
  fpc <- srvyr_select_vars(rlang::enquo(fpc), .data)
  weights <- srvyr_select_vars(rlang::enquo(weights), .data)
  variables <- srvyr_select_vars(rlang::enquo(variables), .data)

  if (is.null(ids)) ids <- ~1
  out <- survey::svydesign(
    ids, probs, strata, variables, fpc, .data, nest, check_strata, weights, pps
  )

  out <- as_tbl_svy(
    out,
    list(ids = ids, probs = probs, strata = strata, fpc = fpc, weights = weights)
  )

  preserve_groups(out, .data)
}

#' @export
#' @rdname as_survey_design
as_survey_design.survey.design2 <- function(.data, ...) {
  as_tbl_svy(.data)
}

#' @export
#' @rdname as_survey_design
as_survey_design.tbl_lazy <-
  function(.data, ids = NULL, probs = NULL, strata = NULL,
           variables = NULL, fpc = NULL, nest = FALSE,
           check_strata = !nest, weights = NULL, pps = FALSE,
           variance = c("HT", "YG"), ...) {

    ids <- rlang::enquo(ids)
    probs <- rlang::enquo(probs)
    strata <- rlang::enquo(strata)
    fpc <- rlang::enquo(fpc)
    weights <- rlang::enquo(weights)
    variables <- rlang::enquo(variables)

    survey_vars_local <- get_lazy_vars(
      data = .data, id = !!ids, !!probs, !!strata, !!fpc, !!weights, !!variables
    )

    ids <- srvyr_select_vars(ids, survey_vars_local, check_ids = TRUE)
    probs <- srvyr_select_vars(probs, survey_vars_local)
    strata <- srvyr_select_vars(strata, survey_vars_local)
    fpc <- srvyr_select_vars(fpc, survey_vars_local)
    weights <- srvyr_select_vars(weights, survey_vars_local)
    variables <- srvyr_select_vars(variables, survey_vars_local)

    if (is.null(ids)) ids <- ~1
    out <- survey::svydesign(
      ids, probs, strata, variables, fpc, survey_vars_local, nest, check_strata, weights, pps
    )
    out$variables <- .data

    as_tbl_svy(
      out,
      list(ids = ids, probs = probs, strata = strata, fpc = fpc, weights = weights)
    )
  }

#' @export
#' @rdname srvyr-se-deprecated
#' @inheritParams as_survey_design
as_survey_design_ <- function(.data, ids = NULL, probs = NULL, strata = NULL,
                              variables = NULL, fpc = NULL, nest = FALSE,
                              check_strata = !nest, weights = NULL, pps = FALSE,
                              variance = c("HT", "YG")) {
  as_survey_design(
    .data,
    ids = !!n_compat_lazy(ids),
    probs = !!n_compat_lazy(probs),
    strata = !!n_compat_lazy(strata),
    variables = !!n_compat_lazy(variables),
    fpc = !!n_compat_lazy(fpc),
    nest = nest,
    check_strata = check_strata,
    weights = !!n_compat_lazy(weights),
    pps = pps,
    variance = variance
  )
}
