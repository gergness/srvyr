#' Create a tbl_svy survey object using two phase design
#'
#' Create a survey object by specifying the survey's two phase design. It is a
#' wrapper around \code{\link[survey]{twophase}}. All survey variables must be
#' included in the data.frame itself. Variables are selected by using bare
#' column names, or convenience functions described in
#' \code{\link[dplyr]{select}}.
#'
#' @export
#' @param .data A data frame (which contains the variables specified below)
#' @param id list of two sets of variable names for sampling unit identifers
#' @param strata list of two sets of variable names (or \code{NULLs}) for stratum identifiers
#' @param probs list of two sets of variable names (or \code{NULLs}) for sampling probabilities
#' @param weights Only for method = "approx", list of two sets of variable names (or \code{NULLs})
#' for sampling weights
#' @param fpc list of two sets of variables (or \code{NULLs} for finite population corrections
#' @param subset bare name of a variable which specifies which observations are selected in phase 2
#' @param method "full" requires (much) more memory, but gives unbiased variance estimates for
#' general multistage designs at both phases. "simple" or "approx" use less memory, and is correect for
#' designs with simple random sampling at phase one and stratifed randoms sampling at phase two. See
#' \code{\link[survey]{twophase}} for more details.
#' @param ... ignored
#' @return An object of class \code{tbl_svy}
#' @examples
#' # Examples from ?survey::twophase
#' # two-phase simple random sampling.
#' data(pbc, package="survival")
#' library(dplyr)
#'
#' pbc <- pbc %>%
#'   mutate(randomized = !is.na(trt) & trt > 0,
#'          id = row_number())
#' d2pbc <- pbc %>%
#'   as_survey_twophase(id = list(id, id), subset = randomized)
#'
#' d2pbc %>% summarize(mean = survey_mean(bili))
#'
#' # two-stage sampling as two-phase
#' library(survey)
#' data(mu284)
#'
#' mu284_1 <- mu284 %>%
#'   dplyr::slice(c(1:15, rep(1:5, n2[1:5] - 3))) %>%
#'   mutate(id = row_number(),
#'          sub = rep(c(TRUE, FALSE), c(15, 34-15)))
#'
#' dmu284 <- mu284 %>%
#'   as_survey_design(ids = c(id1, id2), fpc = c(n1, n2))
#' # first phase cluster sample, second phase stratified within cluster
#' d2mu284 <- mu284_1 %>%
#'   as_survey_twophase(id = list(id1, id), strata = list(NULL, id1),
#'                   fpc = list(n1, NULL), subset = sub)
#' dmu284 %>%
#'   summarize(total = survey_total(y1),
#'             mean = survey_mean(y1))
#' d2mu284 %>%
#'   summarize(total = survey_total(y1),
#'             mean = survey_mean(y1))
#'
#' # dplyr 0.7 introduced new style of NSE called quosures
#' # See `vignette("programming", package = "dplyr")` for details
#' ids <- quo(list(id, id))
#' d2pbc <- pbc %>%
#'   as_survey_twophase(id = !!ids, subset = "randomized")
#'
as_survey_twophase <- function(.data, ...) {
  UseMethod("as_survey_twophase")
}

#' @export
#' @rdname as_survey_twophase
as_survey_twophase.data.frame <-
  function(.data, id, strata = NULL, probs = NULL,
           weights = NULL, fpc = NULL, subset,
           method = c("full", "approx", "simple"), ...) {

  id <- srvyr_select_vars_list(rlang::enquo(id), .data)
  strata <- srvyr_select_vars_list(rlang::enquo(strata), .data)
  probs <- srvyr_select_vars_list(rlang::enquo(probs), .data)
  weights <- srvyr_select_vars_list(rlang::enquo(weights), .data)
  fpc <- srvyr_select_vars_list(rlang::enquo(fpc), .data)
  subset <- srvyr_select_vars(rlang::enquo(subset), .data)

  out <- survey::twophase(
    data = .data,
    id = id,
    strata = strata,
    probs = probs,
    weights = weights,
    fpc = fpc,
    subset = subset,
    method = method
  )

  as_tbl_svy(out, list(ids = id, strata = strata, probs = probs,
                       weights = weights, fpc = fpc, subset = subset))
}


#' @export
#' @rdname as_survey_twophase
as_survey_twophase.twophase2 <- function(.data, ...) {
  as_tbl_svy(.data)
}


#' @export
#' @rdname srvyr-se-deprecated
#' @inheritParams as_survey_twophase
as_survey_twophase_ <- function(.data, id, strata = NULL, probs = NULL,
                             weights = NULL, fpc = NULL, subset,
                             method = c("full", "approx", "simple")) {
  as_survey_twophase(
    .data,
    id = !!n_compat_lazy(id),
    strata = !!n_compat_lazy(strata),
    probs = !!n_compat_lazy(probs),
    weights = !!n_compat_lazy(weights),
    fpc = !!n_compat_lazy(fpc),
    subset = !!n_compat_lazy(subset),
    method = method
  )

}


