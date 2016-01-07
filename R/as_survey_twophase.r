#' Create a tbl_svy survey object using two phase design
#'
#' Create a survey object by specifying the survey's two phase design. It is a
#' wrapper around \code{\link[survey]{twophase}}. All survey variables must be
#' included in the data.frame itself. Variables are selected by using bare
#' column names, or convenience functions described in
#' \code{\link[dplyr]{select}}. \code{as_survey_twophase_} is the standard
#' evaluation counterpart to \code{as_survey_twophase}.
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
#' ## as_survey_twophase_ uses standard evaluation
#' id1 <- "id"
#' id2 <- "id"
#' d2pbc <- pbc %>%
#'   as_survey_twophase_(id = list(id1, id2), subset = "randomized")
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

  id <- helper_list(lazy_parent(id), .data)
  if (!missing(strata)) strata <- helper_list(lazy_parent(strata), .data)
  if (!missing(probs)) probs <- helper_list(lazy_parent(probs), .data)
  if (!missing(weights)) weights <- helper_list(lazy_parent(weights), .data)
  if (!missing(fpc)) fpc <- helper_list(lazy_parent(fpc), .data)
  subset <- helper(lazy_parent(subset), .data)

  as_survey_twophase_(.data, id, strata = strata, probs = probs,
                 weights = weights, fpc = fpc, subset = subset,
                 method = method)
}


#' @export
#' @rdname as_survey_twophase
as_survey_twophase.twophase2 <- function(.data, ...) {
  as_tbl_svy(.data)
}


#' @export
#' @rdname as_survey_twophase
as_survey_twophase_ <- function(.data, id, strata = NULL, probs = NULL,
                             weights = NULL, fpc = NULL, subset,
                             method = c("full", "approx", "simple")) {
  # survey::twophase doesn't work with values, needs to be formula of
  # variable names
  # Change list of variable names to formulas
  list_to_formula <- function(x) {
    if (!is.null(x)) {
      lapply(x, function(y) nullable(survey::make.formula, y))
    } else NULL
  }

  out <- survey::twophase(data = .data,
                          id = list_to_formula(id),
                          strata = list_to_formula(strata),
                          probs = list_to_formula(probs),
                          weights = list_to_formula(weights),
                          fpc = list_to_formula(fpc),
                          subset = survey::make.formula(subset),
                          method = method)

  as_tbl_svy(out, list(ids = id, strata = strata, probs = probs,
                       weights = weights, fpc = fpc, subset = subset))
}
