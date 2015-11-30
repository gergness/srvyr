#' Create a tbl_svy survey object using two phase design
#'
#' A wrapper around \code{\link[survey]{twophase}}. All survey variables must be included
#' in the data.frame itself. Select variables by using bare column names, or convenience
#' functions described in \code{\link[dplyr]{select}}. \code{design_twophase_} is the
#' standard evaluation counterpart to \code{design_twophase}
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
#' @return An object of class \code{tbl_svy}
#' @examples
#' # Examples from ?survey::twophase
#' ## two-phase simple random sampling.
#' data(pbc, package="survival")
#'
#' pbc <- pbc %>%
#'   mutate(randomized = !is.na(trt) & trt > 0,
#'          id = row_number())
#' d2pbc <- pbc %>%
#'   design_twophase(id = list(id, id), subset = randomized)
#'
#' d2pbc %>% summarize(mean = survey_mean(bili))
#'
#' ## two-stage sampling as two-phase
#' library(survey)
#' data(mu284)
#'
#' mu284_1 <- mu284 %>%
#'   dplyr::slice(c(1:15, rep(1:5, n2[1:5] - 3))) %>%
#'   mutate(id = row_number(),
#'          sub = rep(c(TRUE, FALSE), c(15, 34-15)))
#'
#' dmu284 <- mu284 %>%
#'   design_survey(ids = c(id1, id2), fpc = c(n1, n2))
#' ## first phase cluster sample, second phase stratified within cluster
#' d2mu284 <- mu284_1 %>%
#'   design_twophase(id = list(id1, id), strata = list(NULL, id1),
#'                   fpc = list(n1, NULL), subset = sub)
#' dmu284 %>%
#'   summarize(total = survey_total(y1),
#'             mean = survey_mean(y1))
#' d2mu284 %>%
#'   summarize(total = survey_total(y1),
#'             mean = survey_mean(y1))
#'
design_twophase <- function(.data, id, strata = NULL, probs = NULL, weights = NULL, fpc = NULL,
                          subset, method = c("full", "approx", "simple")) {
  # Need to turn bare variable to variable names inside list (for 2phase)
  # NULLS are allowed in the list and should be carried forward.
  helper_list <- function(x) {
    x <- x[["expr"]]
    if(x[[1]] != "list" || length(x) > 3) stop("design_twophase requies a list of 2 sets of variables")
    name1 <- unname(dplyr::select_vars_(names(.data), x[[2]]))
    name1 <- if (length(name1) == 0) NULL else name1
    name2 <- unname(dplyr::select_vars_(names(.data), x[[3]]))
    name2 <- if (length(name2) == 0) NULL else name2
    list(name1, name2)
  }

  # Need to turn bare variable to variable names (when not in list)
  helper <- function(x) unname(dplyr::select_vars_(names(.data), x))

  id <- helper_list(lazy_parent(id))
  if (!missing(strata)) strata <- helper_list(lazy_parent(strata))
  if (!missing(probs)) probs <- helper_list(lazy_parent(probs))
  if (!missing(weights)) weights <- helper_list(lazy_parent(weights))
  if (!missing(fpc)) fpc <- helper_list(lazy_parent(fpc))
  subset <- helper(lazy_parent(subset))

  design_twophase_(.data, id, strata = strata, probs = probs,
                 weights = weights, fpc = fpc, subset = subset,
                 method = method)
}


#' @export
#' @rdname design_twophase
design_twophase_ <- function(.data, id, strata = NULL, probs = NULL, weights = NULL, fpc = NULL,
                             subset, method = c("full", "approx", "simple")) {
  # survey::twophase doesn't work with values, needs to be formula of variable names
  # Change list of variable names to formulas
  list_to_formula <- function(x) {
    if (!is.null(x)) lapply(x, function(y) nullable(survey::make.formula, y)) else NULL
  }

  out <- survey::twophase(data = .data,
                          id = list_to_formula(id),
                          strata = list_to_formula(strata),
                          probs = list_to_formula(probs),
                          weights = list_to_formula(weights),
                          fpc = list_to_formula(fpc),
                          subset = survey::make.formula(subset),
                          method = method)

  class(out) <- c("tbl_svy", class(out))

  out$phase1$full$variables <- dplyr::tbl_df(out$phase1$full$variables)
  out$phase1$sample$variables <- dplyr::tbl_df(out$phase1$sample$variables)

  # Make a list of names that have the survey vars.
  survey_vars(out) <- list(ids = id, strata = strata, probs = probs, weights = weights, fpc = fpc,
                           subset = subset)

  # To make twophase behave similarly to the other survey objects, add sample variables
  # from phase1 to the first level of the object.
  out$variables <- out$phase1$sample$variables

  out
}
