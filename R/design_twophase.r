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
#' # d2pbc<-twophase(id=list(~id,~id), data=pbc, subset=~randomized)
#' svymean(~bili, d2pbc)
#'
#' ## two-stage sampling as two-phase
#' data(mu284)
#' ii<-with(mu284, c(1:15, rep(1:5,n2[1:5]-3)))
#' mu284.1<-mu284[ii,]
#' mu284.1$id<-1:nrow(mu284.1)
#' mu284.1$sub<-rep(c(TRUE,FALSE),c(15,34-15))
#' dmu284<-svydesign(id=~id1+id2,fpc=~n1+n2, data=mu284)
#' ## first phase cluster sample, second phase stratified within cluster
#' d2mu284<-twophase(id=list(~id1,~id),strata=list(NULL,~id1),
#'                   fpc=list(~n1,NULL),data=mu284.1,subset=~sub)
#' svytotal(~y1, dmu284)
#' svytotal(~y1, d2mu284)
#' svymean(~y1, dmu284)
#' svymean(~y1, d2mu284)
design_twophase <- function(.data, id, strata = NULL, probs = NULL, weights = NULL, fpc = NULL,
                          subset, method = c("full", "approx", "simple")) {
  # Need to turn bare variable to variable names inside list (for 2phase)
  helper_list <- function(x) {
    x <- x[["expr"]]
    if(x[[1]] != "list" || length(x) > 3) stop("design_twophase requies a list of 2 sets of variables")
    list(unname(dplyr::select_vars_(names(.data), x[[2]])),
         unname(dplyr::select_vars_(names(.data), x[[3]])))
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
    if (!is.null(x)) lapply(x, survey::make.formula) else NULL
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

  out
}
