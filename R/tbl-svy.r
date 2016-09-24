#' tbl_svy object.
#'
#' A \code{tbl_svy} wraps a locally stored svydesign and adds methods for
#' dplyr single-table verbs like \code{mutate}, \code{group_by} and
#' \code{summarise}. Create a \code{tbl_svy} using \code{\link{as_survey_design}}.
#'
#' @section Methods:
#'
#' \code{tbl_df} implements these methods from dplyr.
#'
#' \describe{
#' \item{\code{\link[dplyr]{select}} or \code{\link[dplyr]{rename}}}{
#'   Select or rename variables in a survey's dataset.}
#' \item{\code{\link[dplyr]{mutate}} or \code{\link[dplyr]{transmute}}}{
#'   Modify and create variables in a survey's dataset.}
#' \item{\code{\link{group_by}} and \code{\link{summarise}}}{
#'  Get descriptive statistics from survey.}
#' }
#'
#' @examples
#' library(survey)
#' library(dplyr)
#' data(api)
#' svy <- as_survey_design(apistrat, strata = stype, weights = pw)
#' svy
#'
#' # Data manipulation verbs ---------------------------------------------------
#' filter(svy, pcttest > 95)
#' select(svy, starts_with("acs")) # variables used in survey design are automatically kept
#' summarise(svy, col.grad = survey_mean(col.grad))
#' mutate(svy, api_diff = api00 - api99)
#'
#' # Group by operations -------------------------------------------------------
#' # To calculate survey
#' svy_group <- group_by(svy, dname)
#'
#' summarise(svy, col.grad = survey_mean(col.grad),
#'           api00 = survey_mean(api00, vartype = "ci"))
#' @name tbl_svy
NULL

# Mostly mimics survey:::print.survey.design2
#' @export
print.tbl_svy <- function (x, varnames = TRUE, all_survey_vars = FALSE, ...) {
  NextMethod()

  if (length(survey_vars(x)) > 0) {
    print(survey_vars(x), all_survey_vars)
  }
  if(length(groups(x)) != 0) {
    cat("Grouping variables: ")
    cat(paste0(deparse_all(groups(x)), collapse = ", "))
    cat("\n")
  }

  if (varnames) {
    vars <- dplyr::tbl_vars(x$variables)

    # Force calculation for lazy tables (eg tbl_sql) so that
    # we know what types the variables are.
    if (inherits(x$variables, "tbl_lazy")) {
      types <- vapply(dplyr::collect(utils::head(x$variables, 1)),
                                     dplyr::type_sum, character(1))
    } else {
      types <- vapply(x$variables, dplyr::type_sum, character(1))
    }
    order_vars <- which(vars %in% attr(x$variables, "order_var"))
    if (length(order_vars) > 0) {
      vars <- vars[-order_vars]
      types <- types[-order_vars]
    }

    var_types <- paste0(vars, " (", types, ")", collapse = ", ")
    cat(wrap("Data variables: ", var_types), "\n", sep = "")
    invisible(x)
  }
}

#' List variables produced by a tbl.
#' @param x A \code{tbl} object
#' @name tbl_vars
#' @export
#' @importFrom dplyr tbl_vars
NULL


#' @export
tbl_vars.tbl_svy <- function(x) {
  dplyr::tbl_vars(x[["variables"]])
}

as_tbl_svy <- function(x, var_names = list(), uid = NULL) {
  if (!inherits(x, "tbl_svy")) {
    class(x) <- c("tbl_svy", class(x))
  }

  if (inherits(x, "twophase2")) {
    # Convert to tbls if not already (expect them to be one of data.frame, tbl_df or tbl_sqls)
    # data.frames will be converted, others should inherit "tbl".
    if (!inherits(x$phase1$full$variables, "tbl")) {
      x$phase1$full$variables <- dplyr::tbl_df(x$phase1$full$variables)
    }
    if (!inherits(x$phase1$sample$variables, "tbl")) {
      x$phase1$sample$variables <- dplyr::tbl_df(x$phase1$sample$variables)
    }

    # To make twophase behave similarly to the other survey objects, add sample
    # variables from phase1 to the first level of the object.
    x$variables <- x$phase1$sample$variables
  } else if (!inherits(x$variables, "tbl")) {
      x$variables <- dplyr::tbl_df(x$variables)
  }

  survey_vars(x) <- var_names

  if (!is.null(uid)) {
    if (any(duplicated(uid))) {
      stop("uid columns not unique.")
    }
    names(uid) <- get_uid_names(length(uid))
  }
  uid(x) <- uid

  # To make printing better, change call
  x$call <- "Called via srvyr"
  class(x$call) <- "quoteless_text"
  x
}


#' @export
as.data.frame.tbl_svy <- function(x, ...) {
  as.data.frame(x$variables, ...)
}

#' @export
as_tibble.tbl_svy <- function(x, ...) {
  as_tibble(x$variables, ...)
}

#' Coerce survey variables to a data frame (tibble)
#' @param x A \code{tbl_svy} object
#' @name as_tibble
#' @export
#' @importFrom tibble as_tibble
NULL

#' @export
print.quoteless_text <- function(x, ...) {
  cat(paste0(x, "\n"))
}

