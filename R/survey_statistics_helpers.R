#' Set the variables for the current survey variable
#'
#' This is a helper to allow srvyr's syntactic style. In general, users
#' will not have to worry about setting variables in a survey object
#' unless they are trying to extend srvyr. This function helps convert a vector
#' to a variable in the correct part of a survey object's structure so that
#' functions can refer to it using the survey package's formula notation.
#' See \code{vignette("extending-srvyr")} for more details.
#'
#' @param .svy A survey object
#' @param x A vector to be included in the variables portion of the survey object
#' @param name The name of the variable once it is added. Defaults to `__SRVYR_TEMP_VAR__`
#'   which is formatted weirdly to avoid name collisions.
#' @param add FALSE, the default, overwrite all current variables. If TRUE, will add
#'   this variable instead.
#'
#' @return a tbl_svy with the variables modified
#' @export
set_survey_vars <- function(
  .svy, x, name = "__SRVYR_TEMP_VAR__", add = FALSE
) {
  out <- .svy

  # Sometimes survey package sets probability to Infinite to indicate
  # that an observation has been dropped rather than actually dropping
  # it. In these cases, we want to stretch x out to fit the actual
  # data.
  # In order to work when a group has a factor with a level with no
  # data in it, we check what the current row group is
  if (length(x) != nrow(.svy)) {
    cur_group_rows <- group_rows(cur_svy_full())[[cur_group_id()]]
    if (length(x) == length(cur_group_rows)) {
      x_stretched <- rep(FALSE, nrow(.svy))
      x_stretched[cur_group_rows] <- x
      x <- x_stretched
    }
  }

  if (inherits(.svy, "twophase2")) {
    if (!add) {
      out$phase1$sample$variables <- select(out$phase1$sample$variables, dplyr::one_of(group_vars(out)))
    }
    out$phase1$sample$variables[[name]] <- x
  } else {
    if (!add) {
      out$variables <- out$variables[, group_vars(out), drop = FALSE]
    }
    out$variables[[name]] <- x
  }
  out
}

#' Get the variance estimates for a survey estimate
#'
#' This is a helper to allow srvyr's syntactic style. In general, users
#' will not have to worry about getting survey variance estimates directly
#' unless they are trying to extend srvyr. This function helps convert from
#' the result of a survey function into a data.frame with an estimate and
#' measures of variance around it in a way that summarize expects.
#' See \code{vignette("extending-srvyr")} for more details.
#'
#' @param stat A survey statistic object, usually the result of a function from the survey
#'   package or svyby.
#' @param vartype A vector indicating which variance estimates to calculate (options are
#'   se for standard error, ci for confidence interval, var for variance or cv for
#'   coefficient of variation). Multiples are allowed.
#' @param level One or more levels to calculate a confidence interval.
#' @param df Degrees of freedom, many survey functions default to Inf, but srvyr functions
#'   generally default to the result of calling degf on the survey object.
#' @param pre_calc_ci Whether the confidence interval is pre-calculated (as in svyciprop)
#' @param deff Whether to return the design effect (calculated using survey::deff)
#'
#' @return a tbl_svy with the variables modified
#' @export
get_var_est <- function(
  stat, vartype, level = 0.95, df = Inf, pre_calc_ci = FALSE, deff = FALSE
) {
  out_width <- 1
  out <- lapply(vartype, function(vvv) {
  if (vvv == "se") {
      se <- survey::SE(stat)
      # Needed for grouped quantile
      if (!inherits(se, "data.frame")) {
        se <- as.data.frame(se)
      }
      names(se) <- "_se"
      se
    } else if (vvv == "ci" && !pre_calc_ci) {
      if (length(level) == 1) {
        ci <- data.frame(matrix(
          stats::confint(stat, level = level, df = df),
          ncol = 2 * out_width
        ))
        names(ci) <- c("_low", "_upp")
      } else {
        lci <- lapply(level, function(x) {as.data.frame(stats::confint(stat,level = x, df = df))})
        ci <- dplyr::bind_cols(lci)
        names(ci) <- paste0(c("_low", "_upp"), rep(level, each = 2) * 100)
      }
      ci
    } else if (vvv == "ci" && pre_calc_ci) {
      if (inherits(stat, "data.frame")) {
        ci <- data.frame(stat[c("ci_l", "ci_u")])
        names(ci) <- c("_low", "_upp")
      } else {
        ci <- data.frame(matrix(stats::confint(stat), ncol = 2 * out_width))
        names(ci) <- c("_low", "_upp")
      }
      ci
    } else if (vvv == "var") {
      var <- data.frame(matrix(survey::SE(stat) ^ 2, ncol = out_width))
      names(var) <- "_var"
      var
    } else if (vvv == "cv") {
      cv <- data.frame((matrix(survey::cv(stat), ncol = out_width)))
      names(cv) <- "_cv"
      cv
    } else {
      stop(paste0("Unexpected vartype ", vvv))
    }
  })

  coef <- as.data.frame(unclass(coef(stat)))
  names(coef) <- "coef"
  out <- c(list(coef), out)

  if (!isFALSE(deff)) {
    deff <- data.frame(matrix(survey::deff(stat), ncol = out_width))
    names(deff) <- "_deff"
    out <- c(out, list(deff))
  }

  as_srvyr_result_df(do.call(cbind, out))
}

# Largely the same as get_var_est(), but need to handle the fact that there can be
# multiple quantiles and that CI's are stored slightly differently.
get_var_est_quantile <- function(stat, vartype, q, level = 0.95, df = Inf) {
  qnames <- paste0("_q", gsub("\\.", "", formatC(q * 100, width = 2, flag = "0")))
  out_width <- length(qnames)
  out <- lapply(vartype, function(vvv) {
    if (vvv == "se") {
      se <- survey::SE(stat)
      # Needed for grouped quantile
      if (!inherits(se, "data.frame")) {
        se <- data.frame(matrix(se, ncol = out_width))
      }
      names(se) <- paste0(qnames, "_se")
      se
    } else if (vvv == "ci") {
      if (inherits(stat, "data.frame")) {
        ci_cols <- grep("^ci_[lu]", names(stat))
        ci <- data.frame(stat[, ci_cols])
      } else {
        ci <- data.frame(matrix(stats::confint(stat), ncol = 2 * out_width))
      }
      names(ci) <- paste0(qnames, rep(c("_low", "_upp"), each = out_width))
      ci
    } else if (vvv == "var") {
      var <- data.frame(matrix(survey::SE(stat) ^ 2, ncol = out_width))
      names(var) <- paste0(qnames, "_var")
      var
    } else if (vvv == "cv") {
      cv <- data.frame((matrix(survey::cv(stat), ncol = out_width)))
      names(cv) <- paste0(qnames, "_cv")
      cv
    } else {
      stop(paste0("Unexpected vartype ", vvv))
    }
  })

  coef <- data.frame(matrix(coef(stat), ncol = out_width))
  names(coef) <- qnames
  out <- lapply(out, as.data.frame)
  out <- c(list(coef), out)

  as_srvyr_result_df(dplyr::bind_cols(out))
}

get_empty_var_est <- function(vartype, level = 0.95, deff = FALSE) {
  out <- lapply(vartype, function(vvv) {
    if (vvv == "se") {
      data.frame("_se" = NA, check.names = FALSE)
    } else if (vvv == "ci") {
      if (length(level) == 1) {
        ci <- data.frame("_low" = NA, "_upp" = NA, check.names = FALSE)
      } else {
        nms <- paste0(c("_low", "_upp"), rep(level, each = 2) * 100)
        ci <- stats::setNames(as.data.frame(lapply(nms, NA)), nms)
      }
      ci
    } else if (vvv == "var") {
      data.frame("_var" = NA, check.names = FALSE)
    } else if (vvv == "cv") {
      data.frame("_cv" = NA, check.names = FALSE)
    } else {
      stop(paste0("Unexpected vartype ", vvv))
    }
  })
  out <- list(data.frame(coef = NA), out)
  if (!isFALSE(deff)) {
    out[["_deff"]] <- NA
  }
  as_srvyr_result_df(do.call(cbind, out))
}


stop_for_factor <- function(x) {
  if (is.factor(x)) {
    stop(paste0(
      "Factor not allowed in survey functions, should be used as a grouping variable."
    ), call. = FALSE)
  } else if (is.character(x)) {
    stop(paste0(
      "Character vectors not allowed in survey functions, should be used as a grouping variable."
    ), call. = FALSE)
  }
}

# srvyr_result_df helpers ---

#' Create a srvyr results data.frame which is automatically unpacked by srvyr
#'
#' srvyr uses data.frame columns wrapped by this function to know when to
#' automatically unpack the results for the user. When developing extensions
#' (see vignette \code{extending-srvyr}), use this function to wrap the result
#' in so that \code{summarize} knows to unpack them.
#'
#' @param x A data.frame
#'
#' @return An object with the \code{srvyr_result_df} added
#' @export
#' @keywords internal
as_srvyr_result_df <- function(x) {
  class(x) <- c("srvyr_result_df", class(x))
  x
}

is_srvyr_result_df <- function(x) {
  inherits(x, "srvyr_result_df")
}

#' @export
Math.srvyr_result_df <- function(x, ...) {
  out <- NextMethod("Math", x)
  class(out) <- c("srvyr_result_df", class(out))
  out
}

#' @export
Ops.srvyr_result_df <- function(e1, e2) {
  out <- NextMethod()
  class(out) <- c("srvyr_result_df", class(out))
  out
}

#' Get the full-sample weights for the current context
#'
#' This is a helper to allow srvyr's syntactic style. This function allows quick access
#' to the full-sample weights for the current group, using \code{cur_svy_wts()},
#' See \code{vignette("extending-srvyr")} for more details.
#'
#' @return a numeric vector containing full-sample weights
#' @examples
#'
#' data(api, package = 'survey')
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarize(sum_of_weights = sum(cur_svy_wts()),
#'             kish_deff = var(cur_svy_wts())/(mean(cur_svy_wts())^2))
#'
#' @export
cur_svy_wts <- function() {
  cur_svy_env$split[[dplyr::cur_group_id()]][['pweights']] %||% (1/cur_svy_env$split[[dplyr::cur_group_id()]][['prob']])
}
