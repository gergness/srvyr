# Want the argument .svy to be in the ..., so that we can not document .svy,
# and still please the checks. Therefore, we can't use S3 method dispatch, because
# .svy isn't a named arguemnt.

#' Calculate the mean and its variation using survey methods
#'
#' Calculate means and proportions from complex survey data. A wrapper
#' around \code{\link[survey]{svymean}}, or if \code{proportion = TRUE},
#' \code{\link[survey]{svyciprop}}. \code{survey_mean} should always be
#' called from \code{\link{summarise}}.
#'
#' @param x A variable or expression, or empty
#' @param na.rm A logical value to indicate whether missing values should be dropped
#' @param vartype Report variability as one or more of: standard error ("se", default),
#'                confidence interval ("ci"), variance ("var") or coefficient of variation
#'                ("cv").
#' @param level A single number or vector of numbers indicating the confidence level
#' @param proportion Use methods to calculate the proportion that may have more accurate
#'                   confidence intervals near 0 and 1. Based on
#'                   \code{\link[survey]{svyciprop}}.
#' @param prop_method Type of proportion method to use if proportion is \code{TRUE}. See
#'                    \code{\link[survey]{svyciprop}} for details.
#' @param ... Ignored
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99 = survey_mean(api99),
#'             api_diff = survey_mean(api00 - api99, vartype = c("ci", "cv")))
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(api00 = survey_mean(api00))
#'
#' # Leave x empty to calculate the proportion in each group
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(pct = survey_mean())
#'
#' # Setting proportion = TRUE uses another method for calculating confidence intervals
#' dstrata %>%
#'   summarise(high_api = survey_mean(api00 > 875, proportion = TRUE, vartype = "ci"))
#'
#' # level takes a vector for multiple levels of confidence intervals
#' dstrata %>%
#'   summarise(api99 = survey_mean(api99, vartype = "ci", level = c(0.95, 0.65)))
#'
#' @export
survey_mean <- function(x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"),
                        level = 0.95, proportion = FALSE,
                        prop_method = c("logit", "likelihood", "asin", "beta",
                                        "mean"), ...) {
  args <- list(...)
  if (!".svy" %in% names(args)) {
    stop_direct_call("survey_mean")
  }

  .svy <- args[[".svy"]]
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  if (missing(prop_method)) prop_method <- "logit"
  prop_method <- match.arg(prop_method, several.ok = TRUE)

  if (inherits(.svy, "grouped_svy")) {
    survey_mean_grouped_svy(.svy, x, na.rm, vartype, level, proportion, prop_method)
  } else if (inherits(.svy, "tbl_svy")) {
    survey_mean_tbl_svy(.svy, x, na.rm, vartype, level, proportion, prop_method)
  } else {
    stop_fake_method("survey_mean", class(.svy))
  }
}

survey_mean_tbl_svy <- function(.svy, x, na.rm = FALSE,
                                vartype = c("se", "ci", "var", "cv"),
                                level = 0.95, proportion = FALSE,
                                prop_method = c("logit", "likelihood", "asin",
                                                "beta", "mean")) {

  if (!proportion) {
    survey_stat_ungrouped(.svy, survey::svymean, x, na.rm, vartype, level)
  } else {
    # survey::ciprop only accepts formulas so can't use main function
    survey_stat_proportion(.svy, x, na.rm, vartype, level, prop_method)
  }
}

survey_mean_grouped_svy <- function(.svy, x, na.rm = FALSE,
                                    vartype = c("se", "ci", "var", "cv"),
                                    level = 0.95, proportion = FALSE,
                                    prop_method = c("logit", "likelihood",
                                                    "asin", "beta", "mean")) {
  if (missing(x)) {
    if (proportion) stop("proportion does not work with factors.")
    survey_stat_factor(.svy, survey::svymean, na.rm, vartype, level)
  } else if (proportion) {
    survey_stat_grouped(.svy, survey::svyciprop, x, na.rm, vartype, level,
                        prop_method)
  } else survey_stat_grouped(.svy, survey::svymean, x, na.rm, vartype, level)
}


#' Calculate the total and its variation using survey methods
#'
#' Calculate totals from complex survey data. A wrapper
#' around \code{\link[survey]{svytotal}}. \code{survey_total} should always be
#' called from \code{\link{summarise}}.
#'
#' @param x A variable or expression, or empty
#' @param na.rm A logical value to indicate whether missing values should be dropped
#' @param vartype Report variability as one or more of: standard error ("se", default),
#'                confidence interval ("ci"), variance ("var") or coefficient of variation
#'                ("cv").
#' @param level A single number or vector of numbers indicating the confidence level
#' @param ... Ignored
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(enroll = survey_total(enroll),
#'             tot_meals = survey_total(enroll * meals / 100, vartype = c("ci", "cv")))
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(api00 = survey_total(enroll))
#'
#' # Leave x empty to calculate the total in each group
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(pct = survey_total())
#'
#' # level takes a vector for multiple levels of confidence intervals
#' dstrata %>%
#'   summarise(enroll = survey_total(enroll, vartype = "ci", level = c(0.95, 0.65)))
#'
#' @export
survey_total <- function(x = NULL, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"),
                         level = 0.95, ...) {
  args <- list(...)
  if (!".svy" %in% names(args)) {
    stop_direct_call("survey_total")
  }

  .svy <- args[[".svy"]]
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)

  if (inherits(.svy, "grouped_svy")) {
    survey_total_grouped_svy(.svy, x, na.rm, vartype, level)
  } else if (inherits(.svy, "tbl_svy")) {
    survey_total_tbl_svy(.svy, x, na.rm, vartype, level)
  } else {
    stop_fake_method("survey_total", class(.svy))
  }
}

survey_total_tbl_svy <- function(.svy, x, na.rm = FALSE,
                                 vartype = c("se", "ci", "var", "cv"),
                                 level = 0.95) {
  survey_stat_ungrouped(.svy, survey::svytotal, x, na.rm, vartype, level)
}

survey_total_grouped_svy <- function(.svy, x, na.rm = FALSE,
                                     vartype = c("se", "ci", "var", "cv"),
                                     level = 0.95) {
  if (!is.null(x)) survey_stat_grouped(.svy, survey::svytotal, x, na.rm,
                                       vartype, level)
  else survey_stat_factor(.svy, survey::svytotal, na.rm, vartype, level)
}


#' Calculate the ratio and its variation using survey methods
#'
#' Calculate ratios from complex survey data. A wrapper
#' around \code{\link[survey]{svyratio}}. \code{survey_ratio}
#' should always be called from \code{\link{summarise}}.
#'
#' @param numerator The numerator of the ratio
#' @param denominator The denominator of the ratio
#' @param na.rm A logical value to indicate whether missing values should be dropped
#' @param vartype Report variability as one or more of: standard error ("se", default),
#'                confidence interval ("ci"), variance ("var") or coefficient of variation
#'                ("cv").
#' @param level A single number or vector of numbers indicating the confidence level
#' @param ... Ignored
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(enroll = survey_ratio(api00, api99, vartype = c("ci", "cv")))
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(api00 = survey_ratio(api00, api99))
#'
#' # level takes a vector for multiple levels of confidence intervals
#' dstrata %>%
#'   summarise(enroll = survey_ratio(api99, api00, vartype = "ci", level = c(0.95, 0.65)))
#'
#' @export
survey_ratio <- function(numerator, denominator, na.rm = FALSE,
                         vartype = c("se", "ci", "var", "cv"),
                         level = 0.95, ...) {
  args <- list(...)
  if (!".svy" %in% names(args)) {
    stop_direct_call("survey_ratio")
  }

  .svy <- args[[".svy"]]
  if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)

  if (inherits(.svy, "grouped_svy")) {
    survey_ratio_grouped_svy(.svy, numerator, denominator, na.rm, vartype, level)
  } else if (inherits(.svy, "tbl_svy")) {
    survey_ratio_tbl_svy(.svy, numerator, denominator, na.rm, vartype, level)
  } else {
    stop_fake_method("survey_ratio", class(.svy))
  }

}

survey_ratio_tbl_svy <- function(.svy, numerator, denominator, na.rm = FALSE,
                                 vartype = c("se", "ci", "var", "cv"),
                                 level = 0.95) {

  vartype <- c("coef", vartype)
  stat <- survey::svyratio(data.frame(numerator), data.frame(denominator),
                           .svy, na.rm = na.rm)

  out <- get_var_est(stat, vartype, level = level)
  out
}

survey_ratio_grouped_svy <- function(.svy, numerator, denominator,
                                     na.rm = FALSE,
                                     vartype = c("se", "ci", "var", "cv"),
                                     level = 0.95) {

  grps <- survey::make.formula(groups(.svy))

  # svyby breaks when you feed it raw vector to be measured... Add it to
  # the data.frame with mutate and then pass in the name
  .svy$variables[["___numerator"]] <- numerator
  .svy$variables[["___denominator"]] <- denominator

  # Slight hack for twophase -- move the created variables to where survey
  # expects them
  if (inherits(.svy, "twophase2")) {
    .svy$phase1$sample$variables <- .svy$variables
  }

  stat <- survey::svyby(~`___numerator`, grps, .svy, survey::svyratio,
                       denominator = ~`___denominator`,
                       na.rm = na.rm, ci = TRUE)

  vartype <- c("grps", "coef", vartype)

  out <- get_var_est(stat, vartype, grps = as.character(groups(.svy)),
                     level = level)

  out
}

#' Calculate the quantile and its variation using survey methods
#'
#' Calculate quantiles from complex survey data. A wrapper
#' around \code{\link[survey]{svyquantile}}. \code{survey_quantile} and
#' \code{survey_median} should always be called from \code{\link{summarise}}.
#'
#' @param x A variable or expression
#' @param na.rm A logical value to indicate whether missing values should be dropped
#' @param quantiles A vector of quantiles to calculate
#' @param vartype Report variability as one or more of: standard error ("se", default),
#'                confidence interval ("ci") (variance and coefficient of variation not
#'                available).
#' @param level A single number indicating the confidence level (only one level allowed)
#' @param q_method See "method" in \code{\link[stats]{approxfun}}
#' @param f See \code{\link[stats]{approxfun}}
#' @param interval_type See \code{\link[survey]{svyquantile}}
#' @param ties See \code{\link[survey]{svyquantile}}
#' @param ... Ignored
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99 = survey_quantile(api99, c(0.25, 0.5, 0.75)),
#'             api00 = survey_median(api00, vartype = c("ci")))
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(api00 = survey_median(api00))
#'
#' @export
survey_quantile <- function(x, quantiles, na.rm = FALSE,
                            vartype = c("none", "se", "ci"),
                            level = 0.95, q_method = "linear", f = 1,
                            interval_type = c("Wald", "score", "betaWald"),
                            ties = c("discrete", "rounded"), ...) {
  args <- list(...)
  if (!".svy" %in% names(args)) {
    stop_direct_call("survey_quantile")
  }

  .svy <- args[[".svy"]]
  if (missing(vartype)) vartype <- "none"
  vartype <- match.arg(vartype, several.ok = TRUE)
  if (missing(interval_type)) interval_type <- "Wald"
  interval_type <- match.arg(interval_type, several.ok = TRUE)
  if (missing(ties)) ties <- "discrete"
  ties <- match.arg(ties, several.ok = TRUE)

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  if (inherits(.svy, "grouped_svy")) {
    survey_quantile_grouped_svy(.svy, x, quantiles, na.rm, vartype, level, q_method, f,
                                interval_type, ties)
  } else if (inherits(.svy, "tbl_svy")) {
    survey_quantile_tbl_svy(.svy, x, quantiles, na.rm, vartype, level, q_method, f,
                            interval_type, ties)
  } else {
    stop_fake_method("survey_quantile", class(.svy))
  }
}

survey_quantile_tbl_svy <- function(.svy, x, quantiles, na.rm = FALSE,
                                    vartype = c("none", "se", "ci"),
                                    level = 0.95, q_method = "linear", f = 1,
                                    interval_type = c("Wald", "score",
                                                      "betaWald"),
                                    ties = c("discrete", "rounded")) {

  vartype <- setdiff(vartype, "none")
  vartype <- c("coef", vartype)

  stat <- survey::svyquantile(data.frame(x), .svy,
                      quantiles = quantiles, na.rm = na.rm,
                      ci = TRUE, level = level, method = q_method, f = f,
                      interval.type = interval_type, ties = ties)

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2,
                                                 flag = "0")))

  out <- get_var_est(stat, vartype, var_names = q_text, level = level,
                     quantile = TRUE)
  out
}

survey_quantile_grouped_svy <- function(.svy, x, quantiles, na.rm = FALSE,
                                        vartype = c("none", "se", "ci"),
                                        level = 0.95, q_method = "linear",
                                        f = 1,
                                        interval_type = c("Wald", "score",
                                                          "betaWald"),
                                        ties = c("discrete", "rounded")) {

  vartype <- setdiff(vartype, "none")

  grps <- survey::make.formula(groups(.svy))

  .svy$variables[["___arg"]] <- x

  # Slight hack for twophase -- move the created variables to where survey
  # expects them
  if (inherits(.svy, "twophase2")) {
    .svy$phase1$sample$variables <- .svy$variables
  }

  stat <- survey::svyby(formula = ~`___arg`, grps, .svy, survey::svyquantile,
                      quantiles = quantiles, na.rm = na.rm,
                      ci = TRUE, level = level, method = q_method,
                      f = f, interval.type = interval_type, ties = ties)

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2,
                                                 flag = "0")))
  vartype <- c("grps", "coef", vartype)
  out <- get_var_est(stat, vartype, var_names = q_text,
                     grps = as.character(groups(.svy)), level = level)

  out
}


#' @export
#' @rdname survey_quantile
survey_median <- function(x, na.rm = FALSE,
                          vartype = c("none", "se", "ci"),
                          level = 0.95, q_method = "linear", f = 1,
                          interval_type = c("Wald", "score",
                                            "betaWald"),
                          ties = c("discrete", "rounded"), ...) {

  args <- list(...)
  if (!".svy" %in% names(args)) {
    stop_direct_call("survey_median")
  }

  .svy <- args[[".svy"]]
  if (missing(vartype)) vartype <- "none"
  vartype <- match.arg(vartype, several.ok = TRUE)
  if (missing(interval_type)) interval_type <- "Wald"
  interval_type <- match.arg(interval_type, several.ok = TRUE)
  if (missing(ties)) ties <- "discrete"
  ties <- match.arg(ties, several.ok = TRUE)

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  survey_quantile(x, quantiles = 0.5, na.rm = na.rm, vartype = vartype,
                  level = level, q_method = q_method, f = f,
                  interval_type = interval_type, ties = ties, .svy = .svy)
}

#' Calculate the an unweighted summary statistic from a survey
#'
#' Calculate unweighted summaries from a survey dataset, just as on
#' a normal data.frame with \code{\link[dplyr]{summarise}}.
#'
#' @param x A variable or expression
#' @param ... Ignored
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99_unw = unweighted(mean(api99)),
#'             n = unweighted(n()))
#'
#' dstrata %>%
#'   group_by(stype) %>%
#'   summarise(api_diff_unw = unweighted(mean(api00 - api99)))
#'
#' @export
unweighted <- function(x, ...) {
  args <- list(...)
  if (!".svy" %in% names(args)) {
    stop_direct_call("unweighted")
  }

  .svy <- args[[".svy"]]
  dots <- lazyeval::lazy(x)

  out <- summarize_(.svy[["variables"]], .dots = dots)
  names(out)[length(names(out))] <- ""
  out
}


survey_stat_ungrouped <- function(.svy, func, x, na.rm, vartype, level) {
  if (class(x) == "factor") {
    stop(paste0("Factor not allowed in survey functions, should ",
                "be used as a grouping variable"))
  }
  if (class(x) == "logical") x <- as.integer(x)
  stat <- func(data.frame(x), .svy, na.rm = na.rm)

  vartype <- c("coef", vartype)
  out <- get_var_est(stat, vartype, level = level)

  out
}

survey_stat_grouped <- function(.svy, func, x, na.rm, vartype, level,
                                prop_method = NULL) {
  grps <- survey::make.formula(groups(.svy))

  if (class(x) == "factor") {
    stop(paste0("Factor not allowed in survey functions, should ",
                "be used as a grouping variable"))
  }
  if (class(x) == "logical") x <- as.integer(x)
  # svyby breaks when you feed it raw vector to be measured... Add it to
  # the data.frame with mutate and then pass in the name
  .svy$variables[["___arg"]] <- x

  # Slight hack for twophase -- move the created variables to where survey
  # expects them
  if (inherits(.svy, "twophase2")) {
    .svy$phase1$sample$variables <- .svy$variables
  }
  vartype <- c("grps", "coef", vartype)

  if (is.null(prop_method)) {
    stat <- survey::svyby(~`___arg`, grps, .svy, func, na.rm = na.rm, se = TRUE)
  } else {
    vartype[vartype == "ci"] <- "ci-prop"
    stat <- survey::svyby(~`___arg`, grps, .svy, func, na.rm = na.rm,
                          se = TRUE, vartype = c("ci", "se"),
                          method = prop_method)
  }

  out <- get_var_est(stat, vartype, grps = as.character(groups(.svy)),
                     level = level)
  out
}

survey_stat_factor <- function(.svy, func, na.rm, vartype, level) {
  grps <- as.character(groups(.svy))
  peel <- grps[length(grps)]
  grps <- setdiff(grps, peel)

  vartype <- c("coef", vartype)

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  if (length(grps) > 0) {
    stat <- survey::svyby(survey::make.formula(peel),
                          survey::make.formula(grps),
                          .svy, func, na.rm = na.rm, se = TRUE)

    var_names <- attr(stat, "svyby")[["variables"]]
    var_names <- unlist(lapply(var_names,
                               function(x) substring(x, nchar(peel) + 1)))

    vartype <- c("grps", vartype)

    out <- get_var_est(stat, vartype, var_names = var_names, grps = grps,
                       level = level)
    peel_levels <- levels(.svy[["variables"]][[peel]])
    out <- factor_stat_reshape(out, grps, peel, var_names, peel_levels)

    out
  } else {
    # Needed because grouped don't usually have "coef"
    vartype <- c("lvls", vartype)
    stat <- func(survey::make.formula(peel), .svy, na.rm = na.rm)

    out <- get_var_est(stat, vartype, peel = peel,
                       peel_levels = levels(.svy[["variables"]][[peel]]))

    out
  }
}

survey_stat_proportion <- function(.svy, x, na.rm, vartype, level,
                                   prop_method) {
  .svy$variables["___arg"] <- x
  stat <- survey::svyciprop(~`___arg`, .svy, na.rm = na.rm, level = level,
                            method = prop_method)

  vartype <- c("coef", vartype)

  out <- get_var_est(stat, vartype, quantile = TRUE)
  out
}

get_var_est <- function(stat, vartype, var_names = "", grps = "",
                        peel = "", peel_levels = NULL, level = 0.95,
                        quantile = FALSE) {
  out_width <- length(var_names)
  out <- lapply(vartype, function(vvv) {
    if (vvv == "coef") {
      coef <- data.frame(matrix(coef(stat), ncol = out_width))
      names(coef) <- var_names
      coef
    } else if (vvv == "se") {
      se <- survey::SE(stat)
      # Needed for grouped quantile
      if (!inherits(se, "data.frame")) {
        se <- data.frame(matrix(se, ncol = out_width))
      }
      names(se) <- paste0(var_names, "_se")
      se
    } else if (vvv == "ci") {
      if (!quantile) {
        if (length(level)==1) {
          ci <- data.frame(matrix(stats::confint(stat, level = level),
                                  ncol = 2 * out_width))
          names(ci) <- c(paste0(var_names, "_low"), paste0(var_names, "_upp"))
        } else {
          lci <- lapply(level, function(x) {as.data.frame(stats::confint(stat,level = x))})
          ci <- dplyr::bind_cols(lci)
          names(ci) <- paste0(var_names,"_", c("low","upp"),rep(level,each=2)*100)
        }
      } else {
        ci <- data.frame(matrix(stats::confint(stat), ncol = 2 * out_width))
        names(ci) <- c(paste0(var_names, "_low"), paste0(var_names, "_upp"))
      }

      ci
    } else if (vvv == "ci-prop") {
      ci <- data.frame(stat[c("ci_l", "ci_u")])
      names(ci) <- c(paste0(var_names, "_low"), paste0(var_names, "_upp"))
      ci
    } else if (vvv == "var") {
      var <- data.frame(matrix(survey::SE(stat) ^ 2, ncol = out_width))
      names(var) <- paste0(var_names, "_var")
      var
    } else if (vvv == "cv") {
      cv <- data.frame((matrix(survey::cv(stat), ncol = out_width)))
      names(cv) <- paste0(var_names, "_cv")
      cv
    } else if (vvv == "grps") {
      stat[grps]
    } else if (vvv == "lvls") {
      # Only for survey_stat_factor with only one groups
      # Add on level variable -- survey leaves it in an ugly state, with the
      # varname pasted in, so we have to remove it. Also, check if it was
      # originally a character and convert if it was.
      lvls <- data.frame(names(coef(stat)), stringsAsFactors = FALSE)
      lvls[[1]] <- gsub(paste0("^", peel), "", lvls[[1]])
      if (!is.null(peel_levels)) {
        lvls[[1]] <- factor(lvls[[1]], peel_levels)
      }
      names(lvls) <- peel
      lvls
    }
  })
  dplyr::bind_cols(out)
}

# base reshape was difficult to work with, but might be worth reinvestigating
# (or others, but have to weight the cost of extra dependency). This takes the
# survey stat object that has the peel variable wide, and makes it long.
factor_stat_reshape <- function(stat, grps, peel, var_names, peel_levels = NULL) {
  out <- lapply(var_names, function(this_var) {
    wide_names <- names(stat)[
      grep(paste0("^", this_var, "(_se|_low|_upp|_var|_cv)?$"), names(stat))
    ]
    stat[[peel]] <- this_var
    out <- stat[, c(grps, peel, wide_names)]
    names(out)[names(out) %in% wide_names] <-
      sub(this_var, "", names(out)[names(out) %in% wide_names])

    out
  })
  out <- dplyr::bind_rows(out)
  if (!is.null(peel_levels)) {
    out[[peel]] <- factor(out[[peel]], peel_levels)
  }
  out
}


stop_direct_call <- function(func, call. = FALSE) {
  stop(func, " should not be called directly", call. = call.)
}

stop_fake_method <- function(func, class, call. = FALSE) {
  stop("no applicable method for '", func,
       "' applied to an object of class ", class,
       call. = call.)
}
