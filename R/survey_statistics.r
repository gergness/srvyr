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
#' @param level (For vartype = "ci" only) A single number or vector of numbers indicating
#'              the confidence level
#' @param proportion Use methods to calculate the proportion that may have more accurate
#'                   confidence intervals near 0 and 1. Based on
#'                   \code{\link[survey]{svyciprop}}.
#' @param prop_method Type of proportion method to use if proportion is \code{TRUE}. See
#'                    \code{\link[survey]{svyciprop}} for details.
#' @param deff A logical value to indicate whether the design effect should be returned.
#' @param df (For vartype = "ci" only) A numeric value indicating the degrees of freedom
#'           for t-distribution. The default (NULL) uses \code{\link[survey]{degf}},
#'           but Inf is the usual survey package's default (except in
#'           \code{\link[survey]{svyciprop}}.
#' @param .svy A \code{tbl_svy} object. When called from inside a summarize function
#'   the default automatically sets the survey to the current survey.
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
#' # Setting proportion = TRUE uses a different method for calculating confidence intervals
#' dstrata %>%
#'   summarise(high_api = survey_mean(api00 > 875, proportion = TRUE, vartype = "ci"))
#'
#' # level takes a vector for multiple levels of confidence intervals
#' dstrata %>%
#'   summarise(api99 = survey_mean(api99, vartype = "ci", level = c(0.95, 0.65)))
#'
#' # Note that the default degrees of freedom in srvyr is different from
#' # survey, so your confidence intervals might not be exact matches. To
#' # Replicate survey's behavior, use df = Inf
#' dstrata %>%
#'   summarise(srvyr_default = survey_mean(api99, vartype = "ci"),
#'             survey_defualt = survey_mean(api99, vartype = "ci", df = Inf))
#'
#' comparison <- survey::svymean(~api99, dstrata)
#' confint(comparison) # survey's default
#' confint(comparison, df = survey::degf(dstrata)) # srvyr's default
#'
#' @export
survey_mean <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95,
  proportion = FALSE, prop_method = c("logit", "likelihood", "asin", "beta", "mean"),
  deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  UseMethod("survey_mean", .svy)
}

#' @export
survey_mean.tbl_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95,
  proportion = FALSE, prop_method = c("logit", "likelihood", "asin", "beta", "mean"),
  deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  prop_method <- match.arg(prop_method)
  if (is.null(df)) df <- survey::degf(.svy)
  if (missing(x)) stop("Variable should be provided as an argument to survey_mean() or grouped survey object should be used.")
  stop_for_factor(x)
  if (!proportion) {
    if (is.logical(x)) x <- as.integer(x)
    .svy <- set_survey_vars(.svy, x)
    stat <- survey::svymean(~`__SRVYR_TEMP_VAR__`, .svy, na.rm = na.rm, deff = deff)
    out <- get_var_est(stat, vartype, level = level, df = df, deff = deff)
    out
  } else {
    if (!isFALSE(deff)) warning("Cannot calculate design effects on proportions.", call. = FALSE)

    .svy <- set_survey_vars(.svy, x)
    stat <- survey::svyciprop(
      ~`__SRVYR_TEMP_VAR__`, .svy, na.rm = na.rm, level = level, method = prop_method
    )
    out <- get_var_est(stat, vartype, pre_calc_ci = TRUE, df = df)
    out
  }
}

#' @export
survey_mean.grouped_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95,
  proportion = FALSE, prop_method = c("logit", "likelihood", "asin", "beta", "mean"),
  deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (is.null(df)) df <- survey::degf(.svy)

  if (missing(prop_method)) prop_method <- "logit"
  prop_method <- match.arg(prop_method, several.ok = TRUE)

  if (missing(x) & proportion) {
    stop("proportion does not work with factors.")
  } else if (missing(x)) {
    survey_stat_factor(.svy, survey::svymean, na.rm, vartype, level, deff, df)
  } else {
    stop_for_factor(x)
    if (is.logical(x)) x <- as.integer(x)
    .svy <- set_survey_vars(.svy, x)
    grps_formula <- survey::make.formula(group_vars(.svy))

    if (proportion) {
      if (!isFALSE(deff)) {
        warning("Cannot calculate design effects on proportions.", call. = FALSE)
        deff <- FALSE
      }
      stat <- survey::svyby(
        ~`__SRVYR_TEMP_VAR__`, grps_formula, .svy, survey::svyciprop, na.rm = na.rm,
        se = TRUE, vartype = c("se", "ci"), method = prop_method, level = level
      )
    } else {
      stat <- survey::svyby(
        ~`__SRVYR_TEMP_VAR__`, grps_formula, .svy, survey::svymean,
        deff = deff, na.rm = na.rm
      )
    }
    out <- get_var_est(
      stat, vartype, grps = group_vars(.svy), level = level, df = df,
      pre_calc_ci = proportion, deff = deff
    )
    out
  }
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
#' @param deff A logical value to indicate whether the design effect should be returned.
#' @param df (For vartype = "ci" only) A numeric value indicating the degrees of freedom
#'           for t-distribution. The default (NULL) uses \code{\link[survey]{degf}},
#'           but Inf is the usual survey package's default.
#' @param .svy A \code{tbl_svy} object. When called from inside a summarize function
#'   the default automatically sets the survey to the current survey.
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
#' # Note that the default degrees of freedom in srvyr is different from
#' # survey, so your confidence intervals might not exactly match. To
#' # replicate survey's behavior, use df = Inf
#' dstrata %>%
#'   summarise(srvyr_default = survey_total(api99, vartype = "ci"),
#'             survey_defualt = survey_total(api99, vartype = "ci", df = Inf))
#'
#' comparison <- survey::svytotal(~api99, dstrata)
#' confint(comparison) # survey's default
#' confint(comparison, df = survey::degf(dstrata)) # srvyr's default
#'
#' @export
survey_total <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95,
  deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  UseMethod("survey_total", .svy)
}

#' @export
survey_total.tbl_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95,
  deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }

  if (is.null(df)) df <- survey::degf(.svy)

  if (missing(x)) stop("Variable should be provided as an argument to survey_total() or grouped survey object should be used.")
  stop_for_factor(x)
  if (is.logical(x)) x <- as.integer(x)

  .svy <- set_survey_vars(.svy, x)
  stat <- survey::svytotal(~`__SRVYR_TEMP_VAR__`, .svy, na.rm = na.rm, deff = deff)

  out <- get_var_est(stat, vartype, level = level, df = df, deff = deff)
  out
}

#' @export
survey_total.grouped_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95,
  deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (is.null(df)) df <- survey::degf(.svy)

  if (missing(x)) {
    survey_stat_factor(.svy, survey::svytotal, na.rm, vartype, level, deff, df)
  } else {
    stop_for_factor(x)
    if (is.logical(x)) x <- as.integer(x)
    .svy <- set_survey_vars(.svy, x)
    grps_formula <- survey::make.formula(group_vars(.svy))

    stat <- survey::svyby(
      ~`__SRVYR_TEMP_VAR__`, grps_formula, .svy, survey::svytotal,
      deff = deff, na.rm = na.rm
    )
    out <- get_var_est(
      stat, vartype, grps = group_vars(.svy), level = level, df = df, deff = deff
    )
    out
  }
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
#' @param deff A logical value to indicate whether the design effect should be returned.
#' @param df (For vartype = "ci" only) A numeric value indicating the degrees of freedom
#'           for t-distribution. The default (NULL) uses \code{\link[survey]{degf}},
#'           but Inf is the usual survey package's default (except in
#'           \code{\link[survey]{svyciprop}}.
#' @param .svy A \code{tbl_svy} object. When called from inside a summarize function
#'   the default automatically sets the survey to the current survey.
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
#' # Note that the default degrees of freedom in srvyr is different from
#' # survey, so your confidence intervals might not exactly match. To
#' # replicate survey's behavior, use df = Inf
#' dstrata %>%
#'   summarise(srvyr_default = survey_total(api99, vartype = "ci"),
#'             survey_defualt = survey_total(api99, vartype = "ci", df = Inf))
#'
#' comparison <- survey::svytotal(~api99, dstrata)
#' confint(comparison) # survey's default
#' confint(comparison, df = survey::degf(dstrata)) # srvyr's default
#'
#' @export
survey_ratio <- function(
  numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"),
  level = 0.95, deff = FALSE, df = NULL, .svy  = current_svy(), ...
) {
  UseMethod("survey_ratio", .svy)
}

#' @export
survey_ratio.tbl_svy <- function(
  numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"),
  level = 0.95, deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (is.null(df)) df <- survey::degf(.svy)
  stop_for_factor(numerator)
  stop_for_factor(denominator)

  .svy <- set_survey_vars(.svy, numerator, "__SRVYR_TEMP_NUM__")
  .svy <- set_survey_vars(.svy, denominator, "__SRVYR_TEMP_DEN__", add = TRUE)

  stat <- survey::svyratio(
    ~`__SRVYR_TEMP_NUM__`, ~`__SRVYR_TEMP_DEN__`, .svy, na.rm = na.rm,
    deff = deff, df = df
  )

  out <- get_var_est(stat, vartype, level = level, df = df, deff = deff)
  out
}

#' @export
survey_ratio.grouped_svy <- function(
  numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"),
  level = 0.95, deff = FALSE, df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (is.null(df)) df <- survey::degf(.svy)
  stop_for_factor(numerator)
  stop_for_factor(denominator)

  grp_names <- group_vars(.svy)

  .svy <- set_survey_vars(.svy, numerator, "__SRVYR_TEMP_NUM__")
  .svy <- set_survey_vars(.svy, denominator, "__SRVYR_TEMP_DEN__", add = TRUE)

  stat <- survey::svyby(
    ~`__SRVYR_TEMP_NUM__`, survey::make.formula(grp_names), .svy,
    survey::svyratio, denominator = ~`__SRVYR_TEMP_DEN__`,
    na.rm = na.rm, ci = TRUE, deff = deff
  )

  out <- get_var_est(stat, vartype, grps = grp_names, level = level, df = df, deff = deff)
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
#' @param vartype NULL to report no variability (default), otherwise one or more of:
#'                standard error ("se") confidence interval ("ci") (variance and coefficient
#'                of variation not available).
#' @param level A single number indicating the confidence level (only one level allowed)
#' @param q_method See "method" in \code{\link[stats]{approxfun}}
#' @param f See \code{\link[stats]{approxfun}}
#' @param interval_type See \code{\link[survey]{svyquantile}}
#' @param ties See \code{\link[survey]{svyquantile}}
#' @param df A number indicating the degrees of freedom for t-distribution. The
#'           default, Inf uses the normal distribution (matches the survey package).
#'           Also, has no effect for \code{type = "betaWald"}.
#' @param .svy A \code{tbl_svy} object. When called from inside a summarize function
#'   the default automatically sets the survey to the current survey.
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
survey_quantile <- function(
  x, quantiles, na.rm = FALSE, vartype = NULL,
  level = 0.95, q_method = "linear", f = 1,
  interval_type = c("Wald", "score", "betaWald", "probability", "quantile"),
  ties = c("discrete", "rounded"), df = NULL, .svy = current_svy(), ...
) {
  UseMethod("survey_quantile", .svy)
}

#' @export
survey_quantile.tbl_svy <- function(
  x, quantiles, na.rm = FALSE, vartype = c("se", "ci"),
  level = 0.95, q_method = "linear", f = 1,
  interval_type = c("Wald", "score", "betaWald", "probability", "quantile"),
  ties = c("discrete", "rounded"), df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(interval_type) & !inherits(.svy, "svyrep.design")) interval_type <- "Wald"
  if (missing(interval_type) & inherits(.svy, "svyrep.design")) interval_type <- "probability"
  interval_type <- match.arg(interval_type, several.ok = TRUE)
  if (missing(ties)) ties <- "discrete"
  ties <- match.arg(ties, several.ok = TRUE)

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  # Because of machine precision issues, 1 - 0.95 != 0.05...
  # Here's a hacky way to force it, though it technically limits
  # us to 7 digits of precision in alpha (seems like enough,
  # we could go higher, but I worry about 32bit vs 64bit systems)
  alpha = round(1 - level, 7)

  if (missing(x)) stop("Variable should be provided as an argument to survey_quantile().")
  stop_for_factor(x)
  .svy <- set_survey_vars(.svy, x)

  # TODO: switch to improved svyquantile
  if (utils::packageVersion("survey") >= "4.1") {
    svyq_func <- get("oldsvyquantile", asNamespace("survey"))
  } else {
    svyq_func <- survey::svyquantile
  }

  stat <- svyq_func(
    ~`__SRVYR_TEMP_VAR__`, .svy, quantiles = quantiles, na.rm = na.rm,
    ci = TRUE, alpha = alpha, method = q_method, f = f,
    interval.type = interval_type, ties = ties, df = df
  )

  out <- get_var_est_quantile(stat, vartype, q = quantiles, level = level)
  out
}

#' @export
survey_quantile.grouped_svy <- function(
  x, quantiles, na.rm = FALSE, vartype = c("se", "ci"),
  level = 0.95, q_method = "linear", f = 1,
  interval_type = c("Wald", "score", "betaWald", "probability", "quantile"),
  ties = c("discrete", "rounded"), df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(interval_type) & !inherits(.svy, "svyrep.design")) interval_type <- "Wald"
  if (missing(interval_type) & inherits(.svy, "svyrep.design")) interval_type <- "probability"
  interval_type <- match.arg(interval_type, several.ok = TRUE)
  if (missing(ties)) ties <- "discrete"
  ties <- match.arg(ties, several.ok = TRUE)

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  # Because of machine precision issues, 1 - 0.95 != 0.05...
  # Here's a hacky way to force it, though it technically limits
  # us to 7 digits of precision in alpha (seems like enough,
  # we could go higher, but I worry about 32bit vs 64bit systems)
  alpha = round(1 - level, 7)

  grp_names <- group_vars(.svy)
  if (missing(x)) stop("survey_quantile() can't be used with regards to the grouping variable.")
  stop_for_factor(x)
  .svy <- set_survey_vars(.svy, x)

  # TODO: switch to improved svyquantile
  if (utils::packageVersion("survey") >= "4.1") {
    svyq_func <- get("oldsvyquantile", asNamespace("survey"))
  } else {
    svyq_func <- survey::svyquantile
  }


  stat <- survey::svyby(
    formula = ~`__SRVYR_TEMP_VAR__`, survey::make.formula(grp_names), .svy,
    svyq_func, quantiles = quantiles, na.rm = na.rm,
    ci = TRUE, alpha = alpha, method = q_method,
    f = f, interval.type = interval_type, ties = ties,
    df = df, vartype = vartype
  )

  out <- get_var_est_quantile(stat, vartype, q = quantiles, grps = grp_names, level = level)

  out
}

#' @export
#' @rdname survey_quantile
survey_median <- function(
  x, na.rm = FALSE, vartype = c("se", "ci"),
  level = 0.95, q_method = "linear", f = 1,
  interval_type = c("Wald", "score", "betaWald", "probability", "quantile"),
  ties = c("discrete", "rounded"), df = NULL, .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(interval_type) & !inherits(.svy, "svyrep.design")) interval_type <- "Wald"
  if (missing(interval_type) & inherits(.svy, "svyrep.design")) interval_type <- "probability"
  interval_type <- match.arg(interval_type, several.ok = TRUE)
  if (missing(ties)) ties <- "discrete"
  ties <- match.arg(ties, several.ok = TRUE)

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  out = survey_quantile(
    x, quantiles = 0.5, na.rm = na.rm, vartype = vartype, level = level, q_method = q_method,
    f = f, interval_type = interval_type, ties = ties, df = df, .svy = .svy
  )
  names(out) = sub("^_q50", "", names(out))
  out
}

#' Calculate the population variance and its variation using survey methods
#'
#' Calculate population variance from complex survey data. A wrapper
#' around \code{\link[survey]{svyvar}}. \code{survey_var} should always be
#' called from \code{\link{summarise}}.
#'
#' @param x A variable or expression, or empty
#' @param na.rm A logical value to indicate whether missing values should be dropped
#' @param vartype Report variability as one or more of: standard error ("se", default)
#'                or variance ("var") (confidence intervals and coefficient
#'                of variation not available).
#' @param level (For vartype = "ci" only) A single number or vector of numbers indicating
#'              the confidence level.
#' @param df (For vartype = "ci" only) A numeric value indicating the degrees of freedom
#'           for t-distribution. The default (Inf) is equivalent to using normal
#'           distribution and in case of population variance statistics there is little
#'           reason to use any other values (see \emph{Details}).
#' @param .svy A \code{tbl_svy} object. When called from inside a summarize function
#'             the default automatically sets the survey to the current survey.
#' @param ... Ignored
#' @details
#' Be aware that confidence intervals for population variance statistic are
#' computed by package \emph{survey} using \emph{t} or normal (with df=Inf)
#' distribution (i.e. symmetric distributions). \strong{This could be a very poor
#' approximation} if even one of these conditions is met:
#' \itemize{
#'   \item{there are few sampling design degrees of freedom,}
#'   \item{analyzed variable isn't normally distributed,}
#'   \item{there is huge variation in sampling probabilities of the survey design.}
#' }
#' Because of this be very careful using confidence intervals for population variance
#' statistics especially while performing analysis within subsets of data or using
#' grouped survey objects.
#'
#' Sampling distribution of the variance statistic in general is asymmetric
#' (chi-squared in case of simple random sampling of normally distributed variable)
#' and if analyzed variable isn't normally distributed or there is huge variation in
#' sampling probabilities of the survey design (or both) it could converge to
#' normality only very slowly (with growing number of survey design degrees of
#' freedom).
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99_var = survey_var(api99),
#'             api99_sd = survey_sd(api99))
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(api00_var = survey_var(api00),
#'             api00_sd = survey_sd(api00))
#'
#' # standard deviation and variance of the population variance estimator
#' # are available with vartype argument
#' # (but not for the population standard deviation estimator)
#' dstrata %>%
#'   summarise(api99_variance = survey_var(api99, vartype = c("se", "var")))
#' @export

survey_var <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var"), level = 0.95, df = NULL,
  .svy = current_svy(), ...
) {
  UseMethod("survey_var", .svy)
}

#' @export
survey_var.tbl_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var"), level = 0.95, df = NULL,
  .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(x)) stop("Variable should be provided as an argument to survey_var().")
  stop_for_factor(x)
  if (is.logical(x)) x <- as.integer(x)

  if (is.null(df)) df <- survey::degf(.svy)

  .svy <- set_survey_vars(.svy, x)
  stat <- survey::svyvar(~`__SRVYR_TEMP_VAR__`, .svy, na.rm = na.rm)

  out <- get_var_est(stat, vartype, level = level, df = df, deff = FALSE)
  out
}

#' @export
survey_var.grouped_svy <- function(
  x, na.rm = FALSE, vartype = c("se", "ci", "var"), level = 0.95, df = NULL,
  .svy = current_svy(), ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(x)) stop("survey_var() can't be used with regards to the grouping variable.")
  stop_for_factor(x)
  if (is.logical(x)) x <- as.integer(x)
  .svy <- set_survey_vars(.svy, x)
  grps_formula <- survey::make.formula(group_vars(.svy))

  if (any(dplyr::count(.svy$variables)$n < 2)) stop("Population variance can't be computed because some groups contain less than 2 observations.")

  if (is.null(df)) df <- survey::degf(.svy)

  stat <- survey::svyby(
    ~`__SRVYR_TEMP_VAR__`, grps_formula, .svy, survey::svyvar,
    na.rm = na.rm
  )
  out <- get_var_est(
    stat, vartype, grps = group_vars(.svy), level = level, df = df, deff = FALSE
  )
  out
}

#' @export
#' @rdname survey_var
survey_sd <- function(
  x, na.rm = FALSE, .svy = current_svy(), ...
) {
  out <- survey_var(
    x, na.rm = na.rm, vartype = NULL, .svy = .svy
  )
  out <- mutate(
    out,
    `__SRVYR_COEF__` = sqrt(.data$`__SRVYR_COEF__`)
  )
  out
}

#' Calculate the an unweighted summary statistic from a survey
#'
#' Calculate unweighted summaries from a survey dataset, just as on
#' a normal data.frame with \code{\link[dplyr]{summarise}}.
#'
#' Uses tidy evaluation semantics and so if you want to use
#' wrapper functions based on variable names, you must use
#' tidy evaluation, see the examples here, documentation in
#' \link[rlang]{nse-force}, or the dplyr vignette called
#' 'programming' for more information.
#'
#' @param x A variable or expression
#' @param .svy A \code{tbl_svy} object. When called from inside a summarize function
#'   the default automatically sets the survey to the current survey.
#' @param ... Ignored
#' @examples
#' library(survey)
#' library(dplyr)
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
#'
#' # If you want to use a wrapper function, be sure to treat
#' # non-standard evaluation correctly
#' umean <- function(x) {
#'   unweighted(mean({{x}}))
#' }
#'  dstrata %>%
#'    group_by(stype) %>%
#'    summarize(api_diff_unw = umean(api00 - api99))
#'
#'
#' @export
unweighted <- function(x, .svy = current_svy(), ...) {
  dots <- rlang::enquo(x)
  # unweighted needs to be evaluated in grandparent environment (in
  # the caller of summarise) because we don't want the same kind of
  # vector retrieval from the survey's variables as we do for other
  # survey statistics
  dots <- rlang::quo_set_env(dots, rlang::env_parent(n = 2))

  if (is.calibrated(.svy) | is.pps(.svy)) {
    excluded_rows <- is.infinite(.svy[['prob']])
    out <- summarize(.svy[["variables"]][!excluded_rows,], !!dots)
  } else {
    out <- summarize(.svy[["variables"]], !!dots)
  }

  names(out)[length(names(out))] <- ""
  out
}

survey_stat_factor <- function(.svy, func, na.rm, vartype, level, deff, df) {
  grps_names <- group_vars(.svy)
  .svy <- set_survey_vars(.svy, NULL)
  peel_name <- grps_names[length(grps_names)]
  grps_names <- setdiff(grps_names, peel_name)

  if (is.numeric(.svy$variables[[peel_name]])) {
    warning("Coercing ", peel_name, " to character.", call. = FALSE)
    peel_var_coerced_to_char <- TRUE
    peel_var_orig_type <- typeof(.svy$variables[[peel_name]])
    .svy$variables[[peel_name]] <- format(.svy$variables[[peel_name]], nsmall = 20, digits = 22)
  } else {
    peel_var_coerced_to_char <- FALSE
  }

  # If any groups have NAs the survey package will drop them, so convert to
  # explicit levels that will get dropped
  for (grp_var_name in c(peel_name, grps_names)) {
    if (any(is.na(.svy$variables[[grp_var_name]]))) {
      if (is.factor(.svy$variables[[grp_var_name]])) {
        levels(.svy$variables[[grp_var_name]]) <- c(levels(.svy$variables[[grp_var_name]]), "__SRVYR_NA_LEVEL__")
      }
      .svy$variables[[grp_var_name]][is.na(.svy$variables[[grp_var_name]])] <- "__SRVYR_NA_LEVEL__"
    }
  }

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  peel_is_factor <- is.factor(.svy[["variables"]][[peel_name]])
  if (peel_is_factor) {
    peel_levels <- levels(.svy[["variables"]][[peel_name]])
    peel_is_ordered <- is.ordered(.svy[["variables"]][[peel_name]])
  } else {
    peel_levels <- sort(unique(.svy[["variables"]][[peel_name]]))
    peel_is_ordered <- FALSE
  }
  if (length(grps_names) > 0) {
    stat <- survey::svyby(survey::make.formula(peel_name),
                          survey::make.formula(grps_names),
                          .svy, func, na.rm = na.rm, se = TRUE, deff = deff)

    var_names <- attr(stat, "svyby")[["variables"]]
    var_names <- unlist(lapply(var_names, function(x) substring(x, nchar(peel_name) + 1)))


    out <- get_var_est_factor(
      stat, vartype, grps = grps_names, peel = peel_name,
      peel_is_factor = peel_is_factor, peel_levels = peel_levels, peel_is_ordered = peel_is_ordered,
      level = level, df = df, deff = deff
    )
  } else {
    stat <- func(survey::make.formula(peel_name), .svy, na.rm = na.rm, deff = deff)

    out <- get_var_est_factor(
        stat, vartype, grps = "", peel = peel_name, peel_levels = peel_levels,
        peel_is_ordered = peel_is_ordered, peel_is_factor = peel_is_factor,
        df = df, deff = deff
    )
  }

  # set back to NA the NAs that were forced to be explicit
  for (grp_var_name in c(peel_name, grps_names)) {
    out[[grp_var_name]][out[[grp_var_name]] == "__SRVYR_NA_LEVEL__"] <- NA
  }

  if (peel_var_coerced_to_char) {
    out[[peel_name]] <- methods::as(out[[peel_name]], peel_var_orig_type)
  }

  out
}
