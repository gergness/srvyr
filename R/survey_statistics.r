#' Calculate mean/proportion and its variation using survey methods
#'
#' Calculate means and proportions from complex survey data.
#' \code{survey_mean} with \code{proportion = FALSE} (the default) or \code{survey_prop} with \code{proportion = FALSE}
#' is a wrapper around \code{\link[survey]{svymean}}.
#' \code{survey_prop} with \code{proportion = TRUE} (the default) or \code{survey_mean} with \code{proportion = TRUE}
#' is a wrapper around \code{\link[survey]{svyciprop}}.
#' \code{survey_mean} and \code{survey_prop} should always be called from \code{\link{summarise}}.
#'
#' Using \code{survey_prop} is equivalent to leaving out the \code{x} argument in
#' \code{survey_mean} and setting \code{proportion = TRUE} and this calculates the proportion represented within the
#' data, with the last grouping variable "unpeeled". \code{\link{interact}}
#' allows for "unpeeling" multiple variables at once.
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
#' @param ... Ignored
#' @examples
#' data(api, package = "survey")
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99_mn = survey_mean(api99),
#'             api_diff = survey_mean(api00 - api99, vartype = c("ci", "cv")))
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(api00 = survey_mean(api00))
#'
#' # Use `survey_prop` calculate the proportion in each group
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(pct = survey_prop())
#'
#' # Or you can also leave  out `x` in `survey_mean`, so this is equivalent
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(pct = survey_mean())
#'
#' # When there's more than one group, the last group is "peeled" off and proportions are
#' # calculated within that group, each adding up to 100%.
#' # So in this example, the sum of prop is 200% (100% for awards=="Yes" &
#' # 100% for awards=="No")
#' dstrata %>%
#'   group_by(stype, awards) %>%
#'   summarize(prop = survey_prop())
#'
#' # The `interact` function can help you calculate the proportion over
#' # the interaction of two or more variables
#' # So in this example, the sum of prop is 100%
#' dstrata %>%
#'   group_by(interact(stype, awards)) %>%
#'   summarize(prop = survey_prop())
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
  x,
  na.rm = FALSE,
  vartype = c("se", "ci", "var", "cv"),
  level = 0.95,
  proportion = FALSE,
  prop_method = c("logit", "likelihood", "asin", "beta", "mean", "xlogit"),
  deff = FALSE,
  df = NULL,
  ...
) {
  .svy <- cur_svy()

  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  prop_method <- match.arg(prop_method)
  if (is.null(df)) df <- survey::degf(cur_svy_full())
  if (missing(x)){
    return(survey_prop(vartype = vartype, level = level,
                       proportion = proportion, prop_method = prop_method,
                       deff = deff, df = df, .svy = cur_svy()))
  }
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

#' @rdname survey_mean
#' @export
survey_prop <- function(
  vartype = c("se", "ci", "var", "cv"),
  level = 0.95,
  proportion = TRUE,
  prop_method = c("logit", "likelihood", "asin", "beta", "mean", "xlogit"),
  deff = FALSE,
  df = NULL,
  ...
) {
  .svy <- cur_svy()
  .full_svy <- cur_svy_full()

  if (missing(proportion) & ("ci" %in% vartype)){
    inform(
      c(
        "When `proportion` is unspecified, `survey_prop()` now defaults to `proportion = TRUE`.",
        i = "This should improve confidence interval coverage."
      ),
      .frequency = "once", .frequency_id="spd"
    )
  }

  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  prop_method <- match.arg(prop_method)
  if (is.null(df)) df <- survey::degf(.full_svy)

  if (!proportion) {
    # TODO: This could be faster if we actually use survey::svyby()
    x <- peeled_cur_group_id(.full_svy, cur_group())
    .full_svy <- set_survey_vars(.full_svy, x)

    stat <- survey::svymean(~`__SRVYR_TEMP_VAR__`, .full_svy, na.rm = TRUE, deff = deff)
    out <- get_var_est(stat, vartype, level = level, df = df, deff = deff)
    out
  } else {
    if (!isFALSE(deff)) warning("Cannot calculate design effects on proportions.", call. = FALSE)
    # (Not possible to use svyby here, so I think this is as fast as it can be)
    x <- peeled_cur_group_id(.full_svy, cur_group())
    .full_svy <- set_survey_vars(.full_svy, x)

    stat <- survey::svyciprop(
      ~`__SRVYR_TEMP_VAR__`, .full_svy, na.rm = TRUE, level = level, method = prop_method
    )
    out <- get_var_est(stat, vartype, pre_calc_ci = TRUE, df = df)
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
#' @param ... Ignored
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(enroll_tot = survey_total(enroll),
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
  x,
  na.rm = FALSE,
  vartype = c("se", "ci", "var", "cv"),
  level = 0.95,
  deff = FALSE,
  df = NULL,
  ...
) {
  .svy <- cur_svy()

  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }

  if (is.null(df)) df <- survey::degf(cur_svy_full())

  if (missing(x)) {
    x <- rep(1L, nrow(.svy))
  }
  stop_for_factor(x)
  if (is.logical(x)) x <- as.integer(x)

  .svy <- set_survey_vars(.svy, x)
  stat <- survey::svytotal(~`__SRVYR_TEMP_VAR__`, .svy, na.rm = na.rm, deff = deff)

  out <- get_var_est(stat, vartype, level = level, df = df, deff = deff)
  out
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
  numerator,
  denominator,
  na.rm = FALSE,
  vartype = c("se", "ci", "var", "cv"),
  level = 0.95,
  deff = FALSE,
  df = NULL,
  ...
) {
  .svy <- cur_svy()

  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (is.null(df)) df <- survey::degf(cur_svy_full())
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

#' Calculate the quantile and its variation using survey methods
#'
#' Calculate quantiles from complex survey data. A wrapper
#' around \code{\link[survey]{svyquantile}}. \code{survey_quantile} and
#' \code{survey_median} should always be called from \code{\link{summarise}}.
#'
#' Note that the behavior of these functions has changed in srvyr version 1.1,
#' but the old functions are still (currently) supported as
#' \code{\link{survey_old_quantile}} and \code{survey_old_median} if you need
#' to replicate the old results. For more details about what has changed, see
#' Thomas Lumley's blog post on the changes, available here:
#' <https://notstatschat.rbind.io/2021/07/20/what-s-new-in-the-survey-package/>
#'
#' @param x A variable or expression
#' @param na.rm A logical value to indicate whether missing values should be dropped
#' @param quantiles A vector of quantiles to calculate
#' @param vartype NULL to report no variability. Otherwise one or more of: standard error ("se", the default),
#'                confidence interval ("ci"), variance ("var") or coefficient of variation
#'                ("cv").
#' @param level A single number indicating the confidence level (only one level allowed). Note that this may effect estimated standard errors (see \code{\link[survey]{svyquantile}} details on \code{alpha}, which equals \code{1-level}).
#' @param interval_type See \code{\link[survey]{svyquantile}}. Note that \code{interval_type = "quantile"} is only available for replicate designs, and \code{interval_type = "score"} is unavailable for replicate designs.
#' @param qrule See \code{\link[survey]{svyquantile}}
#' @param df A number indicating the degrees of freedom for t-distribution. The
#'           default, NULL, uses the design degrees of freedom (matches the survey package).
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
  x,
  quantiles,
  na.rm = FALSE,
  vartype = c("se", "ci", "var", "cv"),
  level = 0.95,
  interval_type = c("mean", "beta","xlogit", "asin", "score", "quantile"),
  qrule = c("math","school","shahvaish","hf1","hf2","hf3",
            "hf4","hf5","hf6","hf7","hf8","hf9"),
  df = NULL,
  ...
) {
  .svy <- cur_svy()

  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(interval_type)) interval_type <- "mean"
  interval_type <- match.arg(interval_type, several.ok = TRUE)
  if (missing(qrule)) qrule <- "math"

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

  stat <- survey::svyquantile(
    x = ~`__SRVYR_TEMP_VAR__`, design = .svy,
    quantiles = quantiles, na.rm = na.rm,
    ci = TRUE, alpha = alpha,
    interval.type = interval_type, qrule = qrule, df = df
  )

  out <- get_var_est_quantile(stat, vartype, q = quantiles, level = level)
  out
}

#' @export
#' @rdname survey_quantile
survey_median <- function(
  x,
  na.rm = FALSE,
  vartype = c("se", "ci", "var", "cv"),
  level = 0.95,
  interval_type = c("mean", "beta","xlogit", "asin", "score", "quantile"),
  qrule = c("math","school","shahvaish","hf1","hf2","hf3",
            "hf4","hf5","hf6","hf7","hf8","hf9"),
  df = NULL,
  ...
) {
  .svy <- cur_svy()

  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(interval_type)) interval_type <- "mean"
  interval_type <- match.arg(interval_type, several.ok = TRUE)
  if (missing(qrule)) qrule <- "math"

  if (length(level) > 1) {
    warning("Only the first confidence level will be used")
    level <- level[1]
  }

  out <- survey_quantile(
    x = x, quantiles = 0.5, na.rm = na.rm,
    vartype = vartype, ci = TRUE, level = level,
    qrule = qrule, interval_type = interval_type, df = df
  )
  names(out) = sub("^_q50", "", names(out))
  out
}

#' Calculate the quantile and its variation using survey methods
#'
#' Calculate quantiles from complex survey data. A wrapper
#' around \code{\link[survey]{oldsvyquantile}}, which is a version of the function
#' from before version 4.1 of the survey package, available for backwards compatibility.
#' \code{survey_old_quantile} and \code{survey_old_median} should always be
#' called from \code{\link{summarise}}. See Thomas Lumley's blogpost
#' <https://notstatschat.rbind.io/2021/07/20/what-s-new-in-the-survey-package/>
#' for more details.
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
#' @param interval_type See \code{\link[survey]{oldsvyquantile}}
#' @param ties See \code{\link[survey]{oldsvyquantile}}
#' @param df A number indicating the degrees of freedom for t-distribution. The
#'           default, Inf uses the normal distribution (matches the survey package).
#'           Also, has no effect for \code{type = "betaWald"}.
#' @param ... Ignored
#' @examples
#' library(survey)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'   as_survey_design(strata = stype, weights = pw)
#'
#' dstrata %>%
#'   summarise(api99 = survey_old_quantile(api99, c(0.25, 0.5, 0.75)),
#'             api00 = survey_old_median(api00, vartype = c("ci")))
#'
#' dstrata %>%
#'   group_by(awards) %>%
#'   summarise(api00 = survey_old_median(api00))
#'
#' @export
survey_old_quantile <- function(
  x,
  quantiles,
  na.rm = FALSE,
  vartype = c("se", "ci", "var", "cv"),
  level = 0.95,
  q_method = "linear",
  f = 1,
  interval_type = c("Wald", "score", "betaWald", "probability", "quantile"),
  ties = c("discrete", "rounded"),
  df = NULL,
  ...
) {
  .svy <- cur_svy()

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

  stat <- survey::oldsvyquantile(
    ~`__SRVYR_TEMP_VAR__`, .svy, quantiles = quantiles, na.rm = na.rm,
    ci = TRUE, alpha = alpha, method = q_method, f = f,
    interval.type = interval_type, ties = ties, df = df
  )

  out <- get_var_est_quantile(stat, vartype, q = quantiles, level = level)
  out
}

#' @export
#' @rdname survey_old_quantile
survey_old_median <- function(
  x,
  na.rm = FALSE,
  vartype = c("se", "ci"),
  level = 0.95,
  q_method = "linear",
  f = 1,
  interval_type = c("Wald", "score", "betaWald", "probability", "quantile"),
  ties = c("discrete", "rounded"),
  df = NULL,
  ...
) {
  .svy <- cur_svy()

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

  out <- survey_old_quantile(
    x, quantiles = 0.5, na.rm = na.rm, vartype = vartype, level = level, q_method = q_method,
    f = f, interval_type = interval_type, ties = ties, df = df
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
  x,
  na.rm = FALSE,
  vartype = c("se", "ci", "var"),
  level = 0.95,
  df = NULL,
  ...
) {
  .svy <- cur_svy()

  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }
  if (missing(x)) stop("Variable should be provided as an argument to survey_var().")
  stop_for_factor(x)
  if (length(x) < 2) stop("Population variance can't be computed because some groups contain less than 2 observations.")
  if (is.logical(x)) x <- as.integer(x)

  if (is.null(df)) df <- survey::degf(cur_svy_full())

  .svy <- set_survey_vars(.svy, x)
  stat <- survey::svyvar(~`__SRVYR_TEMP_VAR__`, .svy, na.rm = na.rm)

  out <- get_var_est(stat, vartype, level = level, df = df, deff = FALSE)
  out
}

#' @export
#' @rdname survey_var
survey_sd <- function(
  x, na.rm = FALSE, ...
) {
  out <- survey_var(x, na.rm = na.rm, vartype = NULL)
  out <- mutate(
    out,
    coef = sqrt(.data$coef)
  )
  out
}

#' Calculate the an unweighted summary statistic from a survey
#'
#' Calculate unweighted summaries from a survey dataset, just as on
#' a normal data.frame with \code{\link[dplyr]{summarise}}. Though it is
#' possible to use regular functions directly, because the survey package
#' doesn't always remove rows when filtering (instead setting the weight to 0),
#' this can sometimes give bad results. See examples for more details.
#'
#' Uses tidy evaluation semantics and so if you want to use
#' wrapper functions based on variable names, you must use
#' tidy evaluation, see the examples here, documentation in
#' \link[rlang]{nse-force}, or the dplyr vignette called
#' 'programming' for more information.
#'
#' @param ... variables or expressions, calculated on the unweighted data.frame
#' behind the \code{tbl_svy} object.
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
#' # Some survey designs, like ones with raked weights, are not removed
#' # when filtered to preserve the structure. So if you don't use `unweighted()`
#' # your results can be wrong.
#' # Declare basic clustered design ----
#' cluster_design <- as_survey_design(
#'   .data = apiclus1,
#'   id = dnum,
#'   weights = pw,
#'   fpc = fpc
#' )
#'
#' # Add raking weights for school type ----
#' pop.types <- data.frame(stype=c("E","H","M"), Freq=c(4421,755,1018))
#' pop.schwide <- data.frame(sch.wide=c("No","Yes"), Freq=c(1072,5122))
#'
#' raked_design <- rake(
#'   cluster_design,
#'   sample.margins = list(~stype,~sch.wide),
#'   population.margins = list(pop.types, pop.schwide)
#' )
#'
#' raked_design %>%
#' filter(cname != "Alameda") %>%
#'   group_by(cname) %>%
#'   summarize(
#'     direct_unw_mean = mean(api99),
#'     wrapped_unw_mean = unweighted(mean(api99))
#'   ) %>%
#'   filter(cname == "Alameda")
#'
#' # Notice how the results are different when using `unweighted()`
#'
#' @export
unweighted <- function(...) {
  .svy <- cur_svy()

  dots <- rlang::enquos(...)

  # --- Use reframe because it works when caller is summarise & reframe
  if (is.calibrated(.svy) | is.pps(.svy)) {
    excluded_rows <- is.infinite(.svy[['prob']])
    out <- reframe(.svy[["variables"]][!excluded_rows,], !!!dots)
  } else {
    out <- reframe(.svy[["variables"]], !!!dots)
  }

  # don't use default dplyr names for dots (but if explicitly named then do)
  names(out) <- ifelse(names(dots) == "", "", names(out))
  as_srvyr_result_df(out)
}


#' Calculate correlation and its variation using survey methods
#'
#' Calculate correlation from complex survey data. A wrapper
#' around \code{\link[survey]{svyvar}}. \code{survey_corr} should always be
#' called from \code{\link{summarise}}. Note this is Pearson's correlation.
#'
#' @param x A variable or expression
#' @param y A variable or expression
#' @param na.rm A logical value to indicate whether missing values should be dropped
#' @param vartype NULL to report no variability. Otherwise one or more of: standard error ("se", the default), confidence interval ("ci"), variance ("var") or coefficient of variation ("cv").
#' @param level (For vartype = "ci" only) A single number or vector of numbers indicating the confidence level
#' @param df 	(For vartype = "ci" only) A numeric value indicating the degrees of freedom for t-distribution. The default (NULL) uses degf, but Inf is the usual survey package's default
#' @param ... Ignored
#' @examples
#' data('api', package = 'survey')
#'
#' apisrs %>%
#'   as_survey_design(.ids = 1) %>%
#'   summarize(api_corr = survey_corr(x = api00, y = api99))
#'
#' apisrs %>%
#'   as_survey_design(.ids = 1) %>%
#'   group_by(sch.wide) %>%
#'   summarize(
#'     api_emer_corr = survey_corr(x = api00, y = emer, na.rm=TRUE, vartype="ci")
#'   )
#' @export
survey_corr <- function(
    x, y, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level=0.95, df=NULL, ...
) {
  if (!is.null(vartype)) {
    vartype <- if (missing(vartype)) "se" else match.arg(vartype, several.ok = TRUE)
  }


  # Add the necessary variables to the survey design
  .svy <- srvyr::set_survey_vars(srvyr::cur_svy(), x,
                                 name = "__SRVYR_TEMP_VAR_X__")
  .svy <- srvyr::set_survey_vars(.svy, y,
                                 name = "__SRVYR_TEMP_VAR_Y__",
                                 add = TRUE)

  # Set up df
  if (is.null(df)) df <- survey::degf(.svy)

  # Get point estimates for population variances and covariance
  if (inherits(.svy, 'svyrep.design')) {
    var_est <- survey::svyvar(
      x = ~ `__SRVYR_TEMP_VAR_X__` + `__SRVYR_TEMP_VAR_Y__`,
      na.rm = na.rm, design = .svy, return.replicates = TRUE)
  } else {
    var_est <- survey::svyvar(
      x = ~ `__SRVYR_TEMP_VAR_X__` + `__SRVYR_TEMP_VAR_Y__`,
      na.rm = na.rm, design = .svy)
  }

  # Turn matrix of point estimates into a vector
  # (and deduplicate)
  point_estimates <- as.vector(stats::coef(var_est)[c(1,2,4)])
  names(point_estimates) <- c('var_x', 'cov_xy', 'var_y')
  # Obtain sampling variance-covariance matrix of the point estimates
  vcov_mat <- stats::vcov(var_est)[c(1,2,4), c(1,2,4)]
  rownames(vcov_mat) <- names(point_estimates)
  colnames(vcov_mat) <- names(point_estimates)
  class(point_estimates) <- "svystat"
  attr(point_estimates, 'var') <- vcov_mat
  attr(point_estimates, 'statistic') <- "covariances"

  # Wrap it up into a 'svystat' object
  if (inherits(.svy, 'svyrep.design')) {
    svstat <- list(
      'covariances' = point_estimates,
      'replicates' = (var_est$replicates[,c(1,2,4)]) |>
        `colnames<-`(value = names(point_estimates))
    )
    attr(svstat$replicates, 'scale') <- attr(var_est$replicates, 'scale')
    attr(svstat$replicates, 'rscales') <- attr(var_est$replicates, 'rscales')
    attr(svstat$replicates, 'mse') <- attr(var_est$replicates, 'mse')
    class(svstat) <- "svrepstat"
  } else {
    svstat <- point_estimates
  }


  # Estimate correlation and use Delta Method to obtain standard error
  out <- survey::svycontrast(
    stat = svstat, contrasts = list(
      'corr' = quote(
        cov_xy / sqrt(var_x * var_y)
      )
    ))

  # Wrap up into a 'srvyr' object
  out <- srvyr::get_var_est(out, vartype, df=df, level=level)
  srvyr::as_srvyr_result_df(out)
}
