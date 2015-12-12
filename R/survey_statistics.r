#' @export
survey_mean <- function(.svy, x, na.rm, vartype = c("se", "ci", "var", "cv"),
                        level = 0.95, proportion = FALSE,
                        prop_method = c("logit", "likelihood", "asin", "beta", "mean")) {
  UseMethod("survey_mean")
}

survey_mean.tbl_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"),
                                level = 0.95, proportion = FALSE,
                                prop_method = c("logit", "likelihood", "asin", "beta", "mean")) {
  if (missing(vartype)) vartype <- "se"

  if (!proportion) {
    survey_stat_ungrouped(.svy, survey::svymean, x, na.rm, vartype, level)
  } else {
    # survey::ciprop only accepts formulas so can't use main function
    survey_stat_proportion(.svy, x, na.rm, vartype, level, prop_method)
  }
}

survey_mean.grouped_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"),
                                    level = 0.95, proportion = FALSE,
                                    prop_method = c("logit", "likelihood", "asin", "beta", "mean")) {
  if (missing(vartype)) vartype <- "se"
  if (missing(x)) {
    survey_stat_factor(.svy, survey::svymean, na.rm, vartype, level)
  } else if (proportion) {
    survey_stat_grouped(.svy, survey::svyciprop, x, na.rm, vartype, level, prop_method)
  } else survey_stat_grouped(.svy, survey::svymean, x, na.rm, vartype, level)
}


#' @export
survey_total <- function(.svy, x, na.rm, vartype = c("se", "ci", "var", "cv"), level = 0.95) {
  UseMethod("survey_total")
}

survey_total.tbl_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95) {
  if (missing(vartype)) vartype <- "se"
  survey_stat_ungrouped(.svy, survey::svytotal, x, na.rm, vartype, level)
}

survey_total.grouped_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95) {
  if (missing(vartype)) vartype <- "se"
  if (!missing(x)) survey_stat_grouped(.svy, survey::svytotal, x, na.rm, vartype, level)
  else survey_stat_factor(.svy, survey::svytotal, na.rm, vartype)
}


#' @export
survey_ratio <- function(.svy, numerator, denominator, na.rm, vartype = c("se", "ci", "var", "cv"), level = 0.95) {
  UseMethod("survey_ratio")
}

survey_ratio.tbl_svy <- function(.svy, numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))

  stat <- survey::svyratio(data.frame(numerator), data.frame(denominator), .svy, na.rm = na.rm)

  out <- get_var_est(stat, vartype, level = level)
  out
}

survey_ratio.grouped_svy <- function(.svy, numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), level = 0.95) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c(match.arg(vartype, several.ok = TRUE))

  grps <- survey::make.formula(groups(.svy))

  # svyby breaks when you feed it raw vector to be measured... Add it to
  # the data.frame with mutate and then pass in the name
  .svy$variables[["___numerator"]] <- numerator
  .svy$variables[["___denominator"]] <- denominator

  # Slight hack for twophase -- move the created variables to where survey expects them
  if (inherits(.svy, "twophase2")) {
    .svy$phase1$sample$variables <- .svy$variables
  }

  stat <- survey::svyby(~`___numerator`, grps, .svy, survey::svyratio,
                       denominator = ~`___denominator`,
                       na.rm = na.rm, ci = TRUE)

  vartype <- c("grps", "coef", vartype)

  out <- get_var_est(stat, vartype, grps = as.character(groups(.svy)), level = level)

  out
}

#' @export
survey_quantile <- function(.svy, x, quantiles, na.rm = FALSE, vartype = c("none", "se", "ci"), level = 0.95) {
  UseMethod("survey_quantile")
}

survey_quantile.tbl_svy <- function(.svy, x, quantiles, na.rm = FALSE, vartype = c("none", "se", "ci"), level = 0.95) {
  if(missing(vartype)) vartype <- "none"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))

  vartype <- setdiff(vartype, "none")

  stat <- survey::svyquantile(data.frame(x), .svy,
                      quantiles = quantiles, na.rm = na.rm,
                      ci = TRUE, level = level)

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2, flag = "0")))

  out <- get_var_est(stat, vartype, var_names = q_text, level = level, quantile = TRUE)
  out
}

survey_quantile.grouped_svy <- function(.svy, x, quantiles, na.rm = FALSE, vartype = c("none", "se", "ci"), level = 0.95) {
  if(missing(vartype)) vartype <- "none"
  vartype <- match.arg(vartype, several.ok = TRUE)
  vartype <- setdiff(vartype, "none")

  grps <- survey::make.formula(groups(.svy))

  .svy$variables[["___arg"]] <- x

  # Slight hack for twophase -- move the created variables to where survey expects them
  if (inherits(.svy, "twophase2")) {
    .svy$phase1$sample$variables <- .svy$variables
  }

  stat <- survey::svyby(~`___arg`, grps, .svy, survey::svyquantile,
                      quantiles = quantiles, na.rm = na.rm,
                      ci = TRUE, level = level)

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2, flag = "0")))
  vartype <- c("grps", "coef", vartype)
  out <- get_var_est(stat, vartype, var_names = q_text, grps = as.character(groups(.svy)),
                     level = level)

  out
}


#' @export
survey_median <- function(.svy, x, na.rm = FALSE, vartype = c("none", "se", "ci"), level = 0.95) {
  UseMethod("survey_median")
}

survey_median.default <- function(.svy, x, na.rm = FALSE, vartype = c("none", "se", "ci"), level = 0.95) {
  if (missing(vartype)) vartype <- "none"

  survey_quantile(.svy, x, quantiles = 0.5, na.rm = na.rm, vartype = vartype, level = level)
}


#' @export
unweighted <- function(.svy, x) {
  UseMethod("unweighted")
}

unweighted.default <- function(.svy, x) {
  dots <- lazyeval::lazy(x)

  out <- summarize_(.svy[["variables"]], .dots = dots)
  names(out)[length(names(out))] <- ""
  out
}


survey_stat_ungrouped <- function(.svy, func, x, na.rm, vartype, level) {
  if (class(x) == "factor") stop("Factor not allowed in survey functions, should be used as a grouping variable")
  if (class(x) == "logical") x <- as.integer(x)
  stat <- func(data.frame(x), .svy, na.rm = na.rm)

  vartype <- c("coef", vartype)
  out <- get_var_est(stat, vartype, level = level)

  out
}

survey_stat_grouped <- function(.svy, func, x, na.rm, vartype, level, prop_method = NULL) {
  grps <- survey::make.formula(groups(.svy))

  if (class(x) == "factor") stop("Factor not allowed in survey functions, should be used as a grouping variable")
  if (class(x) == "logical") x <- as.integer(x)
  # svyby breaks when you feed it raw vector to be measured... Add it to
  # the data.frame with mutate and then pass in the name
  .svy$variables[["___arg"]] <- x

  # Slight hack for twophase -- move the created variables to where survey expects them
  if (inherits(.svy, "twophase2")) {
    .svy$phase1$sample$variables <- .svy$variables
  }
  vartype <- c("grps", "coef", vartype)

  if (is.null(prop_method)) {
    stat <- survey::svyby(~`___arg`, grps, .svy, func, na.rm = na.rm, se = TRUE)
  } else {
    vartype[vartype == "ci"] <- "ci-prop"
    stat <- survey::svyby(~`___arg`, grps, .svy, func, na.rm = na.rm, se = TRUE, vartype = c("ci", "se"),
                          method = prop_method)
  }

  out <- get_var_est(stat, vartype, grps = as.character(groups(.svy)), level = level)
  out
}

survey_stat_factor <- function(.svy, func, na.rm, vartype, level) {
  grps <- as.character(groups(.svy))
  peel <- grps[length(grps)]
  grps <- setdiff(grps, peel)

  vartype <- c("coef", vartype)

  if (length(grps) > 0) {
    stat <- survey::svyby(survey::make.formula(peel),
                          survey::make.formula(grps),
                          .svy, func, na.rm = na.rm, se = TRUE)

    var_names <- attr(stat, "svyby")[["variables"]]
    var_names <- unlist(lapply(var_names, function(x) substring(x, nchar(peel)+1)))

    vartype <- c("grps", vartype)

    out <- get_var_est(stat, vartype, var_names = var_names, grps = grps, level = level)
    out <- factor_stat_reshape(out, grps, peel, var_names)

    out
  } else {
    vartype <- c("lvls", vartype) # Needed because grouped don't usually have "coef"
    stat <- func(survey::make.formula(peel), .svy, na.rm = na.rm)

    out <- get_var_est(stat, vartype, peel = peel,
                       peel_class = class(.svy[["variables"]][[peel]]))

    out
  }
}

survey_stat_proportion <- function(.svy, x, na.rm, vartype, level, prop_method) {
  .svy$variables["___arg"] <- x
  stat <- survey::svyciprop(~`___arg`, .svy, na.rm = na.rm, level = level, method = prop_method)

  vartype <- c("coef", vartype)

  out <- get_var_est(stat, vartype, quantile = TRUE)
  out
}

get_var_est <- function(stat, vartype, var_names = "", grps = "",
                        peel = "", peel_class = NULL, level = 0.95,
                        quantile = FALSE) {
  out_width <- length(var_names)
  out <- lapply(vartype, function(vvv) {
    if (vvv == "coef") {
      coef <- data.frame(matrix(coef(stat), ncol = out_width))
      names(coef) <- var_names
      coef
    } else if (vvv == "se") {
      se <- survey::SE(stat)
      if (!inherits(se, "data.frame")) se <- data.frame(matrix(se, ncol = out_width)) # Needed for grouped quantile
      names(se) <- paste0(var_names, "_se")
      se
    } else if (vvv == "ci") {
      if (!quantile) {
        ci <- data.frame(matrix(confint(stat, level = level), ncol = 2 * out_width))
      } else {
        ci <- data.frame(matrix(confint(stat), ncol = 2 * out_width))
      }
      names(ci) <- c(paste0(var_names, "_low"), paste0(var_names, "_upp"))
      ci
    } else if (vvv == "ci-prop") {
      ci <- data.frame(stat[c("ci_l", "ci_u")])
      names(ci) <- c(paste0(var_names, "_low"), paste0(var_names, "_upp"))
      ci
    } else if (vvv == "var") {
      var <- data.frame(matrix(survey::SE(stat)^2, ncol = out_width))
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
      lvls <- data.frame(names(coef(stat)))
      levels(lvls[[1]]) <- gsub(paste0("^", peel), "", levels(lvls[[1]]))
      if (peel_class == "character") lvls[[1]] <- as.character(lvls[[1]])
      names(lvls) <- peel
      lvls
    }
  })
  dplyr::bind_cols(out)
}

# base reshape was difficult to work with, but might be worth reinvestigating
# (or others, but have to weight the cost of extra dependency). This takes the survey stat
# object that has the peel variable wide, and makes it long.
factor_stat_reshape <- function(stat, grps, peel, var_names) {
  out <- lapply(var_names, function(this_var) {
    wide_names <- names(stat)[grep(paste0("^", this_var, "(_se|_low|_upp|_var|_cv)?$"), names(stat))]
    stat[[peel]] <- this_var
    out <- stat[, c(grps, peel, wide_names)]
    names(out)[names(out) %in% wide_names] <- sub(this_var, "",  names(out)[names(out) %in% wide_names])

    out
  })
  dplyr::bind_rows(out)
}
