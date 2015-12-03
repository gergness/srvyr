#' @export
survey_mean <- function(.svy, x, na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("survey_mean")
}

survey_mean.tbl_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if (missing(vartype)) vartype <- "se"
  survey_stat_ungrouped(.svy, survey::svymean, x, na.rm, vartype)
}

survey_mean.grouped_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if (missing(vartype)) vartype <- "se"
  if (!missing(x)) survey_stat_grouped(.svy, survey::svymean, x, na.rm, vartype)
  else survey_stat_factor(.svy, survey::svymean, na.rm, vartype)
}


#' @export
survey_total <- function(.svy, x, na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("survey_total")
}

survey_total.tbl_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if (missing(vartype)) vartype <- "se"
  survey_stat_ungrouped(.svy, survey::svytotal, x, na.rm, vartype)
}

survey_total.grouped_svy <- function(.svy, x, na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if (missing(vartype)) vartype <- "se"
  if (!missing(x)) survey_stat_grouped(.svy, survey::svytotal, x, na.rm, vartype)
  else survey_stat_factor(.svy, survey::svytotal, na.rm, vartype)
}


#' @export
survey_ratio <- function(.svy, numerator, denominator, na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("survey_ratio")
}

survey_ratio.tbl_svy <- function(.svy, numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))

  stat <- survey::svyratio(data.frame(numerator), data.frame(denominator), .svy, na.rm = na.rm)

  out <- get_var_est(stat, vartype)
  out
}

survey_ratio.grouped_svy <- function(.svy, numerator, denominator, na.rm = FALSE, vartype = c("se", "ci", "var")) {
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

  out <- survey::svyby(~`___numerator`, grps, .svy, survey::svyratio,
                       denominator = ~`___denominator`,
                       na.rm = na.rm, vartype = vartype)

  # Format it nicely
  out <- dplyr::tbl_df(as.data.frame(out))
  names(out)[names(out) == "___numerator/___denominator"] <- ""
  names(out)[names(out) == "se.___numerator/___denominator"] <- "_se"
  names(out)[names(out) == "ci_l"] <- "_low"
  names(out)[names(out) == "ci_u"] <- "_upp"
  names(out)[names(out) == "var.___numerator/___denominator"] <- "_var"

  out
}

#' @export
survey_quantile <- function(.svy, x, quantiles, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  UseMethod("survey_quantile")
}

survey_quantile.tbl_svy <- function(.svy, x, quantiles, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  if(missing(vartype)) vartype <- "none"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))

  vartype <- setdiff(vartype, "none")

  stat <- survey::svyquantile(data.frame(x), .svy,
                      quantiles = quantiles, na.rm = na.rm,
                      ci = TRUE)

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2, flag = "0")))

  out <- get_var_est(stat, vartype, var_names = q_text)
  out
}

survey_quantile.grouped_svy <- function(.svy, x, quantiles, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  if(missing(vartype)) vartype <- "none"
  vartype <- match.arg(vartype, several.ok = TRUE)
  vartype <- setdiff(vartype, "none")

  grps <- survey::make.formula(groups(.svy))

  .svy$variables[["___arg"]] <- x

  # Slight hack for twophase -- move the created variables to where survey expects them
  if (inherits(.svy, "twophase2")) {
    .svy$phase1$sample$variables <- .svy$variables
  }

  out <- survey::svyby(~`___arg`, grps, .svy, survey::svyquantile,
                      quantiles = quantiles, na.rm = na.rm,
                      ci = TRUE, vartype = c(vartype, "se"))

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2, flag = "0")))
  # Format it nicely
  out <- dplyr::tbl_df(as.data.frame(out))
  names(out)[length(groups(.svy)) + seq_along(q_text)] <- q_text
  if ("se" %in% vartype) {
    names(out)[length(names(out)) - (rev(seq_along(q_text)) - 1)] <- paste0(q_text, "_se")
  } else {
    out <- out[-(length(names(out)) - (rev(seq_along(q_text)) - 1))]
  }

  if ("ci" %in% vartype) {
    names(out)[grep("^ci_l", names(out))] <- paste0(q_text, "_low")
    names(out)[grep("^ci_u", names(out))] <- paste0(q_text, "_upp")
  }

  out
}


#' @export
survey_median <- function(.svy, x, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  UseMethod("survey_median")
}

survey_median.default <- function(.svy, x, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  if (missing(vartype)) vartype <- "none"

  survey_quantile(.svy, x, quantiles = 0.5, na.rm = na.rm, vartype = vartype)
}


#' @export
unweighted <- function(.svy, x, na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("unweighted")
}

unweighted.default <- function(.svy, x) {
  dots <- lazyeval::lazy(x)

  out <- summarize_(.svy[["variables"]], .dots = dots)
  names(out)[length(names(out))] <- ""
  out
}


survey_stat_ungrouped <- function(.svy, func, x, na.rm, vartype) {
  if (class(x) == "factor") stop("Factor not allowed in survey functions, should be used as a grouping variable")
  if (class(x) == "logical") x <- as.integer(x)
  stat <- func(data.frame(x), .svy, na.rm = na.rm)

  vartype <- c("coef", vartype)
  out <- get_var_est(stat, vartype)

  out
}

survey_stat_grouped <- function(.svy, func, x, na.rm, vartype ) {
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

  out <- survey::svyby(~`___arg`, grps, .svy, func, na.rm = na.rm, vartype = vartype)

  # Format it nicely
  out <- dplyr::tbl_df(as.data.frame(out))
  names(out)[names(out) == "`___arg`"] <- ""
  names(out)[names(out) == "se"] <- "_se"
  names(out)[names(out) == "ci_l"] <- "_low"
  names(out)[names(out) == "ci_u"] <- "_upp"
  names(out)[names(out) == "var"] <- "_var"

  out
}

survey_stat_factor <- function(.svy, func, na.rm, vartype) {
  grps <- as.character(groups(.svy))
  peel <- grps[length(grps)]
  grps <- setdiff(grps, peel)

  vartype <- c("coef", vartype)

  if (length(grps) > 0) {
    stat <- survey::svyby(survey::make.formula(peel),
                          survey::make.formula(grps),
                          .svy, func, na.rm = na.rm, vartype = vartype)

    stat <- data.frame(stat)
    vlist <- list(paste0(peel, unique(.svy[["variables"]][[peel]])))
    vnames <- "coef"
    if ("se" %in% vartype) {
      vlist[[length(vlist) + 1]] <- paste0("se.", vlist[[1]])
      vnames <- c(vnames, "se")
    }
    if ("var" %in% vartype) {
      vlist[[length(vlist) + 1]] <- paste0("var.", vlist[[1]])
      vnames <- c(vnames, "var")
    }
    if ("ci" %in% vartype) {
      vlist[[length(vlist) + 1]] <- paste0("ci_l.", vlist[[1]])
      vlist[[length(vlist) + 1]] <- paste0("ci_u.", vlist[[1]])
      vnames <- c(vnames, "low", "upp")
    }

    out <- reshape(stat, varying = vlist,
            idvar = grps, times = unique(.svy[["variables"]][[peel]]), timevar = peel,
            v.names = vnames, direction = "long")

    out <- data.frame(out)
    names(out)[names(out) == "coef"] <- ""
    names(out)[names(out) == "se"] <- "_se"
    names(out)[names(out) == "var"] <- "_var"
    names(out)[names(out) == "low"] <- "_low"
    names(out)[names(out) == "upp"] <- "_upp"

    out
  } else {
    vartype <- c("lvls", vartype) # Needed because grouped don't usually have "coef"
    stat <- func(survey::make.formula(peel), .svy, na.rm = na.rm)

    out <- get_var_est(stat, vartype, peel = peel,
                       peel_class = class(.svy[["variables"]][[peel]]))


    dplyr::bind_cols(out)
  }
}

get_var_est <- function(stat, vartype, var_names = "", peel = "", peel_class = NULL) {
  out_width <- length(var_names)
  out <- lapply(vartype, function(vvv) {
    if (vvv == "coef") {
      coef <- data.frame(matrix(coef(stat), ncol = out_width))
      names(coef) <- var_names
      coef
    } else if (vvv == "se") {
      se <- data.frame(matrix(survey::SE(stat), ncol = out_width))
      names(se) <- paste0(var_names, "_se")
      se
    } else if (vvv == "ci") {
      ci <- data.frame(matrix(confint(stat), ncol = 2 * out_width))
      names(ci) <- c(paste0(var_names, "_low"), paste0(var_names, "_upp"))
      ci
    } else if (vvv == "var") {
      var <- data.frame(matrix(survey::SE(stat)^2, ncol = out_width))
      names(var) <- paste0(var_names, "_var")
      var
    } else  if (vvv == "lvls") {
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
