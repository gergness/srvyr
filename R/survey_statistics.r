#' @export
survey_mean <- function(.svy, ..., na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("survey_mean")
}

survey_mean.tbl_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  survey_stat_ungrouped(.svy, survey::svymean, arg, na.rm, vartype)
}

survey_mean.grouped_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c(match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  if (length(arg) == 1) survey_stat_grouped(.svy, survey::svymean, arg, na.rm, vartype)
  else if (length(arg) == 0) survey_stat_factor(.svy, survey::svymean, arg, na.rm, vartype)
  else stop("Unexpected arguments")
}


#' @export
survey_total <- function(.svy, ..., na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("survey_total")
}

survey_total.tbl_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  survey_stat_ungrouped(.svy, survey::svytotal, arg, na.rm, vartype)
}

survey_total.grouped_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c(match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  if (length(arg) == 1) survey_stat_grouped(.svy, survey::svytotal, arg, na.rm, vartype)
  else if (length(arg) == 0) survey_stat_factor(.svy, survey::svytotal, arg, na.rm, vartype)
  else stop("Unexpected arguments")
}


#' @export
survey_ratio <- function(.svy, ..., na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("survey_ratio")
}

survey_ratio.tbl_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  stat <- survey::svyratio(arg[[1]], arg[[2]], .svy, na.rm = na.rm)

  out <- lapply(vartype, function(vvv) {
    if (vvv == "coef") {
      coef <- data.frame(stat$ratio)
      names(coef) <- ""
      coef
    } else if (vvv == "se") {
      se <- data.frame(sqrt(stat$var))
      names(se) <- "_se"
      se
    } else if (vvv == "ci") {
      ci <- data.frame(confint(stat))
      names(ci) <- c("_low", "_upp")
      ci
    } else if (vvv == "var") {
      var <- data.frame(stat$var)
      names(var) <- "_var"
      var
    }
  })

  dplyr::bind_cols(out)
}

survey_ratio.grouped_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c(match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  grps <- survey::make.formula(groups(.svy))

  # svyby breaks when you feed it raw vector to be measured... Add it to
  # the data.frame with mutate and then pass in the name
  .svy$variables[["___numerator"]] <- arg[[1]]
  .svy$variables[["___denominator"]] <- arg[[2]]

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
survey_quantile <- function(.svy, ..., quantiles, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  UseMethod("survey_quantile")
}

survey_quantile.tbl_svy <- function(.svy, ..., quantiles, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  if(missing(vartype)) vartype <- "none"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  vartype <- setdiff(vartype, "none")
  se <- "se" %in% vartype
  ci <- "ci" %in% vartype

  stat <- survey::svyquantile(data.frame(arg[[1]]), .svy,
                      quantiles = quantiles, na.rm = na.rm,
                      se = se, ci = ci)

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2, flag = "0")))

  out <- lapply(vartype, function(vvv) {
    if (vvv == "coef") {
      # Accessors to svquantile change if there's CI or not
      if (class(stat) == "matrix") coef <- data.frame(stat)
      else coef <- data.frame(stat[["quantiles"]])
      names(coef) <- q_text
      coef
    } else if (vvv == "se") {
      se <- data.frame(t(attr(stat, "SE")))
      names(se) <- paste0(q_text, "_se")
      se
    } else if (vvv == "ci") {
      ci <- data.frame(t(stat[["CIs"]][seq_len(2 * length(q_text))]))
      names(ci) <- paste0(rep(q_text, each = 2),
                          rep(c("_low", "_upp"), length = 2 * length(q_text)))
      ci
    }
  })
  dplyr::bind_cols(out)
}

survey_quantile.grouped_svy <- function(.svy, ..., quantiles, na.rm = FALSE, vartype = c("none", "se", "ci")) {
  if(missing(vartype)) vartype <- "none"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))
  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)

  vartype <- setdiff(vartype, "none")
  grps <- survey::make.formula(groups(.svy))

  .svy$variables[["___arg"]] <- arg[[1]]

  out <- survey::svyby(~`___arg`, grps, .svy, survey::svyquantile,
                      quantiles = quantiles, na.rm = na.rm,
                      ci = TRUE, vartype = vartype)

  q_text <- paste0("_q", gsub("\\.", "", formatC(quantiles * 100, width = 2, flag = "0")))
  # Format it nicely
  out <- dplyr::tbl_df(as.data.frame(out))
  names(out)[1 + seq_along(q_text)] <- q_text
  if ("se" %in% vartype) names(out)[grep("^se", names(out))] <- paste0(q_text, "_se")
  if ("ci" %in% vartype) {
    names(out)[grep("^ci_l", names(out))] <- paste0(q_text, "_low")
    names(out)[grep("^ci_u", names(out))] <- paste0(q_text, "_upp")
  }

  out
}


#' @export
survey_median <- function(.svy, ..., na.rm = FALSE, vartype = c("none", "se", "ci")) {
  UseMethod("survey_median")
}

survey_median.default <- function(.svy, ..., na.rm = FALSE, vartype = c("none", "se", "ci")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))

  survey_quantile(.svy, ..., quantiles = 0.5, na.rm = na.rm, vartype = vartype)
}


#' @export
unweighted <- function(.svy, ..., na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("unweighted")
}

unweighted.default <-  function(.svy, ...) {
  dots <-lazyeval::lazy_dots(...)

  out <- summarize_(.svy[["variables"]], .dots = dots)
  names(out)[length(names(out))] <- ""
  out
}


survey_stat_ungrouped <- function(.svy, func, arg, na.rm, vartype) {
  if (class(arg[[1]]) == "factor") stop("Factor not allowed in survey functions, should be used as a grouping variable")
  stat <- func(data.frame(arg[[1]]), .svy, na.rm = na.rm)

  out <- lapply(vartype, function(vvv) {
      if (vvv == "coef") {
        coef <- data.frame(coef(stat))
        names(coef) <- ""
        coef
      } else if (vvv == "se") {
        se <- data.frame(sqrt(attr(stat, "var")))
        names(se) <- "_se"
        se
      } else if (vvv == "ci") {
        ci <- data.frame(confint(stat))
        names(ci) <- c("_low", "_upp")
        ci
      } else if (vvv == "var") {
        var <- data.frame(attr(stat, "var"))
        names(var) <- "_var"
        var
      }
    })

  dplyr::bind_cols(out)
}

survey_stat_grouped <- function(.svy, func, arg, na.rm, vartype ) {
  grps <- survey::make.formula(groups(.svy))

  if (class(arg[[1]]) == "factor") stop("Factor not allowed in survey functions, should be used as a grouping variable")

  # svyby breaks when you feed it raw vector to be measured... Add it to
  # the data.frame with mutate and then pass in the name
  .svy$variables[["___arg"]] <- arg[[1]]
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

survey_stat_factor <- function(.svy, func, arg, na.rm, vartype) {
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

    out <- lapply(vartype, function(vvv) {
      if (vvv == "lvls") {
        # Add on level variable -- survey leaves it in an ugly state, with the
        # varname pasted in, so we have to remove it. Also, check if it was
        # originally a character and convert if it was.
        lvls <- data.frame(names(coef(stat)))
        levels(lvls[[1]]) <- gsub(paste0("^", peel), "", levels(lvls[[1]]))
        if (class(.svy[["variables"]][[peel]]) == "character") lvls[[1]] <- as.character(lvls[[1]])
        names(lvls) <- peel
        lvls
      } else if (vvv == "coef") {
        coef <- data.frame(coef(stat))
        names(coef) <- ""
        coef
      } else if (vvv == "se") {
        se <- data.frame(sqrt(diag(attr(stat, "var"))))
        names(se) <- "_se"
        se
      } else if (vvv == "ci") {
        ci <- data.frame(confint(stat))
        names(ci) <- c("_low", "_upp")
        ci
      } else if (vvv == "var") {
        var <- data.frame(diag(attr(stat, "var")))
        names(var) <- "_var"
        var
      }
    })

    dplyr::bind_cols(out)
  }
}
