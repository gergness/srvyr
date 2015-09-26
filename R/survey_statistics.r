#' @export
survey_mean <- function(.svy, ..., na.rm, vartype = c("se", "ci", "var")) {
  UseMethod("survey_mean")
}

survey_mean.tbl_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c("coef", match.arg(vartype, several.ok = TRUE))

  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)
  mn <- survey::svymean(data.frame(arg[[1]]), .svy, na.rm = na.rm)


  out <- lapply(vartype, function(vvv) {
    if (vvv == "coef") {
      coef <- data.frame(coef(mn))
      names(coef) <- ""
      coef
    } else if (vvv == "se") {
      se <- data.frame(sqrt(attr(mn, "var")))
      names(se) <- "_se"
      se
    } else if (vvv == "ci") {
      ci <- data.frame(confint(mn))
      names(ci) <- c("_low", "_upp")
      ci
    } else if (vvv == "var") {
      var <- data.frame(attr(mn, "var"))
      names(var) <- "_var"
      var
    }
  })

  dplyr::bind_cols(out)
}

survey_mean.grouped_svy <- function(.svy, ..., na.rm = FALSE, vartype = c("se", "ci", "var")) {
  if(missing(vartype)) vartype <- "se"
  vartype <- c(match.arg(vartype, several.ok = TRUE))

  arg <- lazyeval::lazy_eval(lazyeval::lazy_dots(...), .svy$variables)
  grps <- survey::make.formula(groups(.svy))

  # svyby breaks when you feed it raw vector to be measured... Add it to
  # the data.frame with mutate and then pass in the name
  .svy$variables[["___arg"]] <- arg[[1]]
  out <- survey::svyby(~`___arg`, grps, .svy, survey::svymean, na.rm = na.rm, vartype = vartype)

  # Format it nicely
  out <- dplyr::tbl_df(as.data.frame(out))
  names(out)[names(out) == "`___arg`"] <- ""
  names(out)[names(out) == "se"] <- "_se"
  names(out)[names(out) == "ci_l"] <- "_low"
  names(out)[names(out) == "ci_u"] <- "_upp"
  names(out)[names(out) == "var"] <- "_var"

  out
}
