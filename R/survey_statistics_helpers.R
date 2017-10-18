set_survey_vars <- function(.svy, x, name = "SRVYR_TEMP_VAR", add = FALSE) {
  out <- .svy
  if (!add) {
    out$variables <- dplyr::data_frame(!!name := x)
  } else {
    out$variables[[name]] <- x
  }
  out
}


get_var_est <- function(
  stat, vartype, grps = "", level = 0.95, df = Inf, pre_calc_ci = FALSE, deff = FALSE
) {
  out_width <- 1
  out <- lapply(vartype, function(vvv) {
  if (vvv == "se") {
      se <- survey::SE(stat)
      # Needed for grouped quantile
      if (!inherits(se, "data.frame")) {
        se <- data.frame(matrix(se, ncol = out_width))
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

  coef <- data.frame(matrix(coef(stat), ncol = out_width))
  names(coef) <- "__SRVYR_COEF__"
  out <- c(list(coef), out)

  if (!identical(grps, "")) {
    out <- c(list(stat[grps]), out)
  }

  if (deff) {
    deff <- data.frame(matrix(survey::deff(stat), ncol = out_width))
    names(deff) <- "_deff"
    out <- c(out, list(deff))
  }

  dplyr::bind_cols(out)
}

# Largely the same as get_var_est(), but need to handle the fact that there can be
# multiple quantiles and that CI's are stored slightly differently.
get_var_est_quantile <- function(stat, vartype, q, grps = "", level = 0.95, df = Inf) {
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
        ci <- data.frame(stat[c("ci_l", "ci_u")])
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
  out <- c(list(coef), out)

  if (!identical(grps, "")) {
    out <- c(list(stat[grps]), out)
  }

  dplyr::bind_cols(out)
}

# Again a fair amount of overlap with the other get_var_ests, but handles the way
# factors are peeled off
get_var_est_factor <- function(
  stat, vartype, grps, peel, peel_levels, peel_is_factor, level = 0.95, df = Inf, deff = FALSE
) {
  var_names <- if (length(grps) > 0) peel_levels else ""
  out_width <- length(var_names)
  out <- lapply(vartype, function(vvv) {
    if (vvv == "se") {
      se <- survey::SE(stat)
      # Needed for grouped quantile
      if (!inherits(se, "data.frame")) {
        se <- data.frame(matrix(se, ncol = out_width))
      }
      names(se) <- paste0(var_names, "_se")
      se
    } else if (vvv == "ci") {
      if (length(level) == 1) {
        ci <- data.frame(matrix(
          stats::confint(stat, level = level, df = df),
          ncol = 2 * out_width
        ))
        names(ci) <- paste0(var_names, rep(c("_low", "_upp"), each = length(var_names)))
      } else {
        lci <- lapply(level, function(x) {as.data.frame(stats::confint(stat,level = x, df = df))})
        ci <- dplyr::bind_cols(lci)
        names(ci) <- paste0(c("_low", "_upp"), rep(level, each = 2) * 100)
      }
      ci
    } else if (vvv == "var") {
      var <- data.frame(matrix(survey::SE(stat) ^ 2, ncol = out_width))
      names(var) <- paste0(var_names, "_var")
      var
    } else if (vvv == "cv") {
      cv <- data.frame((matrix(survey::cv(stat), ncol = out_width)))
      names(cv) <- paste0(var_names, "_cv")
      cv
    } else {
      stop(paste0("Unexpected vartype ", vvv))
    }
  })

  coef <- data.frame(matrix(coef(stat), ncol = out_width))
  names(coef) <- var_names
  out <- c(list(coef), out)

  if (deff) {
    deff <- data.frame(matrix(survey::deff(stat), ncol = out_width))
    names(deff) <- paste0(var_names, "_deff")
    out <- c(out, list(deff))
  }

  if (length(grps) == 0  || grps == "") {
    names_out <- c("coef", vartype)
    if (deff) names_out <- c(names_out, "deff")
    names(out) <- names_out
  } else {
    out <- c(list(stat[grps]), out)
    names_out <- c("grps", "coef", vartype)
    if (deff) names_out <- c(names_out, "deff")
    names(out) <- names_out
  }
  if (!peel_is_factor) peel_levels <- NULL
  out <- factor_stat_reshape(out, peel, var_names, peel_levels)
  out
}



factor_stat_reshape <- function(stat, peel, var_names, peel_levels) {
  out <- lapply(seq_along(stat), function(iii) {
    stat_name <- names(stat)[iii]
    stat_df <- stat[[iii]]
    if (stat_name == "grps") {
      stat_df <- dplyr::tbl_df(stat_df)
      stat_df[rep(seq_len(nrow(stat_df)), length(var_names)), ]
    } else if(stat_name == "ci") {
      out <- utils::stack(stat_df)
      out <- data.frame(
        `_low` = out[substr_right(out$ind, 4) == "_low", "values"],
        `_upp` = out[substr_right(out$ind, 4) == "_upp", "values"],
        check.names = FALSE, stringsAsFactors = FALSE
      )
    } else if(stat_name == "coef") {
      out <- utils::stack(stat_df)
      names(out) <- c("__SRVYR_COEF__", peel)
      out[, c(2, 1)]
    } else {
      out <- utils::stack(stat_df)
      out <- select(out, -.data$ind)
      names(out) <- paste0("_", stat_name)
      out
    }
  })
  out <- dplyr::bind_cols(out)

  # peel's factor was created by stack, but is just alphabetic
  out[[peel]] <- as.character(out[[peel]])
  if (!is.null(peel_levels)) {
    out[[peel]] <- factor(out[[peel]], peel_levels)
  }

  out
}
