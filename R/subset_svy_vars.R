subset_svy_vars <- function(x, ..., .preserve = FALSE) {
  UseMethod("subset_svy_vars")
}

# Adapted from survey:::"[.survey.design2"
subset_svy_vars.survey.design2 <- function(x, ..., .preserve = FALSE) {
  dots <- rlang::quos(...)
  filtered <- filtered_row_numbers(x, !!!dots, .preserve = .preserve)
  filtered_vars <- filtered[["filtered_vars"]]
  row_numbers <- filtered[["row_numbers"]]

  if (is.calibrated(x) || is.pps(x)){
    ## Set weights to zero: no memory saving possible
    ## Will always be numeric because srvyr's construction
    x$prob[-row_numbers] <- Inf

    index <- is.finite(x$prob)
    psu <- !duplicated(x$cluster[index, 1])
    tt <- table(x$strata[index, 1][psu])
    if(any(tt == 1) && getOption("survey.adjust.domain.lonely")){
      warning(sum(tt == 1)," strata have only one PSU in this subset.")
    }
  } else {
    ## subset everything.
    if (!is.null(x$variables)) ## phase 2 of twophase design
      x$variables <- filtered_vars
    x$cluster <- x$cluster[row_numbers, , drop=FALSE]
    x$prob <- x$prob[row_numbers]
    x$allprob <- x$allprob[row_numbers, , drop=FALSE]
    x$strata <- x$strata[row_numbers, , drop=FALSE]
    x$fpc$sampsize <- x$fpc$sampsize[row_numbers, , drop=FALSE]
    x$fpc$popsize <- x$fpc$popsize[row_numbers, , drop=FALSE]
  }

  x
}

# Adapted from survey:::"[.svyrep.design"
subset_svy_vars.svyrep.design <- function(x, ..., .preserve = FALSE){
  dots <- rlang::quos(...)
  filtered <- filtered_row_numbers(x, !!!dots, .preserve = .preserve)
  filtered_vars <- filtered[["filtered_vars"]]
  row_numbers <- filtered[["row_numbers"]]


  pwt <- x$pweights

  if (is.data.frame(pwt)) pwt <- pwt[[1]]
  x$pweights <- pwt[row_numbers]
  x$repweights <- x$repweights[row_numbers, , drop=FALSE]
  if (!is.null(x$selfrep))
    x$selfrep<-x$selfrep[row_numbers]

  x$variables <- filtered_vars

  x$degf<-NULL
  x$degf<-survey::degf(x)

  x
}

# Adapted from survey:::"[.twophase2"
subset_svy_vars.twophase2 <- function(x, ..., .preserve = FALSE) {
  dots <- rlang::quos(...)
  filtered <- filtered_row_numbers(x, !!!dots, .preserve = .preserve)
  filtered_vars <- filtered[["filtered_vars"]]
  row_numbers <- filtered[["row_numbers"]]

  ## Set weights to zero:  don't try to save memory
  ## Will always have numeric because of srvyr's structure
  x$prob[-row_numbers] <- Inf
  x$phase2$prob[-row_numbers] <- Inf
  x$dcheck <- lapply(x$dcheck, function(m) {m[-row_numbers, -row_numbers] <- 0; m})

  index <- is.finite(x$prob)
  psu <- !duplicated(x$phase2$cluster[index, 1])
  tt <- table(x$phase2$strata[index, 1][psu])
  if(any(tt == 1)){
    warning(sum(tt == 1), " strata have only one PSU in this subset.")
  }

  x
}


filtered_row_numbers <- function(.svy, ..., .preserve = FALSE) {
  dots <- rlang::quos(...)
  filtered_vars <- .svy$variables

  filtered_vars[["___row_number"]] <- seq_len(nrow(filtered_vars))
  filtered_vars <- dplyr::filter(filtered_vars, !!!dots, .preserve = .preserve)
  row_numbers <- dplyr::select(filtered_vars, .data[["___row_number"]])[["___row_number"]]
  filtered_vars <- dplyr::select(filtered_vars, -.data[["___row_number"]])

  list(row_numbers = row_numbers, filtered_vars = filtered_vars)

}
