subset_svy_vars <- function(x, dots) {
  UseMethod("subset_svy_vars")
}

# Adapted from survey:::"[.survey.design2"
subset_svy_vars.survey.design2 <- function(x, dots) {
  filtered <- filtered_row_numbers(x, dots)
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
subset_svy_vars.svyrep.design <- function(x, dots){
  filtered <- filtered_row_numbers(x, dots)
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
subset_svy_vars.twophase2 <- function(x, dots) {
  filtered <- filtered_row_numbers(x, dots)
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


filtered_row_numbers <- function(.svy, dots) {
  filtered_vars <- .svy$variables

  if (!inherits(.svy$variables, "tbl_lazy")) {
    filtered_vars <- dplyr::mutate_(filtered_vars, `___row_number` = "row_number()")
    filtered_vars <- dplyr::filter_(filtered_vars, .dots = dots)
    row_numbers <- dplyr::select_(filtered_vars, "`___row_number`")[[1]]
    filtered_vars <- dplyr::select_(filtered_vars, "-`___row_number`")
  } else {
    order_var_names <- attr(.svy$variables, "order_var")
    filtered_vars <- dplyr::filter_(filtered_vars, .dots = dots)
    order_vars <- dplyr::select_(filtered_vars, order_var_names)
    order_vars <- dplyr::collect(order_vars, n = Inf)

    uid <- uid(.svy)
    uid <- mutate_(uid, `___row_number` = "row_number()")

    row_numbers <- dplyr::inner_join(uid, order_vars, by = order_var_names)
    row_numbers <- select_(row_numbers, "`___row_number`")[[1]]
  }

  list(row_numbers = row_numbers, filtered_vars = filtered_vars)

}
