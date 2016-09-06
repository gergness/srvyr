# Adapted from survey:::"[.survey.design2"
subset_svy_vars_design <- function(x, i, ..., drop = TRUE) {
  if (!missing(i)){
    if (is.calibrated(x) || is.pps(x) || !drop){
      ## Set weights to zero: no memory saving possible
      ## There should be an easier way to complement a subscript..
      if (is.logical(i))
        x$prob[!i]<-Inf
      else if (is.numeric(i) && length(i))
        x$prob[-i]<-Inf
      else {
        tmp<-x$prob[i,]
        x$prob<-rep(Inf, length(x$prob))
        x$prob[i,]<-tmp
      }
      index<-is.finite(x$prob)
      psu<-!duplicated(x$cluster[index,1])
      tt<-table(x$strata[index,1][psu])
      if(any(tt==1) && getOption("survey.adjust.domain.lonely")){
        warning(sum(tt==1)," strata have only one PSU in this subset.")
      }
    } else {
      ## subset everything.
      if (!is.null(x$variables)) ## phase 2 of twophase design
        x$variables<-"[.data.frame"(x$variables,i,..1,drop=FALSE)
      x$cluster<-x$cluster[i,,drop=FALSE]
      x$prob<-x$prob[i]
      x$allprob<-x$allprob[i,,drop=FALSE]
      x$strata<-x$strata[i,,drop=FALSE]
      x$fpc$sampsize<-x$fpc$sampsize[i,,drop=FALSE]
      x$fpc$popsize<-x$fpc$popsize[i,,drop=FALSE]
    }

  } else {
    if(!is.null(x$variables))
      x$variables<-x$variables[,..1,drop=FALSE]
  }

  x
}

# Adapted from survey:::"[.survey.rep"
subset_svy_vars_rep <- function(x, i, j, drop = FALSE){
  if (!missing(i)){
    pwt<-x$pweights
    if (is.data.frame(pwt)) pwt<-pwt[[1]]
    x$pweights<-pwt[i]
    x$repweights<-x$repweights[i,,drop=FALSE]
    if(!is.null(x$selfrep))
      x$selfrep<-x$selfrep[i]
    if (!missing(j))
      x$variables<-x$variables[i,j, drop=FALSE]
    else
      x$variables<-x$variables[i,,drop=FALSE]
    x$degf<-NULL
    x$degf<-degf(x)
  } else {
    x$variables<-x$variables[,j,drop=FALSE]
  }
  x
}

# Adapted from survey:::"[.twophase2"
subset_svy_vars_twophase <- function(x, i, ..., drop = TRUE) {
  if (!missing(i)){
    ## Set weights to zero:  don't try to save memory
    ## There should be an easier way to complement a subscript..
    if (is.logical(i) && any(!i)){
      ## logical indexing: use !
      x$prob[!i]<-Inf
      x$phase2$prob[!i]<-Inf
      x$dcheck<-lapply(x$dcheck, function(m) {m[!i,!i]<-0; m})
    } else if (is.numeric(i) && length(i)){
      ## numeric indexing: use -
      x$prob[-i]<-Inf
      x$phase2$prob[-i]<-Inf
      x$dcheck<-lapply(x$dcheck, function(m) {m[-i,-i]<-0;m})
    } else if (is.character(i)){
      ##character indexing: use brute force and ignorance
      tmp<-x$prob[i,]
      x$prob<-rep(Inf, length(x$prob))
      x$prob[i,]<-tmp
      tmp<-x$phase2$prob[i,]
      x$phase2$prob<-rep(Inf, length(x$phase2$prob))
      x$phase2$prob[i,]<-tmp
      x$dcheck<-lapply(x$dcheck, function(m) {n<-Matrix(ncol(m),ncol(m)); n[i,i]<-m[i,i]})
    }
    index<-is.finite(x$prob)
    psu<-!duplicated(x$phase2$cluster[index,1])
    tt<-table(x$phase2$strata[index,1][psu])
    if(any(tt==1)){
      warning(sum(tt==1)," strata have only one PSU in this subset.")
    }
  } else {
    x$phase1$full<-x$phase1$full[,...]
    x$phase1$sample<-x$phase1$sample[,...]
    x$phase2<-x$phase2[,...]
  }
  x
}
