# Taken from survey package, want to fix a bug
svyby_fix<-function(formula, by, design, FUN,..., deff=FALSE, keep.var=TRUE,
                        keep.names=TRUE, verbose=FALSE, vartype=c("se","ci","ci","cv","cvpct","var"),
                        drop.empty.groups=TRUE, covmat=FALSE, return.replicates=FALSE, na.rm.by=FALSE,
                        na.rm.all=FALSE,
                        multicore=getOption("survey.multicore")){

  if (inherits(by, "formula"))
    byfactors<-model.frame(by, model.frame(design), na.action=na.pass)
  else
    byfactors<-as.data.frame(by)

  if(covmat || return.replicates){
    if (!inherits(design,"svyrep.design"))
      stop("covmat=TRUE not implemented for this design type")
  }

  if (multicore && !require("parallel",quietly=TRUE))
    multicore<-FALSE

  ## some people insist on using vectors rather than formulas
  ## so I suppose we should be nice to them
  if (!inherits(formula, "formula")){
    if (NROW(formula)!=length(byfactor))
      stop("'formula' is the wrong length")
    if (!(is.data.frame(formula) ||
          is.matrix(formula) ||
          is.vector(formula))){
      stop("invalid type for 'formula'")
    }
  }

  hasdeff<- is.character(deff) || deff

  ## all combinations that actually occur in this design
  byfactor<-do.call("interaction", byfactors)
  dropped<- weights(design,"sampling")==0
  if (na.rm.by) dropped<-dropped | apply(byfactors, 1, function(x) any(is.na(x)))
  if (na.rm.all){
    if (inherits(formula,"formula"))
      allx<-model.frame(formula,model.frame(design),na.action=na.pass)
    else
      allx<-formula
    dropped <- dropped | (!complete.cases(allx))
  }
  uniquelevels<-sort(unique(byfactor[!dropped]))
  uniques <- match(uniquelevels, byfactor)


  if(missing(vartype)) vartype<-"se"
  vartype<-match.arg(vartype,several.ok=TRUE)
  nvartype<-which(eval(formals(sys.function())$vartype) %in% vartype)
  if(any(is.na(nvartype))) stop("invalid vartype")

  if (keep.var){
    unwrap <-function(x){
      rval<-c(coef(x))
      nvar<-length(rval)
      rval<-c(rval,c(se=SE(x),
                     ci_l=confint(x)[,1],
                     ci_u=confint(x)[,2],
                     cv=cv(x,warn=FALSE),
                     `cv%`=cv(x,warn=FALSE)*100,
                     var=SE(x)^2)[rep((nvartype-1)*(nvar),each=nvar)+(1:nvar)])
      if(!is.null(attr(x,"deff")))
        rval<-c(rval,DEff=deff(x))
      rval
    }

    ## In dire need of refactoring (or rewriting)
    ## but it seems to work.
    results<-(if (multicore) mclapply else lapply)(uniques,
                                                   function(i){
                                                     if(verbose && !multicore) print(as.character(byfactor[i]))
                                                     if (inherits(formula,"formula"))
                                                       data<-formula
                                                     else
                                                       data<-subset(formula, byfactor %in% byfactor[i])
                                                     if (covmat || return.replicates) {
                                                       FUN(data,
                                                           design[byfactor %in% byfactor[i],],
                                                           deff=deff,...,return.replicates=TRUE)
                                                     } else {
                                                       FUN(data,
                                                           design[byfactor %in% byfactor[i],],
                                                           deff=deff,...)
                                                     }
                                                   })
    rval<-t(sapply(results, unwrap))
    if (covmat || return.replicates) {
      replicates<-do.call(cbind,lapply(results,"[[","replicates"))
      colnames(replicates)<-rep(as.character(uniquelevels), each=NCOL(replicates)/length(uniquelevels))
      covmat.mat<-svrVar(replicates,design$scale,design$rscales, mse=design$mse,coef=as.vector(sapply(results,coef)))
    } else{
      covmats<-lapply(results,vcov)
      ncovmat<-sum(sapply(covmats,ncol))
      covmat.mat<-matrix(0,ncol=ncovmat,nrow=ncovmat)
      j<-0
      for(i in 1:length(covmats)){
        ni<-nrow(covmats[[i]])
        covmat.mat[j+(1:ni),j+(1:ni)]<-covmats[[i]]
        j<-j+ni
      }
    }
  } else {
    unwrap2 <- function(x){
      if(!is.null(attr(x, "deff")))
        c(statistic = unclass(x),
          DEff = deff(x))
      else c(statistic = unclass(x))
    }
    rval<-sapply(uniques,
                 function(i) {
                   if(verbose) print(as.character(byfactor[i]))
                   if (inherits(formula,"formula"))
                     data<-formula
                   else
                     data<-subset(formula, byfactor %in% byfactor[i])
                   unwrap2(FUN(data,
                               design[byfactor %in% byfactor[i],],
                               deff=deff,...))}
    )
    if (is.matrix(rval)) rval<-t(rval)
  }

  nr<-NCOL(rval)
  nstats<-nr/(1+ keep.var*(length(vartype)+ ("ci" %in% vartype)) + hasdeff)


  if (nr>1)
    rval<-cbind(byfactors[uniques,,drop=FALSE], rval)
  else
    rval <-cbind(byfactors[uniques,,drop=FALSE], statistic=rval)

  expand.index<-function(index,reps,x=FALSE){
    ns<-max(index)
    if (x){
      i<-matrix(1:(ns*reps),ncol=reps)
      rval<-t(i[index,])

    } else{
      i<-matrix(1:(ns*reps), ncol=reps, nrow=ns, byrow=TRUE)
      rval<- i[index,]
    }
    as.vector(rval)
  }

  if(drop.empty.groups){
    if (keep.names)
      rownames(rval)<-paste(byfactor[uniques])
    rval<-rval[order(byfactor[uniques]),]

    i<-expand.index(order(byfactor[uniques]),nstats)
    if (keep.var)
      covmat.mat<-covmat.mat[i,i]

  } else {
    a<-do.call("expand.grid", lapply(byfactors,function(f) levels(as.factor(f))))
    a<-cbind(a,matrix(NA, ncol=nr, nrow=nrow(a)))
    names(a)<-names(rval)
    a[match(byfactor[uniques], levels(byfactor)),]<-rval
    rval<-a
    if (keep.names)
      rownames(rval)<-levels(byfactor)
    if (keep.var){
      tmp<-matrix(ncol=nrow(a)*nstats,nrow=nrow(a)*nstats)
      i<-expand.index(match(byfactor[uniques], levels(byfactor)),nstats,TRUE)
      tmp[i,i]<-covmat.mat
      covmat.mat<-tmp
    }
  }

  attr(rval,"svyby")<-list(margins=1:NCOL(byfactors),nstats=nstats,
                           vars=if(keep.var) length(vartype) else 0,
                           deffs=deff,
                           statistic=deparse(substitute(FUN)),
                           variables= names(rval)[-(1:NCOL(byfactors))][1:nstats],
                           vartype=vartype
  )
  if (!keep.names)
    rownames(rval)<-1:NROW(rval)

  if(covmat)
    attr(rval,"var")<-covmat.mat
  if (return.replicates)
    attr(rval,"replicates")<-replicates
  attr(rval,"call")<-sys.call()
  class(rval)<-c("svyby","data.frame")
  rval
}

SE.svyby <-function(object,...){
  aa<-attr(object,"svyby")
  if (!aa$vars) stop("Object does not contain variances")
  vartype<-attr(object,"svyby")$vartype
  if (pos<-match("se",vartype,0))
    object[,max(aa$margins)+aa$nstats*pos+(1:aa$nstats)]
  else if (pos<-match("var",vartype,0))
    sqrt(object[,max(aa$margins)+aa$nstats*pos+(1:aa$nstats)])
  else if (pos<-match("cv",vartype,0))
    object[,max(aa$margins)+aa$nstats*pos+(1:aa$nstats)]*coef(object)
  else if (pos<-match("cvpct",vartype,0))
    object[,max(aa$margins)+aa$nstats*pos+(1:aa$nstats)]*coef(object)/100
  else stop("This can't happen")

}

coef.svyby<-function (object, ...)
{
  aa <- attr(object, "svyby")
  rval <- object[, max(aa$margins) + (1:aa$nstats)]
  if (is.null(dim(rval))){
    names(rval) <- row.names(object)
  } else {
    rval<-as.vector(as.matrix(rval))
    names(rval)<-outer(rownames(object),
                       gsub("statistics\\.","",aa$variables), paste, sep=":")
  }
  rval
}

deff.svyby<-function(object,...){
  aa<-attr(object,"svyby")
  if (!aa$deffs) stop("object does not have design effect information")
  object[,max(aa$margins)+aa$nstats*(1+aa$vars)+(1:aa$nstats)]
}

vcov.svyby<-function(object,...){
  rval<-attr(object,"var")
  if(is.null(rval)){
    warning("Only diagonal elements of vcov() available")
    se<-SE(object)
    if (is.data.frame(se)) se<-as.vector(as.matrix(se))
    if(length(se)>1)
      rval<-diag(se^2)
    else
      rval<-as.matrix(se^2)
  }
  nms<-names(coef(object))
  dimnames(rval)<-list(nms,nms)
  rval
}

confint.svyquantile<-function(object,parm=NULL,level=NULL,...){
  if (!is.null(level)) stop("need to re-run svyquantile to specify level")
  ci<-t(matrix(as.vector(object$CIs),nrow=2))
  colnames(ci)<-dimnames(object$CIs)[[1]]
  rownames(ci)<-outer(dimnames(object$CIs)[[2]],
                      dimnames(object$CIs)[[3]],paste,sep="_")
  if (is.null(parm))
    ci
  else
    ci[parm,,drop=FALSE]
}
