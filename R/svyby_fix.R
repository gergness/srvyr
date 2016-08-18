##
##  svyby (from survey package's surveyby.r file)
##


svyby_fixed<-function(formula, by, design, FUN,..., deff=FALSE, keep.var=TRUE,
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

  if (multicore && !requireNamespace("parallel",quietly=TRUE))
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
  nvartype<-base::which(eval(formals(sys.function())$vartype) %in% vartype)
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
    results<-(if (multicore) parallel::mclapply else lapply)(uniques,
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

