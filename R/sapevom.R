sapevom<-function(criteriaEvaluations, alternativesEvaluations){


  if(!inherits(criteriaEvaluations, "list"))
    stop("criteriaEvaluations must be a list")
  if(!all(sapply(criteriaEvaluations, is.matrix)))
    stop("criteriaEvaluations must be a list of matrices")
  if(!all(sapply(criteriaEvaluations, diag)==0))
    stop("criteriaEvaluations must be a list of matrices with diagonal all zero")
  if(!var(sapply(criteriaEvaluations, ncol))==0)
    stop("criteriaEvaluations must be a list of matrices with same number of columns")
  if(!all(sapply(criteriaEvaluations, function(x){ncol(x)==nrow(x)})))
    stop("criteriaEvaluations must be a list of square matrices with pairwise comparison of criteria")
  if(!all(sapply(criteriaEvaluations, function(x){x[upper.tri(x, diag=FALSE)]==-x[lower.tri(x, diag=FALSE)]})))
    stop("criteriaEvaluations must be a list of matrices with consistent pairwise comparison of criteria")




  if(!inherits(alternativesEvaluations, "list"))
    stop("alternativesEvaluations must be a list")

  if(!all(sapply(alternativesEvaluations, is.list)))
    stop("alternativesEvaluations must be a list of lists")

  i=1
  blist<-list()
  for (list in alternativesEvaluations){
    blist[[i]]<-sapply(list, is.matrix)
    i=i+1
  }
  if(!all(sapply(blist, all)))
    stop("alternativesEvaluations must be a list of lists of matrices")

  i=1
  clist<-list()
  for (list in alternativesEvaluations){
    clist[[i]]<-sapply(list, diag)==0
    i=i+1
  }
  if(!all(sapply(clist, all)))
    stop("alternativesEvaluations must be a list of lists of matrices with diagonal all zero")

  i=1
  dlist<-list()
  for (list in alternativesEvaluations){
    dlist[[i]]<-var(sapply(list, ncol))==0
    i=i+1
  }
  if(!all(sapply(dlist, all)))
    stop("alternativesEvaluations must be a list of lists of matrices with same number of columns")

  i=1
  elist<-list()
  for (list in alternativesEvaluations){
    elist[[i]]<-sapply(list, function(x){ncol(x)==nrow(x)})
    i=i+1
  }
  if(!all(sapply(elist, all)))
    stop("alternativesEvaluations must be a list of lists of square matrices with pairwise comparison of alternatives")


  i=1
  flist<-list()
  for (list in alternativesEvaluations){
    flist[[i]]<-sapply(list,function(x){x[upper.tri(x, diag=FALSE)]==-x[lower.tri(x, diag=FALSE)]})
    i=i+1
  }
  if(!all(sapply(flist, all)))
    stop("alternativesEvaluations must be a list of lists of square matrices with consistent pairwise comparison of alternatives")






  getcriteriaweights<-function(x){
    vs<-rowSums(x)
    vsn<-(vs-min(vs))/(max(vs)-min(vs))
    vf<-ifelse(vsn==0, min(vsn[vsn>0])*0.01, vsn)

  }
  criteriaweights<-rowSums(sapply(criteriaEvaluations, getcriteriaweights))






  norm<-function(x){
    v<-rowSums(x)
    (v-min(v))/(max(v)-min(v))
  }


  i=1
  mlist<-list()
  for (list in alternativesEvaluations){
    mlist[[i]]<-sapply(list, norm)
    i=i+1
    compiledmatrix<-sapply(mlist, rowSums)
  }




  sapevomresultsvector<-compiledmatrix%*%criteriaweights
  sapevomresults<-data.frame(values=sapevomresultsvector, rank=rank(-sapevomresultsvector))
  return(list(criteriaweights, sapevomresults))
}

