#####################################################################
#   A function to alternatively parametrise network meta-analysis   #
#####################################################################
# Arguments:
#netmetaobject: An object of class netmeta
#random: A logical indicating whether a random effects meta-analysis should be conducted.
#small.values: A character string specifying whether small treatment effects indicate a "good" or "bad" effect
#####################################################################

alternativenma <- function(netmetaobject,random=T,small.values="good"){

  require(magic)
  require(netmeta)

  #run network meta-analysis as a two stage meta-regression model
  if (random==T)  a <- nma.krahn.output(netmetaobject,tau.preset = netmetaobject$tau)
  if (random==F)  a <- nma.krahn.output(netmetaobject,tau.preset = 0)

  #define design matrices for the alternative parametrisation
  #X.alt is the Y* matrix and X.obs.alt is the X^C* in the paper
  X.alt=rbind(a$X.full[1:(a$n-1),]-1, a$X.full[a$n:nrow(a$X.full),])
  X.obs2.design.alt <- X.alt[a$direct2$comparison, , drop = FALSE]

  #define design matrix in the case of multi arm studies
  if (a$multiarm) {
    X.obs3.design.alt <- X.alt[as.character(a$basics), ]
    X.obs.alt <- rbind(X.obs2.design.alt, X.obs3.design.alt)
  }
  if (!a$multiarm)
  {X.obs.alt <- X.obs2.design.alt}

  #estimate the effects versus average and the network meta-analysis effects
  avs <-  solve(t(X.obs.alt) %*% solve(a$V) %*% X.obs.alt) %*% t(X.obs.alt) %*% solve(a$V) %*% a$TE.dir
  TE.net.avs <- X.alt %*% avs

  #variance covariance matrix of avs
  covTE.net.base.alt <- solve(t(X.obs.alt) %*% solve(a$V) %*% X.obs.alt)

  #data frame with avs and their standard errors
  averages0 <- data.frame(TE = avs, seTE = sqrt(diag(covTE.net.base.alt)))

  #estimation of effect versus average and its standard error for the reference treatment
  cnb=as.matrix(covTE.net.base.alt)
  refavvar=sum(diag(cnb))+sum(2*cnb[lower.tri(cnb)])

  labels1=rep(0,a$n-1)
  for(i in 1:(a$n-1)){
    labels1[i]=strsplit(rownames(averages0),":")[[i]][2]
  }
  labels2=strsplit(rownames(averages0),":")[[1]][1]
  labels=c(labels1,labels2)

  z1=qnorm(1-0.05/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  TE = round(c(avs,-sum(avs)), digits = 3)
  seTE = round(c(sqrt(diag(covTE.net.base.alt)),sqrt(refavvar)), digits = 3)
  lower= TE - z1*seTE
  upper= TE + z1*seTE

  #calculation of probabilities of being better than the average
  Pscoreaverage=rep(0,a$n)
  if (small.values=="good") {Pscoreaverage=pnorm(TE/seTE)        #change from original altnma script: swapped with one from "if" below to match Pscore equation in paper
                             TE_ranks=(a$n+1) - rank(TE)}
  if (small.values=="bad") {Pscoreaverage=1-pnorm(TE/seTE)
                            TE_ranks=rank(TE)}

  #summary of results
  averages=data.frame(TE = TE, seTE = seTE, lower=lower, upper=upper, TE_ranks, Pscoreaverage=Pscoreaverage)
  rownames(averages)=labels

  res <- list(n = a$n,
              trts = a$trts,
              comparisons = a$comparisons,
              studies = a$studies,
              X.alt=X.alt,
              X.obs.alt=X.obs.alt,
              averages=averages,
              TE.net.avs=TE.net.avs)

  class(res) <- "alternativenma"

  res
}
