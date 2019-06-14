
### jags models for binary outcomes - positive outcome

nmajagsranks_bin_B <- function(netdata) {
  
  #transform the data into a list suitable for JAGS analysis
  NMAdataBinary=make.jagsNMA.data(studyid=id, t=t,
                                  r=r, n=n,
                                  data=netdata, type="binary", reference = min(netdata$t))
  
  
  
  #run Jags and create a jags object
  NMAinJAGS<- jags.parallel(data = NMAdataBinary, inits = NULL,
                            parameters.to.save = c("ORref","tau", "SUCRA"),
                            n.chains = 2, n.iter = 10000,
                            n.burnin = 1000,DIC=F,
                            model.file = modelNMABinary.SUCRA_B)
  SUCRAjags_ranks <- (NMAdataBinary$nt+1) - rank(NMAinJAGS$BUGSoutput$mean$SUCRA)
  SUCRAjags <- NMAinJAGS$BUGSoutput$mean$SUCRA
  return(cbind("SUCRA jags"= SUCRAjags, "SUCRAjags ranks" = SUCRAjags_ranks))
}