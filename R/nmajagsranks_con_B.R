
### jags models for continuous outcomes - positive outcome


nmajagsranks_con_B <- function(netdata) {

  #transform the data into a list suitable for JAGS analysis
  NMAdataContinuous=make.jagsNMA.data(studyid=id, t=t,
                                      y=y, sd=sd, n=n,
                                      data=netdata, type="cont", reference = min(netdata$t))



  #run Jags and create a jags object
  NMAinJAGS<- jags.parallel(data = NMAdataContinuous, inits = NULL,
                            parameters.to.save = c("SMD","tau", "SUCRA"),
                            n.chains = 2, n.iter = 10000,
                            n.burnin = 1000,DIC=F, n.thin = 2,
                            model.file = modelNMAContinuous_B)

  #check chain mixing
  traceplot(NMAinJAGS,varname="tau", ask=F )

  SUCRAjags_ranks <- (NMAdataContinuous$nt+1) - rank(NMAinJAGS$BUGSoutput$mean$SUCRA)
  SUCRAjags <- NMAinJAGS$BUGSoutput$mean$SUCRA
  return(cbind("SUCRA jags"= SUCRAjags, "SUCRAjags ranks" = SUCRAjags_ranks))
}
