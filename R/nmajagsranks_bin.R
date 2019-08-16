# library(devtools)
# install_github("esm-ispm-unibe-ch/nmadata")
# install_github("esm-ispm-unibe-ch/NMAJags")
# library(NMAJags)
# library(R2jags)




### jags models for binary outcomes - negative outcome

nmajagsranks_bin <- function(netdata) {

                        #transform the data into a list suitable for JAGS analysis
                        NMAdataBinary=make.jagsNMA.data(studyid=id, t=t,
                                                            r=r, n=n,
                                                            data=netdata, type="binary", reference = min(netdata$t))



                        #run Jags and create a jags object
                        NMAinJAGS<- jags.parallel(data = NMAdataBinary, inits = NULL,
                                         parameters.to.save = c("ORref","tau", "SUCRA"),
                                         n.chains = 2, n.iter = 10000,
                                         n.burnin = 1000,DIC=F,
                                         model.file = modelNMABinary.SUCRA)

                        #check chain mixing
                        traceplot(NMAinJAGS,varname="tau", ask=F )


                        SUCRAjags_ranks <- (NMAdataBinary$nt+1) - rank(NMAinJAGS$BUGSoutput$mean$SUCRA)
                        SUCRAjags <- NMAinJAGS$BUGSoutput$mean$SUCRA
                        return(cbind("SUCRA jags"= SUCRAjags, "SUCRAjags ranks" = SUCRAjags_ranks))
}
                        # start_time <- Sys.time()
                        # end_time <- Sys.time()
                        # end_time - start_time

