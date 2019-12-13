SUCRAvsSUCRAjags_s[Filter(Negate(anyNA), match(nmadb[nmadb$Effect.Measure=="odds ratio" |
                                                     nmadb$Effect.Measure=="standardized mean difference","Record.ID"],
                                              names(SUCRAvsSUCRAjags_s)))][SUCRAvsSUCRAjags_s[Filter(Negate(anyNA), match(nmadb[nmadb$Effect.Measure=="odds ratio" |
                                                    nmadb$Effect.Measure=="standardized mean difference","Record.ID"], names(SUCRAvsSUCRAjags_s)))]<0.9]
# only 2 with correlation between 0.4 and 0.6, 479770 is binary, 501307 continuous

data479770 <- binaryDatasets[["479770"]]$data
NMAdata479770 <- make.jagsNMA.data(studyid=id, t=t, r=r, n=n,
                                    data=data479770, type="binary", reference = min(data479770$t))
NMAjags479770 <- jags.parallel(data = NMAdata479770, inits = NULL,
                               parameters.to.save = c("LOR"),
                               n.chains = 2, n.iter = 10000,
                               n.burnin = 1000, DIC=F, n.thin=2,
                               model.file = modelNMABinary.SUCRA)


# data501307 <- continuousDatasets[["501307"]]$data
# NMAdata501307 <- make.jagsNMA.data(studyid = id, t=t, y=y, sd=sd, n=n,
#                                    data = data501307, type = "cont", reference = min(data501307$t))
# NMAjags501307 <- jags.parallel(data = NMAdata501307, inits = NULL,
#                                parameters.to.save = c("SMD","tau", "SUCRA"),
#                                n.chains = 2, n.iter = 10000,
#                                n.burnin = 1000, DIC=F, n.thin=2,
#                                model.file = modelNMAContinuous)
#

# transform jags output in mcmc list for plotting it
NMA479770mcmc <- as.mcmc(NMAjags479770)
plot(NMA479770mcmc, trace = F, ask = T)


# plot normal distributions from netmeta (frequentist model)
nma479770 <- binaryNetObjects[["479770"]]$netobj
mean <- t(nma479770$TE.random)[upper.tri(nma479770$TE.random)] ## save relative treatment effects in a vector
sd <- t(nma479770$seTE.random)[upper.tri(nma479770$seTE.random)] ## save std err in a vector

x <- lapply(1:10, function(i) {
      seq(-4,4,length=100)*sd[i] + mean[i]
})
names(x) <- paste("z", colnames(NMA479770mcmc[[1]]))

y <- lapply(1:10, function(i) {
      dnorm(x[[i]], mean[i], sd[i])
})
names(y) <- paste("pr", colnames(NMA479770mcmc[[1]]))


xy <- data.frame(x,y)


pdf("~/Virginia/PhD project/Papers/Empirical evaluation ranking/SUCRAasymmetricDist.pdf")
NMA479770mcmcDF <- as.data.frame(NMA479770mcmc[[1]])
par(mfrow=c(5,2),
    oma = c(1,1,1,0) + 0.1,
    mar = c(1,1,3,1) + 0.1)
lapply(1:10, function(i) {
  plot(density(NMA479770mcmcDF[,i]), main = names(NMA479770mcmcDF[i]), ylim = range(c(0,2)), ylab = "")
  lines(xy[,i], xy[,i+10], col="red", lwd=2)
})
dev.off()
