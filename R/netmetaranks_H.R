### create ranking metrics from netmeta results for negative outcomes


netmetaranks_H <- function(nma, nsim) {

  rte <- nma$TE.random   ## relative treatment effects (already in logOR)
  rte.v <- t(rte)[lower.tri(rte)] ## save relative treatment effects in a vector

  ## simulates relative treatment effects from multinormal distribution
  library(mvtnorm)
  nma.mvnorm <- rmvnorm(nsim, rte.v, nma$Cov.random)  ## nsim simulation of the relative treatment effects from multivariate normal

  # ### calculate probabilities that a treatment is better than the other for each comparison
  # p.rte=c()
  # for (i in 1:ncol(nma.mvnorm)) {
  #   p.rte[i] <- sum(nma.mvnorm[,i]<0)/nrow(nma.mvnorm)
  # }
  # names(p.rte) <- names(rte.v)

  ### calculate probabilities that a treatment ranks at a particular position
  ## first rearrange all relative treatment effects into matrices for all simulations
  nma.mvnorm.m <- array(NA,c(nrow(rte),ncol(rte),nrow(nma.mvnorm)),dimnames = list(rownames(rte), colnames(rte), 1:nsim))
  for (i in 1:nrow(nma.mvnorm)) {
    nma.mvnorm.m[,,i][lower.tri(nma.mvnorm.m[,,i])] <- nma.mvnorm[i,]
    nma.mvnorm.m[,,i] <- t(nma.mvnorm.m[,,i])
    nma.mvnorm.m[,,i][lower.tri(nma.mvnorm.m[,,i])] <- -nma.mvnorm[i,]
  }
  ## then calculate the treatment ranking in each simulation (matrix) - do this by counting the signs
  rankings <- matrix(NA, nrow(nma.mvnorm), ncol(rte), dimnames = list(1:nrow(nma.mvnorm), colnames(rte)))
  for (i in 1:nsim) {
    rankings[i,] <- rank(rowSums(nma.mvnorm.m[,,i] > 0, na.rm = T))
  }
  ## then calculate the probabilities for each treatment to rank at a particular position and store them in matrix p.rank
  p.rank=matrix(,nrow(rte),ncol(rte),dimnames = list(rownames(rte), 1:nma$n))
  for (i in 1:nrow(p.rank)) {
    for (j in 1:ncol(p.rank)) {
      p.rank[i,j] <- sum(rankings[,i]==j)/nsim
    }
  }
  #apply(p.rank, 1, sum) ## check probabilities add to 1
  ## plot rankograms
  # par(mfrow=c(3,3), las=1)
  # for (i in 1:nrow(p.rank)) {
  #   plot(p.rank[i,], type = "l", main = rownames(p.rank)[i], ylab = "", xlab = "Ranks", ylim = c(0,0.8))
  #   axis(1, at=seq(1,nma$n))
  # }

  ## probability of being the best
  p.rank_1st <- c()
  for (i in 1:nrow(p.rank)) {
    p.rank_1st[i]=p.rank[i,1]
  }
  #create vector of rankings based on prob of being the best
  p1st_rankings=(nrow(p.rank) + 1) - rank(p.rank_1st)


  ## calculate SUCRAs
  # first calculate cumulative probabilities
  p.rank.cum=matrix(,nrow(p.rank),ncol(p.rank),dimnames = list(rownames(p.rank), 1:nma$n))
  for (i in 1:nrow(p.rank)) {
    p.rank.cum[i,] <- cumsum(p.rank[i,])
  }
  # then calculate SUCRA for each treatment
  SUCRA <- vector(, length = nrow(p.rank))
  names(SUCRA) <- rownames(p.rank)
  for (i in 1:nrow(p.rank.cum)) {
    SUCRA[i] <- round(sum(p.rank.cum[i,-length(p.rank.cum[i,])])/(nrow(p.rank.cum)-1), digits = 5)
  }
  #create vector of rankings based on SUCRA
  SUCRA_ranks=(nrow(p.rank) + 1) - rank(SUCRA)

  # ## plot SUCRAs
  # par(mfrow=c(3,3), las=1)
  # for (i in 1:nrow(p.rank)) {
  #   plot(p.rank.cum[i,], type = "l", main = rownames(p.rank)[i], ylab = "Cumulative probability", xlab = "Ranks", ylim = c(0,1))
  #   axis(1, at=seq(1,nma1$n))
  #   text(5, 0.2, paste0(SUCRA[i]*100, "%"))
  # }
  # cbind(SUCRA, netrank(nma1)$Pscore.random) ## check it is the same as p-score from netrank

  ## calculate mean rank
  mean.rank <- colMeans(rankings)
  #mean.rank.S <- nma$n - (nma$n - 1)*SUCRA ## check mean ranks are transformation of SUCRAs
  #cbind(mean.rank, mean.rank.S)

  ## calculate median rank
  med.rank<- structure(rep(NA, length=nma$n), names = colnames(rankings))
  for (i in 1:nrow(p.rank)) {
    med.rank[i] <- median(rankings[,i])

  ##calculate vector of rankings based on P-score
  Pscore_ranks=(nrow(p.rank) + 1) - rank(netrank(nma)$Pscore.random)
  }

  ## view all ranking metrics together
  return(cbind("pBV"=p.rank_1st, "pBV ranks"=p1st_rankings, SUCRA, SUCRA_ranks, "P-score"=netrank(nma)$Pscore.random, Pscore_ranks, "Mean rank"=mean.rank, "Median rank"=med.rank))


}
