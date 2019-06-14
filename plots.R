### graphs to show relationship between correlations and networks measures (avg sample size, avg precision)


par(mfrow=c(2,4))


# plots of average sample size per treatment in a network and correlations
plot(samp_nt, pBVvsSUCRA_k)         # kendall correlation
plot(samp_nt, pBVvsSUCRA_s)         # spearman correlation
plot(samp_nt, SUCRAvsSUCRAjags_k)   # kendall correlation
plot(samp_nt, SUCRAvsSUCRAjags_s)   # spearman correlation


# plots of normalised precision and correlations involving Avg TE
plot(AvgTEprec, SUCRAvsAvgTE_k)   # kendall correlation
plot(AvgTEprec, SUCRAvsAvgTE_s)   # spearman correlation
plot(AvgTEprec, pBVvsAvgTE_k)     # kendall correlation
plot(AvgTEprec, pBVvsAvgTE_s)     # spearman correlation

