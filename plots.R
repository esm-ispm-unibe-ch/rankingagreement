### graphs to show relationship between correlations and networks measures (avg sample size, avg precision)


par(mfrow=c(2,4))

#jpeg('plot_sampnt.jpg')
# plots of average sample size per treatment in a network and correlations
plot(samp_nt, pBVvsSUCRA_k)         # kendall correlation
plot(samp_nt, pBVvsSUCRA_s)         # spearman correlation
plot(samp_nt, SUCRAvsSUCRAjags_k)   # kendall correlation
plot(samp_nt, SUCRAvsSUCRAjags_s)   # spearman correlation
plot(samp_nt, SUCRAvsAvgTE_k)   # kendall correlation
plot(samp_nt, SUCRAvsAvgTE_s)   # spearman correlation
plot(samp_nt, pBVvsAvgTE_k)   # kendall correlation
plot(samp_nt, pBVvsAvgTE_s)   # spearman correlation


#jpeg('plot_avgTEv.jpg')
# plots of normalised precision and correlations involving Avg TE
plot(AvgTEprec_v, SUCRAvsAvgTE_k)   # kendall correlation
plot(AvgTEprec_v, SUCRAvsAvgTE_s)   # spearman correlation
plot(AvgTEprec_v, pBVvsAvgTE_k)     # kendall correlation
plot(AvgTEprec_v, pBVvsAvgTE_s)     # spearman correlation
plot(AvgTEprec_v, pBVvsSUCRA_k)         # kendall correlation
plot(AvgTEprec_v, pBVvsSUCRA_s)         # spearman correlation
plot(AvgTEprec_v, SUCRAvsSUCRAjags_k)   # kendall correlation
plot(AvgTEprec_v, SUCRAvsSUCRAjags_s)   # spearman correlation


#jpeg('plot_avgTEavg.jpg')
# plots of average precision and correlations involving Avg TE
plot(AvgTEprec_avg, SUCRAvsAvgTE_k)   # kendall correlation
plot(AvgTEprec_avg, pBVvsAvgTE_k)     # kendall correlation
plot(AvgTEprec_avg, SUCRAvsAvgTE_s)   # spearman correlation
plot(AvgTEprec_avg, pBVvsAvgTE_s)     # spearman correlation
plot(AvgTEprec_avg, pBVvsSUCRA_k)         # kendall correlation
plot(AvgTEprec_avg, pBVvsSUCRA_s)         # spearman correlation
plot(AvgTEprec_avg, SUCRAvsSUCRAjags_k)   # kendall correlation
plot(AvgTEprec_avg, SUCRAvsSUCRAjags_s)   # spearman correlation



#dev.off()
