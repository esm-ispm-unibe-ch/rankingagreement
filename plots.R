### graphs to show relationship between correlations and networks measures (avg sample size, avg precision)

library(splines)


# plots of average sample size per treatment in a network and correlations
pdf('plots_sampnt.pdf')

par(mfrow=c(2,4))

plot(log(samp_nt), pBVvsSUCRA_k,
     xlab = "log of average sample size \n per treatment",
     ylab = "Kendall tau pBV vs SUCRA")         # kendall correlation
lines(smooth.spline(log(samp_nt), pBVvsSUCRA_k, df = 3),lwd=2,col="purple")
plot(log(samp_nt), pBVvsSUCRA_s,
     xlab = "log of average sample size \n per treatment",
     ylab = "Spearman rho pBV vs SUCRA")         # spearman correlation
lines(smooth.spline(log(samp_nt), pBVvsSUCRA_s, df = 3),lwd=2,col="purple")
plot(log(samp_nt), SUCRAvsSUCRAjags_k,
     xlab = "log of average sample size \n per treatment",
     ylab = "Kendall tau SUCRA vs Bayesian SUCRA")   # kendall correlation
lines(smooth.spline(log(samp_nt), SUCRAvsSUCRAjags_k, df = 3),lwd=2,col="purple")
plot(log(samp_nt), SUCRAvsSUCRAjags_s,
     xlab = "log of average sample size \n per treatment",
     ylab = "Spearman rho SUCRA vs Bayesian SUCRA")   # spearman correlation
lines(smooth.spline(log(samp_nt), SUCRAvsSUCRAjags_s, df = 3),lwd=2,col="purple")
plot(log(samp_nt), SUCRAvsAvgTE_k,
     xlab = "log of average sample size \n per treatment",
     ylab = "Kendall tau SUCRA vs average treatment effect")   # kendall correlation
lines(smooth.spline(log(samp_nt), SUCRAvsAvgTE_k, df = 3),lwd=2,col="purple")
plot(log(samp_nt), SUCRAvsAvgTE_s,
     xlab = "log of average sample size \n per treatment",
     ylab = "Spearman rho SUCRA vs average treatment effect")   # spearman correlation
lines(smooth.spline(log(samp_nt), SUCRAvsAvgTE_s, df = 3),lwd=2,col="purple")
plot(log(samp_nt), pBVvsAvgTE_k,
     xlab = "log of average sample size \n per treatment",
     ylab = "Kendall tau pBV vs average treatment effect")   # spearman correlation
lines(smooth.spline(log(samp_nt), pBVvsAvgTE_k, df = 3),lwd=2,col="purple")
plot(log(samp_nt), pBVvsAvgTE_s,
     xlab = "log of average sample size \n per treatment",
     ylab = "Spearman rho pBV vs average treatment effect")   # kendall correlation
lines(smooth.spline(log(samp_nt), pBVvsAvgTE_k, df = 3),lwd=2,col="purple")

dev.off()



# plots of normalised precision and correlations
pdf('plots_prec_v.pdf')

par(mfrow=c(2,4))

plot(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsAvgTE_k,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Kendall tau SUCRA vs average treatment effect")   # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsAvgTE_k, df = 3),lwd=2,col="purple")
plot(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsAvgTE_s,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Spearman rho SUCRA vs average treatment effect")   # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsAvgTE_s, df = 3),lwd=2,col="purple")
plot(log(log(1/as.numeric(AvgTEprec_v))), pBVvsAvgTE_k,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Kendall tau pBV vs average treatment effect")     # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), pBVvsAvgTE_k, df = 3),lwd=2,col="purple")
plot(log(log(1/as.numeric(AvgTEprec_v))), pBVvsAvgTE_s,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Spearman rho pBV vs average treatment effect")     # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), pBVvsAvgTE_s, df = 3),lwd=2,col="purple")
plot(log(log(1/as.numeric(AvgTEprec_v))), pBVvsSUCRA_k,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Kendall tau pBV vs SUCRA")         # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), pBVvsSUCRA_k, df = 3),lwd=2,col="purple")
plot(log(log(1/as.numeric(AvgTEprec_v))), pBVvsSUCRA_s,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Spearman rho pBV vs SUCRA")         # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), pBVvsSUCRA_s, df = 3),lwd=2,col="purple")
plot(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsSUCRAjags_k,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Kendall tau SUCRA vs Bayesian SUCRA")   # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsSUCRAjags_k, df = 3),lwd=2,col="purple")
plot(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsSUCRAjags_s,
     xlab = "log of log of inverse \n of precision variation",
     ylab = "Spearman rho SUCRA vs Bayesian SUCRA")   # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_v))), SUCRAvsSUCRAjags_s, df = 3),lwd=2,col="purple")

dev.off()



# plots of average precision and correlations
pdf('plots_prec_avg.pdf')

par(mfrow=c(2,4))

plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_k,
     xlab = "log of average precision",
     ylab = "Kendall tau SUCRA vs average treatment effect")   # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_k, df = 3),lwd=2,col="purple")
plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_s,
     xlab = "log of average precision",
     ylab = "Spearman rho SUCRA vs average treatment effect")   # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_s, df = 3),lwd=2,col="purple")
plot(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_k,
     xlab = "log of average precision",
     ylab = "Kendall tau pBV vs average treatment effect")     # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_k, df = 3),lwd=2,col="purple")
plot(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_s,
     xlab = "log of average precision",
     ylab = "Spearman rho pBV vs average treatment effect")     # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_s, df = 3),lwd=2,col="purple")
plot(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_k,
     xlab = "log of average precision",
     ylab = "Kendall tau pBV vs SUCRA")         # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_k, df = 3),lwd=2,col="purple")
plot(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_s,
     xlab = "log of average precision",
     ylab = "Spearman rho pBV vs SUCRA")         # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_s, df = 3),lwd=2,col="purple")
plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_k,
     xlab = "log of average precision",
     ylab = "Kendall tau SUCRA vs Bayesian SUCRA")   # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_k, df = 3),lwd=2,col="purple")
plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_s,
     xlab = "log of average precision",
     ylab = "Spearman rho SUCRA vs Bayesian SUCRA")   # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_s, df = 3),lwd=2,col="purple")


dev.off()

