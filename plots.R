### graphs to show relationship between correlations and networks measures (avg sample size, avg precision)

library(splines)


# plots of average sample size per treatment in a network and spearman/kendall correlations
pdf('plots_sampnt.pdf')


par(mfrow=c(2,4),
    oma = c(1,1,0,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)


plot(log(samp_nt), pBVvsSUCRA_k,
     ylab = "Kendall tau pBV vs SUCRA", ylim=c(0,1))      # kendall correlation
lines(smooth.spline(log(samp_nt), pBVvsSUCRA_k, df = 5),lwd=2,col="purple")

plot(log(samp_nt), pBVvsSUCRA_s,
     ylab = "Spearman rho pBV vs SUCRA", ylim=c(0,1))    # spearman correlation
lines(smooth.spline(log(samp_nt), pBVvsSUCRA_s, df = 5),lwd=2,col="purple")

plot(log(samp_nt), SUCRAvsSUCRAjags_k,
     ylab = "Kendall tau SUCRA vs Bayesian SUCRA", ylim=c(0,1))  # kendall correlation
lines(smooth.spline(log(samp_nt), SUCRAvsSUCRAjags_k, df = 5),lwd=2,col="purple")

plot(log(samp_nt), SUCRAvsSUCRAjags_s,
     ylab = "Spearman rho SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # spearman correlation
lines(smooth.spline(log(samp_nt), SUCRAvsSUCRAjags_s, df = 5),lwd=2,col="purple")

plot(log(samp_nt), SUCRAvsAvgTE_k,
     ylab = "Kendall tau SUCRA vs average treatment effect", ylim=c(0,1))   # kendall correlation
lines(smooth.spline(log(samp_nt), SUCRAvsAvgTE_k, df = 5),lwd=2,col="purple")

plot(log(samp_nt), SUCRAvsAvgTE_s,
     ylab = "Spearman rho SUCRA vs average treatment effect", ylim=c(0,1))   # spearman correlation
lines(smooth.spline(log(samp_nt), SUCRAvsAvgTE_s, df = 5),lwd=2,col="purple")

plot(log(samp_nt), pBVvsAvgTE_k,
     ylab = "Kendall tau pBV vs average treatment effect", ylim=c(0,1))   # kendall correlation
lines(smooth.spline(log(samp_nt), pBVvsAvgTE_k, df = 5),lwd=2,col="purple")

plot(log(samp_nt), pBVvsAvgTE_s,
     ylab = "Spearman rho pBV vs average treatment effect", ylim=c(0,1))   # spearman correlation
lines(smooth.spline(log(samp_nt), pBVvsAvgTE_s, df = 5),lwd=2,col="purple")

dev.off()



# plots of average sample size per treatment in a network and Yilmaz AP/average overlap
pdf('plots_sampnt_AP&AO.pdf')


par(mfrow=c(2,4),
    oma = c(1,1,0,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)



plot(log(samp_nt), pBVvsSUCRA_AP,
     ylab = "Yilmaz tauAP pBV vs SUCRA", ylim=c(0,1))         # Yilmaz tauAP
lines(smooth.spline(log(samp_nt), pBVvsSUCRA_AP, df = 5),lwd=2,col="purple")

plot(log(samp_nt[match(names(pBVvsSUCRA_AO), names(samp_nt))]), pBVvsSUCRA_AO,
     ylab = "Averge Overlap pBV vs SUCRA", ylim=c(0,1))         # Averge Overlap
lines(smooth.spline(log(samp_nt[match(names(pBVvsSUCRA_AO), names(samp_nt))]), pBVvsSUCRA_AO, df = 3),lwd=2,col="purple")

plot(log(samp_nt), SUCRAvsSUCRAjags_AP,
     ylab = "Yilmaz tauAP SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # Yilmaz tauAP
lines(smooth.spline(log(samp_nt), SUCRAvsSUCRAjags_AP, df = 5),lwd=2,col="purple")

plot(log(samp_nt[match(names(SUCRAvsSUCRAjags_AO), names(samp_nt))]), SUCRAvsSUCRAjags_AO,
     ylab = "Averge Overlap SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # Averge Overlap
lines(smooth.spline(log(samp_nt[match(names(SUCRAvsSUCRAjags_AO), names(samp_nt))]), SUCRAvsSUCRAjags_AO, df = 3),lwd=2,col="purple")

plot(log(samp_nt), SUCRAvsAvgTE_AP,
     ylab = "Yilmaz tauAP SUCRA vs average treatment effect", ylim=c(0,1))   # Yilmaz tauAP
lines(smooth.spline(log(samp_nt), SUCRAvsAvgTE_AP, df = 5),lwd=2,col="purple")

plot(log(samp_nt[match(names(SUCRAvsAvgTE_AO), names(samp_nt))]), SUCRAvsAvgTE_AO,
     ylab = "Averge Overlap SUCRA vs average treatment effect", ylim=c(0,1))   # Averge Overlap
lines(smooth.spline(log(samp_nt[match(names(SUCRAvsAvgTE_AO), names(samp_nt))]), SUCRAvsAvgTE_AO, df = 3),lwd=2,col="purple")

plot(log(samp_nt), pBVvsAvgTE_AP,
     ylab = "Yilmaz tauAP pBV vs average treatment effect", ylim=c(0,1))   # Yilmaz tauAP
lines(smooth.spline(log(samp_nt), pBVvsAvgTE_AP, df = 5),lwd=2,col="purple")

plot(log(samp_nt[match(names(pBVvsAvgTE_AO), names(samp_nt))]), pBVvsAvgTE_AO,
     ylab = "Averge Overlap pBV vs average treatment effect", ylim=c(0,1))   # Averge Overlap
lines(smooth.spline(log(samp_nt[match(names(pBVvsAvgTE_AO), names(samp_nt))]), pBVvsAvgTE_AO, df = 3),lwd=2,col="purple")

dev.off()




# plots of relative range precision and correlations
pdf('plots_prec_range.pdf')

par(mfrow=c(2,4),
    oma = c(1,1,0,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)


plot(log(log(1/as.numeric(AvgTEprec_range))), pBVvsSUCRA_k,
     ylab = "Kendall tau pBV vs SUCRA", ylim=c(0,1))         # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), pBVvsSUCRA_k, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range))), pBVvsSUCRA_s,
     ylab = "Spearman rho pBV vs SUCRA", ylim=c(0,1))         # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), pBVvsSUCRA_s, df = 5),lwd=2,col="purple")


plot(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsSUCRAjags_k,
     ylab = "Kendall tau SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsSUCRAjags_k, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsSUCRAjags_s,
     ylab = "Spearman rho SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsSUCRAjags_s, df = 5),lwd=2,col="purple")


plot(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsAvgTE_k,
     ylab = "Kendall tau SUCRA vs average treatment effect", ylim=c(0,1))   # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsAvgTE_k, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsAvgTE_s,
     ylab = "Spearman rho SUCRA vs average treatment effect", ylim=c(0,1))   # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsAvgTE_s, df = 5),lwd=2,col="purple")


plot(log(log(1/as.numeric(AvgTEprec_range))), pBVvsAvgTE_k,
     ylab = "Kendall tau pBV vs average treatment effect", ylim=c(0,1))     # kendall correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), pBVvsAvgTE_k, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range))), pBVvsAvgTE_s,
     ylab = "Spearman rho pBV vs average treatment effect", ylim=c(0,1))     # spearman correlation
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), pBVvsAvgTE_s, df = 5),lwd=2,col="purple")

dev.off()




# plots of relative range precision and Yilmaz AP/average overlap
pdf('plots_prec_range_AP&AO.pdf')


par(mfrow=c(2,4),
    oma = c(1,1,0,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)



plot(log(log(1/as.numeric(AvgTEprec_range))), pBVvsSUCRA_AP,
     ylab = "Yilmaz tauAP pBV vs SUCRA", ylim=c(0,1))         # Yilmaz tauAP
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), pBVvsSUCRA_AP, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range[match(names(pBVvsSUCRA_AO), names(AvgTEprec_range))]))), pBVvsSUCRA_AO,
     ylab = "Average overlap pBV vs SUCRA", ylim=c(0,1))         # Average Overlap
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range[match(names(pBVvsSUCRA_AO), names(AvgTEprec_range))]))),
                    pBVvsSUCRA_AO, df = 3),lwd=2,col="purple")


plot(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsSUCRAjags_AP,
     ylab = "Yilmaz tauAP SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # Yilmaz tauAP
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsSUCRAjags_AP, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range[match(names(SUCRAvsSUCRAjags_AO), names(AvgTEprec_range))]))), SUCRAvsSUCRAjags_AO,
     ylab = "Average overlap SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # Average Overlap
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range[match(names(SUCRAvsSUCRAjags_AO), names(AvgTEprec_range))]))),
                    SUCRAvsSUCRAjags_AO, df = 3),lwd=2,col="purple")


plot(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsAvgTE_AP,
     ylab = "Yilmaz tauAP SUCRA vs average treatment effect", ylim=c(0,1))   # Yilmaz tauAP
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), SUCRAvsAvgTE_AP, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range[match(names(SUCRAvsAvgTE_AO), names(AvgTEprec_range))]))), SUCRAvsAvgTE_AO,
     ylab = "Average overlap SUCRA vs average treatment effect", ylim=c(0,1))   # Average Overlap
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range[match(names(SUCRAvsAvgTE_AO), names(AvgTEprec_range))]))),
                    SUCRAvsAvgTE_AO, df = 3),lwd=2,col="purple")


plot(log(log(1/as.numeric(AvgTEprec_range))), pBVvsAvgTE_AP,
     ylab = "Yilmaz tauAP pBV vs average treatment effect", ylim=c(0,1))     # Yilmaz tauAP
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range))), pBVvsAvgTE_AP, df = 5),lwd=2,col="purple")

plot(log(log(1/as.numeric(AvgTEprec_range[match(names(pBVvsAvgTE_AO), names(AvgTEprec_range))]))), pBVvsAvgTE_AO,
     ylab = "Average overlap pBV vs average treatment effect", ylim=c(0,1))     # Average Overlap
lines(smooth.spline(log(log(1/as.numeric(AvgTEprec_range[match(names(pBVvsAvgTE_AO), names(AvgTEprec_range))]))),
                    pBVvsAvgTE_AO, df = 3),lwd=2,col="purple")


dev.off()




# plots of average precision and correlations
pdf('plots_prec_avg.pdf')

par(mfrow=c(2,4),
    oma = c(1,1,0,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)


plot(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_k,
     ylab = "Kendall tau pBV vs SUCRA", ylim=c(0,1))         # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_k, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_s,
     ylab = "Spearman rho pBV vs SUCRA", ylim=c(0,1))         # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_s, df = 5),lwd=2,col="purple")


plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_k,
     ylab = "Kendall tau SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_k, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_s,
     ylab = "Spearman rho SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_s, df = 5),lwd=2,col="purple")


plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_k,
     ylab = "Kendall tau SUCRA vs average treatment effect", ylim=c(0,1))   # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_k, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_s,
     ylab = "Spearman rho SUCRA vs average treatment effect", ylim=c(0,1))   # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_s, df = 5),lwd=2,col="purple")


plot(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_k,
     ylab = "Kendall tau pBV vs average treatment effect", ylim=c(0,1))     # kendall correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_k, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_s,
     ylab = "Spearman rho pBV vs average treatment effect", ylim=c(0,1))     # spearman correlation
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_s, df = 5),lwd=2,col="purple")


dev.off()




# plots of average precision and Yilmaz AP/average overlap
pdf('plots_prec_avg_AP&AO.pdf')

par(mfrow=c(2,4),
    oma = c(1,1,0,0) + 0.1,
    mar = c(1,4,1,1) + 0.1)



plot(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_AP,
     ylab = "Yilmaz tauAP pBV vs SUCRA", ylim=c(0,1))          # Yilmaz tauAP
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsSUCRA_AP, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg[match(names(pBVvsSUCRA_AO), names(AvgTEprec_avg))])), pBVvsSUCRA_AO,
     ylab = "Average overlap pBV vs SUCRA", ylim=c(0,1))         # Average Overlap
lines(smooth.spline(log(as.numeric(AvgTEprec_avg[match(names(pBVvsSUCRA_AO), names(AvgTEprec_avg))])),
                    pBVvsSUCRA_AO, df = 3),lwd=2,col="purple")


plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_AP,
     ylab = "Yilmaz tauAP SUCRA vs Bayesian SUCRA", ylim=c(0,1))    # Yilmaz tauAP
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsSUCRAjags_AP, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg[match(names(SUCRAvsSUCRAjags_AO), names(AvgTEprec_avg))])), SUCRAvsSUCRAjags_AO,
     ylab = "Average overlap SUCRA vs Bayesian SUCRA", ylim=c(0,1))   # Average Overlap
lines(smooth.spline(log(as.numeric(AvgTEprec_avg[match(names(SUCRAvsSUCRAjags_AO), names(AvgTEprec_avg))])),
                    SUCRAvsSUCRAjags_AO, df = 3),lwd=2,col="purple")


plot(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_AP,
     ylab = "Yilmaz tauAP SUCRA vs average treatment effect", ylim=c(0,1))   # Yilmaz tauAP
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), SUCRAvsAvgTE_AP, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg[match(names(SUCRAvsAvgTE_AO), names(AvgTEprec_avg))])), SUCRAvsAvgTE_AO,
     ylab = "Average overlap SUCRA vs average treatment effect", ylim=c(0,1))   # Average Overlap
lines(smooth.spline(log(as.numeric(AvgTEprec_avg[match(names(SUCRAvsAvgTE_AO), names(AvgTEprec_avg))])),
                    SUCRAvsAvgTE_AO, df = 3),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_AP,
     ylab = "Yilmaz tauAP pBV vs average treatment effect", ylim=c(0,1))      # Yilmaz tauAP
lines(smooth.spline(log(as.numeric(AvgTEprec_avg)), pBVvsAvgTE_AP, df = 5),lwd=2,col="purple")

plot(log(as.numeric(AvgTEprec_avg[match(names(pBVvsAvgTE_AO), names(AvgTEprec_avg))])), pBVvsAvgTE_AO,
     ylab = "Average overlap pBV vs average treatment effect", ylim=c(0,1))    # Average Overlap
lines(smooth.spline(log(as.numeric(AvgTEprec_avg[match(names(pBVvsAvgTE_AO), names(AvgTEprec_avg))])),
                    pBVvsAvgTE_AO, df = 3),lwd=2,col="purple")


dev.off()
