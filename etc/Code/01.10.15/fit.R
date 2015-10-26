fitIt <- function(){
  .GlobalEnv$nmethod <- c("AEV.MM", "AEV.LM", "AEV.ML",
                              "Weibull.MM", "Weibull.LM", "Weibull.ML",
                              "logW.MM", "logW.LM", "logW.ML",
                              "gumbel.MM", "gumbel.LM", "gumbel.ML",
                              "expo.MM", "expo.LM", "expo.ML",
                              "pears.MM", "pears.LM", "pears.ML",
                              "log-pears.MM", "log-pears.LM", "log-pears.ML",
                              "gamma.MM", "gamma.LM", "gamma.ML",
                              "frecht.MM", "frecht.LM", "frecht.ML",
                              "normal.MM", "normal.LM", "normal.ML",
                              "log-norm.MM", "log-norm.LM", "log-norm.ML",
                              "3-par-log-norm.MM", "3-par-log-norm.LM", "3-par-log-norm.ML"
                              )
  .GlobalEnv$distribution <- c("GEVDistr", "GEVDistr", "GEVDistr",
                               "WeibullDistr", "WeibullDistr", "WeibullDistr",
                               "LogWeibullDistr", "LogWeibullDistr", "LogWeibullDistr",
                               "GumbelDistr", "GumbelDistr", "GumbelDistr",
                               "ExponentialDistr", "ExponentialDistr", "ExponentialDistr",
                               "Pear3Distr", "Pear3Distr", "Pear3Distr",
                               "LogPear3Distr", "LogPear3Distr", "LogPear3Distr",
                               "GammaDistr", "GammaDistr", "GammaDistr",
                               "FrechetDistr", "FrechetDistr", "FrechetDistr",
                               "NormalDistr", "NormalDistr", "NormalDistr",
                               "LogNormalDistr", "LogNormalDistr", "LogNormalDistr",
                               "LogNormal3PDistr", "LogNormal3PDistr", "LogNormal3PDistr"
                               )
  .GlobalEnv$nfitFunction <- c(distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML,
                               distrFitMM, distrFitLM, distrFitML
                               )
  
  .GlobalEnv$check.index <- which(c(
                                svalue(.GlobalEnv$check.AEV.MM),
                                svalue(.GlobalEnv$check.AEV.LM),
                                svalue(.GlobalEnv$check.AEV.ML),
                                svalue(.GlobalEnv$check.Weibull.MM),
                                svalue(.GlobalEnv$check.Weibull.LM),
                                svalue(.GlobalEnv$check.Weibull.ML),
                                svalue(.GlobalEnv$check.logW.MM),
                                svalue(.GlobalEnv$check.logW.LM),
                                svalue(.GlobalEnv$check.logW.ML),
                                svalue(.GlobalEnv$check.gumbel.MM),
                                svalue(.GlobalEnv$check.gumbel.LM),
                                svalue(.GlobalEnv$check.gumbel.ML),
                                svalue(.GlobalEnv$check.expo.MM),
                                svalue(.GlobalEnv$check.expo.LM),
                                svalue(.GlobalEnv$check.expo.ML),
                                svalue(.GlobalEnv$check.pears.MM),
                                svalue(.GlobalEnv$check.pears.LM),
                                svalue(.GlobalEnv$check.pears.ML),
                                svalue(.GlobalEnv$check.logP.MM),
                                svalue(.GlobalEnv$check.logP.LM),
                                svalue(.GlobalEnv$check.logP.ML),
                                svalue(.GlobalEnv$check.gamma.MM),
                                svalue(.GlobalEnv$check.gamma.LM),
                                svalue(.GlobalEnv$check.gamma.ML),
                                svalue(.GlobalEnv$check.frech.MM),
                                svalue(.GlobalEnv$check.frech.LM),
                                svalue(.GlobalEnv$check.frech.ML),
                                svalue(.GlobalEnv$check.normal.MM),
                                svalue(.GlobalEnv$check.normal.LM),
                                svalue(.GlobalEnv$check.normal.ML),
                                svalue(.GlobalEnv$check.logN.MM),
                                svalue(.GlobalEnv$check.logN.LM),
                                svalue(.GlobalEnv$check.logN.ML),
                                svalue(.GlobalEnv$check.log3PN.MM),
                                svalue(.GlobalEnv$check.log3PN.LM),
                                svalue(.GlobalEnv$check.log3PN.ML)
                              ) == TRUE)
  .GlobalEnv$colors <- .GlobalEnv$check.index
  
  par(mar=c(5, 4.1, 1.5, 12), xpd=TRUE)
  #marge used to show the legend
  to.add <- FALSE
  .GlobalEnv$objects <- vector()
  for(i in .GlobalEnv$check.index){
    a <- .GlobalEnv$nfitFunction[[i]](.GlobalEnv$distribution[i],
                                       .GlobalEnv$activeDataSet[, .GlobalEnv$activeVar]) 
    distrPlot(a, trans = TRUE, add = to.add, col = .GlobalEnv$colors[i])
    to.add <- TRUE
    .GlobalEnv$objects <- c(.GlobalEnv$objects, a)
  }
  legend("topright", col=colors,
         legend=.GlobalEnv$nmethod[.GlobalEnv$check.index],
         inset=c(-0.5, 0), cex=0.8, lty=rep(1, length(.GlobalEnv$check.index)),
         box.col=NA)

}