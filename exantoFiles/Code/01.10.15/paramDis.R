# Parameters of GEVDistr
setEvalStatAndLMoment <- function(datafrm, meann = 0, sdd = 1,
                                  varr = 1, var.coef = 0, schiefe = 0,
                                  kurtosiss = 0, L1 = 0, L2 = 0, L3 = 0, L4 = 0){
  datafrm[datafrm$A == "Mittelwert:", "B"] <- meann
  datafrm[datafrm$A == "Standardabweichung:", "B"] <- sdd
  datafrm[datafrm$A == "Varianz:", "B"] <- varr
  datafrm[datafrm$A == "Variationskoeffizient:", "B"] <- var.coef
  datafrm[datafrm$A == "Schiefe:", "B"] <- schiefe
  datafrm[datafrm$A == "Kurtosis:", "B"] <- kurtosiss
  datafrm[datafrm$A == "L1:", "B"] <- L1
  datafrm[datafrm$A == "L2:", "B"] <- L2
  datafrm[datafrm$A == "L3:", "B"] <- L3
  datafrm[datafrm$A == "L4:", "B"] <- L4
  return(datafrm)
}
# bulding of head of excel files

# function to set the sample value
setSampleValue <- function(datafrm, m = 1:10, t = 1:10,
                           pu = 1:10, yT = 1:10, 
                           HQ = 1:10){
  data.value <- rbind(.GlobalEnv$head.stichProb, cbind(m, t, pu, yT, HQ, 
                                                       NA, NA))
  colnames(data.value) <- colnames(datafrm)
  return(rbind(datafrm, data.value))
}

# createQuantileFrame builds the quantile frame to be added to spefic data frame.
# Note that the returned data frame will have 7 colunm, while the bis yet original
# data frame had 6 colunm. => Attention while binding the two data frames!
setQuantileFrame <- function(datafrm, names.qval = tail.quantile,
                             probs = probabilities){
  Tn <- 1 / (1 - probs)
  Tp <- 1 / log(Tn / (Tn - 1))
  yT <- log(log(Tn / (Tn - 1)))
  MM <- rep(NA, length(probs))
  LM <- rep(NA, length(probs))
  ML <- rep(NA, length(probs))
  values <- cbind(Tp, Tn, probs, yT, MM, LM, ML)
  q.dfrm <- as.data.frame(rbind(names.qval, values))
  names.datafrm <- names(datafrm)
  #datafrm <- cbind(datafrm, rep(NA, nrow(datafrm)))
  names(datafrm) <- c(names.datafrm)
  names(q.dfrm) <- c(names.datafrm)
  datafrm <- rbind(datafrm, q.dfrm)
  return(datafrm)
}

# getQuantile return the quantile vector of a vector of probabilities gegeben an
# object a.
getQuantile <- function(a = obj, probs = probabilities){
  sapply(probs, function(p){
    qdistr(a, p)
  })
}


setTailValue <- function(datafrm, t = 1:10, pu = 1:10, yT = 1:10,
                         mm = matrix(ncol = 3, nrow = 10)){
  tail.values <- rbind(.GlobalEnv$tail.stichProb, cbind(t, pu, yT, mm, NA))
  colnames(tail.values) <- colnames(datafrm)
  return(rbind(datafrm, tail.values))
}

 setTailParamFitted <- function(datafrm,  a = obj){
   methods <- c("distrFitLM", "distrFitMM", "distrFitML")
   which.mthd <- which(methods == a@method)
   dn <- bootstrapGOFtest(a, testfun = "KS")$testvalue#for Kolmogorov Smirnov
   nw <- bootstrapGOFtest(a, testfun = "nw2")$testvalue#for Cramer-von-Mises test
   qcor <- qqcor(a)
   aic <- AIC(a)
   bic <- BIC(a)
   values <- c(dn, nw, qcor, aic, bic)
   colnames(.GlobalEnv$tail.fitted.test) <- colnames(datafrm)
   .GlobalEnv$tail.fitted.test[4:(3 + length(values)), 1 + which.mthd] <- values[1:length(values)]
   #3:length(values) let's start from Kolmogrov-Smirnov-Test dn= in the table tail
   return(rbind(datafrm, .GlobalEnv$tail.fitted.test))
 }

# Initialize the GEV-dataframe
initDatafrm <- function(){
.GlobalEnv$aev.MM <- c(
  "AUSWERTUNG EXANTO:",
  "Parameter",
  "Allgemeine Extremwertverteilung MM",
  "Allgemeine Extremwertverteilung:",
  "F(x)=exp[-(1-k*(x-u)/a)^(1/k)]",
  "Geschaetzte Parameter:",
  "u =",
  "a =",
  "k ="
  )

.GlobalEnv$aev.LM <- c(
  "Parameter",
  "Allgemeine Extremwertverteilung LM",
  "Allgemeine Extremwertverteilung:",
  "F(x)=exp[-(1-k*(x-u)/a)^(1/k)]",
  "Geschaetzte Parameter:",
  "u =",
  "a =",
  "k ="
)

.GlobalEnv$aev.ML <- c(
  "Parameter",
  "Allgemeine Extremwertverteilung ML",
  "Allgemeine Extremwertverteilung:",
  "F(x)=exp[-(1-k*(x-u)/a)^(1/k)]",
  "Geschaetzte Parameter:",
  "u =",
  "a =",
  "k ="
  )

.GlobalEnv$auswert.stat <- c(
  "Auswertung Statistik",
  "Eigenschaften Stichprobe",
  "Mittelwert:",
  "Standardabweichung:",
  "Varianz:",
  "Variationskoeffizient:",
  "Schiefe:",
  "Kurtosis:",
  "",
  "Wahrscheinlichkeitsgewichtete L-Momente",
  "L1:",
  "L2:",
  "L3:",
  "L4:",
  ""
  )

A <- c(.GlobalEnv$aev.MM, .GlobalEnv$aev.LM, .GlobalEnv$aev.ML, .GlobalEnv$auswert.stat)
.GlobalEnv$GEVDistr <- data.frame(A = A, B = rep(NA,length(A)), 
                                  C = rep(NA,length(A)), D = rep(NA,length(A)),
                                  E = rep(NA,length(A)), FF = rep(NA,length(A)),
                                  G = rep(NA,length(A)))
}# End of data frame intilisation
#@@@@@@@@@@@@@@ Review and complete me please !!!!! @@@@@@@@@@@@@@@@@
#.GlobalEnv$head.stichProb <- rbind(
#  c("Stichprobenwerte", "", "", "", "", ""),
#  c("m", "T", "pu", svalue(.GlobalEnv$radio.btn.achsenskal), HQ)
#  )
#@ hier (upper) is the good version

.GlobalEnv$head.stichProb <- rbind(
    c("Stichprobenwerte", rep(NA, 6)),
    c("m", "T", "pu", "yT=ln(ln(T/(T-1)))", "HQ", NA, NA)
    )

# when the quantile are gave in Entries.
.GlobalEnv$tail.quantile <- c(
  "Tp", "Tn", "pu", "yT=ln(ln(T/(T-1)))", "Allgemeine Extremwertverteilung LM",
  "Allgemeine Extremwertverteilung MM", "Allgemeine Extremwertverteilung ML"
  )

.GlobalEnv$tail.fitted.test <- rbind(
    rep("", 7),#just set a space line in the data set
    c("Anpassungstestgroessen", rep(NA, 6)),
    c("Anpassungstest", "Allgemeine Extremwertverteilung LM",
      "Allgemeine Extremwertverteilung MM", "Allgemeine Extremwertverteilung ML", NA, NA, NA),
    c("Kolmogrov-Smirnov-Test dn=", rep(NA, 6)),
    c("nw2-Test nw2=", rep(NA, 6)),
    c("Quantil-Korrelationstest r2=", rep(NA, 6)),
    c("AIC = -2 * ln(Likelihood)+2*(Anzahl Parameter) =", rep(NA, 6)),
    c("BIC = -2 * ln(Likelihood)+(Anzahl Parameter)*ln(Stichprobengroesse) =", rep(NA, 6))
  )


# Probabilities for determinations of quantiles
.GlobalEnv$probabilities <- c(0.047, 0.5, 0.8, 0.9, 0.95, 0.96, 0.98, 0.99, 0.995, 0.998, 
                              0.999, 0.9995, 0.9998, 0.9999)
# addToDataFrame refreshes all the created dataframe dataframe
addToDataFrame <- function(a = obj){
  # this if will move to global initialisation !
  if(.GlobalEnv$append.GEV == FALSE)
  initDatafrm()
  
  if(class(a@distr) == "GEVDistr"){
    parameters <- param(a)
    if(a@method == "distrFitLM"){
      index <- which(.GlobalEnv$GEVDistr$A == "Allgemeine Extremwertverteilung LM")
      .GlobalEnv$GEVDistr$B[(index + 4):(index + 3 +length(parameters))] <- 
                                        parameters[1:length(parameters)]
      print("I do it for distrFitMM")
    }#the "distrFitMM" method had choosed
    
    if(a@method == "distrFitMM"){
      index <- which(.GlobalEnv$GEVDistr$A == "Allgemeine Extremwertverteilung MM")
      .GlobalEnv$GEVDistr$B[(index + 4):(index + 3 + length(parameters))] <- 
                                        parameters[1:length(parameters)]
      print("I do it for distrFitLM")
    }#the "distrFitLM" method had choosed
    if(a@method == "distrFitML"){
      index <- which(.GlobalEnv$GEVDistr$A == "Allgemeine Extremwertverteilung ML")
      .GlobalEnv$GEVDistr$B[(index + 4):(index + 3 +length(parameters))] <- 
                                        parameters[1:length(parameters)]
      print("I do it for distrFitML")
    }#the "distrFitML" method had choosed
  if(.GlobalEnv$append.GEV == FALSE){
    meann <- mean(distr(a))
    sdd <- sd(distr(a))
    varr <- var(distr(a))
    var.coef <- sdd / meann
    schiefe <- skewness(distr(a))
    kurtosiss <- kurtosis(distr(a))
    lmm <- rawMoment(samp(a), 4)
    L1 <- lmm[1]
    L2 <- lmm[2]
    L3 <- lmm[3]
    L4 <- lmm[4]
    .GlobalEnv$GEVDistr <- setEvalStatAndLMoment(.GlobalEnv$GEVDistr,
                                                 meann = meann, sdd = sdd,
                                                 varr = varr, var.coef = var.coef, schiefe = schiefe,
                                                 kurtosiss = kurtosiss, L1 = L1, L2 = L2, L3 = L3, L4 = L4)
    }#add the statistical values
  if(.GlobalEnv$append.GEV == FALSE){
    pu <- 1:length(samp(a)) / (1 + length(samp(a)))
    t <- 1 / (1 - pu)
    yT <- log(log(t/(t-1)))
    HQ <- sort(samp(a))
  .GlobalEnv$GEVDistr <- setSampleValue(.GlobalEnv$GEVDistr,
                              m = 1:length(samp(a)), t = t,
                               pu = pu,
                              yT = yT, 
                               HQ = HQ)
  }#add the statical fitted values
  if(.GlobalEnv$append.GEV == FALSE)
  .GlobalEnv$GEVDistr <- setTailParamFitted(.GlobalEnv$GEVDistr, a = a)

  if(.GlobalEnv$append.GEV == FALSE){
    .GlobalEnv$GEVDistr <- setQuantileFrame(GEVDistr, names.qval = tail.quantile,
                                            probs = probabilities)
  }# estend the data frame to quantile tail
  
  # Let's yet compute and complete the quantile value geven object a.
  quant <- getQuantile(a = a, probs = probabilities)
  if(a@method == "distrFitLM"){
    index.q <- which(row.names(.GlobalEnv$GEVDistr) == "names.qval")
    .GlobalEnv$GEVDistr[(index.q + 1):(index.q + length(quant)), "E"] <- quant
  }#the "distrFitMM" method had been choosed
  
  if(a@method == "distrFitMM"){
    index.q <- which(row.names(.GlobalEnv$GEVDistr) == "names.qval")
    .GlobalEnv$GEVDistr[(index.q + 1):(index.q + length(quant)), "FF"] <- quant
  }#the "distrFitMM" method had been choosed
  if(a@method == "distrFitML"){
    index.q <- which(row.names(.GlobalEnv$GEVDistr) == "names.qval")
    .GlobalEnv$GEVDistr[(index.q + 1):(index.q + length(quant)), "G"] <- quant
  }#the "distrFitMM" method had been choosed
  
  .GlobalEnv$append.GEV = TRUE
  return(.GlobalEnv$GEVDistr)
  }# the GEV distribution had been cheked
  
  if(class(a@distr) == "WeibullDistr"){
    print("the Weibull distribution had been cheked")
  }# the Weibull distribution had been cheked
  
  if(class(a@distr) == "LogWeibullDist"){
    print("the LogWeibullDist distribution had been cheked")
  }# the LogWeibullDist distribution had been cheked
  
  if(class(a@distr) == "GumbelDistr"){
    print("the GumbelDistr distribution had been cheked")
  }# the GumbelDistr distribution had been cheked
  
  if(class(a@distr) == "ExponentialDistr"){
    print("the ExponentialDistr distribution had been cheked")
  }# the ExponentialDistr distribution had been cheked
  
  if(class(a@distr) == "Pear3Distr"){
    print("the Pear3Distr distribution had been cheked")
  }# the Pear3Distr distribution had been cheked
  
  if(class(a@distr) == "LogPear3Distr"){
    print("the LogPear3Distr distribution had been cheked")
  }# the LogPear3Distr distribution had been cheked
  
  if(class(a@distr) == "GammaDistr"){
    print("the GammaDistr distribution had been cheked")
  }# the GammaDistr distribution had been cheked
  
  if(class(a@distr) == "FrechetDistr"){
    print("the FrechetDistr distribution had been cheked")
  }# the FrechetDistr distribution had been cheked
  
  if(class(a@distr) == "NormalDistr"){
    print("the NormalDistr distribution had been cheked")
  }# the NormalDistr distribution had been cheked
  
  if(class(a@distr) == "LogNormal3PDistr"){
    print("the LogNormal3PDistr distribution had been cheked")
  }# the LogNormal3PDistr distribution had been cheked
  
  
}

#.GlobalEnv$GEVDistr <- addToDataFrame(a)
saveAsExcelFile <- function(datafrm = .GlobalEnv$GEVDistr,
                            filename = "ergeb.xlsx", appendd = FALSE){
  write.xlsx2(datafrm, file = filename, sheetName = class("GEVDistr"),
              col.names = FALSE, row.names = FALSE, append = appendd, showNA = FALSE)
  .GlobalEnv$appendd <- TRUE
  #indicates whether a neu file other just a sheet will be generated.
  #the variable .GlobalEnv$appandd is initialized in save-handler-section.
  #after that all dataframes have been saved, the appendd variable will been set to FALSE.
}
#saveAsExcelFile(a = a, filename = "ergeb.xlsx", appendd = FALSE)
#.GlobalEnv$appendd <- FALSE
