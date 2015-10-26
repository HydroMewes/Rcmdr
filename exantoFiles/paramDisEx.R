# Data frame to display
.GlobalEnv$nparam <- c("u = ", "a = ", "k = ",# begin of GEV
                       "u = ", "a = ", "k = ",
                       "u = ", "a = ", "k = ",# end of GEV
                       "m = ", "a = ", "b = ",# weibull
                       "m = ", "a = ", "b = ",
                       "m = ", "a = ", "b = ",# end of log-weibull distribution
                       "m = ", "a = ", "b = ",#log-weibull
                       "m = ", "a = ", "b = ",
                       "m = ", "a = ", "b = ",# end of log-weibull distribution
                       "u = ", "a = ", "",   # begin of Gumbel
                       "u = ", "a = ", "",
                       "u = ", "a = ", "",   # end of Gumbel
                       "u = ", "a = ", "",   # Exponential
                       "u = ", "a = ", "",
                       "u = ", "a = ", "",   # end of Exponential
                       "u = ", "a = ", "b = ",#Pearson III
                       "u = ", "a = ", "b = ",
                       "u = ", "a = ", "b = ",#End of Pearson III
                       "u = ", "a = ", "b = ",#Log-Pearson III
                       "u = ", "a = ", "b = ",
                       "u = ", "a = ", "b = ",#End of LOg-Pearson III
                       "u = 0", "a = ", "b = ",#Gamma
                       "u = 0", "a = ", "b = ",
                       "u = 0", "a = ", "b = ",#End of Gamma
                       "u = ", "a = ", "k = ",# Frechet
                       "u = ", "a = ", "k = ",
                       "u = ", "a = ", "k = ",# end of Frechet
                       "", "m = ", "s = ",    # normal distribution
                       "", "m = ", "s = ",
                       "", "m = ", "s = ",    #end of normal distribution
                       "", "m = ", "s = ",    # log-normal distribution
                       "", "m = ", "s = ",
                       "", "m = ", "s = ",   #end of log-normal distribution
                       "a = ", "m = ", "s = ",#  3-Param LOg-Norm
                       "a = ", "m = ", "s = ",
                       "a = ", "m = ", "s = ")
.GlobalEnv$sample.prop <- c("Eigenschaften Stichprobe",
                            "Mittelwert",
                            "Standardabweichung:",
                            "Varianz:",
                            "Variationskoeffizient:",
                            "Schiefe:",
                            "Kurtosis:", "")
.GlobalEnv$fit.test <- c("Anpassungstest","KS-Test dn =", "nw2-Test nw2 = ",
                         "Quantil-Korrel.Test r2",
                         "AIC", "BIC", "")
.GlobalEnv$fitted.param <- c("Geschaetzte Parameter", "Lageparameter",
                             "Streuungsparameter", "Formparameter")
.GlobalEnv$sample.name <- c("Auswertung Exanto", "Stichprobe", "")
# Reviwe and updateMe
#.GlobalEnv$dfrm.display <- c(.GlobalEnv$sample.prop, .GlobalEnv$fit.test, .GlobalEnv$fitted.param)

computeSamplePropEx <- function(a = obj){
    samplee <- samp(a)
  .GlobalEnv$meann <- mean(distr(a))
  .GlobalEnv$sdd <- sd(distr(a))
  .GlobalEnv$varr <- var(distr(a))
  .GlobalEnv$var.coef <- sdd / meann
  .GlobalEnv$schiefe <- skewness(distr(a))
  #if(class(a@distr) %in% c("LogWeibullDistr", "LogPear3Distr"))
  #.GlobalEnv$kurtosiss <-  
  #.GlobalEnv$kurtosiss <- kurtosis(distr(a))
  #b <- (1 / length(samplee)) * sum((samplee - mean(samplee))^4) / 
   # (1 / length(samplee)) * sum((samplee - mean(samplee))^2)^2
  b <- (1 / length(samplee)) * sum(((samplee - mean(samplee)) / sd(samplee))^4)
  .GlobalEnv$kurtosiss <- unlist(list(b = b))
  if(class(a@distr) != "LogNormal3PDistr")
  lmm <- rawMoment(samp(a), 4)
  else{
  lmm  <- lmrln3(para = a@distr@param, nmom = 4)
  }
  print("Hier die L-Momente:")
  print(lmm)
  L1 <- lmm[1]
  L2 <- lmm[2]
  L3 <- lmm[3]
  L4 <- lmm[4]
  param.names <- c("mean", "sd", "var", "var.coef", "schiefe", "kurtosis", "L1",
                   "L2", "L3", "L4")
  param.values <- c(.GlobalEnv$meann, .GlobalEnv$sdd, .GlobalEnv$varr,
                    .GlobalEnv$var.coef, .GlobalEnv$schiefe, .GlobalEnv$kurtosiss, 
                    lmm[1:4])
  names(param.values) <- param.names
  return(round(param.values, 2))
}
setSamplePropEx <- function(a = .GlobalEnv$objects[1][[1]]){
  .GlobalEnv$sample.prop.ex <- c("Eigenschaften Stichprobe",
                              "Mittelwert",
                              "Standardabweichung:",
                              "Varianz:",
                              "Variantionskoeffizient:",
                              "Schiefe:",
                              "Kurtosis:", "",
                              "L-Momente",
                              "L1:",
                              "L2:",
                              "L3:",
                              "L4:",
                              "")
  prop.value <- computeSamplePropEx(a)
  st <- prop.value[c("mean", "sd", "var", "var.coef", "schiefe", "kurtosis")]
  lm <- prop.value[c("L1", "L2", "L3", "L4")]
  .GlobalEnv$sample.prop.ex <- cbind(.GlobalEnv$sample.prop.ex, c("", st, "", "", lm))
  # spaces according to a part for statistics and a part for L-moments
}


fitTest <- function(a = obj, nmeth = "fit.method"){
  dn <- round(bootstrapGOFtest(a, testfun = "KS")$testvalue, 3)#for Kolmogorov Smirnov
  nw <- round(bootstrapGOFtest(a, testfun = "nw2")$testvalue, 3)#for Cramer-von-Mises test
  qcor <- round(qqcor(a), 3)
  aic <- round(AIC(a), 3)
  bic <- round(BIC(a), 3)
  return(c(nmeth, dn, nw, qcor, aic, bic, ""))
}
setFitTest <- function(index = .GlobalEnv$check.index){
  .GlobalEnv$fit.test <- c("Anpassungstest","KS-Test dn =", "nw2-Test nw2 = ",
                           "Quantil-Korrel.Test r2",
                           "AIC", "BIC", "")
  n <- length(.GlobalEnv$fit.test)
  .GlobalEnv$message.fit <- "Beginn der Anpassung..."
  doItAndPrint("print(message.fit)")
  for(i in 1:length(index)){
    .GlobalEnv$message.fit <- paste("Anpassung mit ", paste(nmethod[index[i]], "...", sep = ""), sep = "")
    doItAndPrint("print(message.fit)")
    
   # .GlobalEnv$fit.test <- cbind(.GlobalEnv$fit.test, rep("", n),
    #                             fitTest(.GlobalEnv$objects[i][[1]], nmeth = .GlobalEnv$nmethod[index[i]]))
    
    tryCatch({
      .GlobalEnv$fit.test <- cbind(.GlobalEnv$fit.test, rep("", n),
                                   fitTest(.GlobalEnv$objects[i][[1]], nmeth = .GlobalEnv$nmethod[index[i]]))
    },
    error = function(cond){
      .GlobalEnv$warnung <- "Die LM-Momente fuer log-3-P-Normalverteil. ist nicht klomplet behandelt..."
      doItAndPrint("print(warnung)")
    })
  }
  .GlobalEnv$message.fit <- "Ende der Anpassung..."
  doItAndPrint("print(message.fit)")
  return(.GlobalEnv$fit.test)
}
setEstimatedParamEx <- function(index = .GlobalEnv$check.index){
  .GlobalEnv$fitted.param <- c("Geschaetzte Parameter", "Lageparameter",
                               "Streuungsparameter", "Formparameter")
  for(i in 1:length(index)){
    p.name <- names(.GlobalEnv$objects[i][[1]]@distr@param)
    par.name <- rep("", 4)
    par.name[2: (1 + length(p.name))] <- p.name
    par.var <- c(.GlobalEnv$nmethod[index[i]], round(.GlobalEnv$objects[i][[1]]@distr@param, 3))
    .GlobalEnv$fitted.param <- cbind(.GlobalEnv$fitted.param, par.name, par.var)
  }
  row.names(.GlobalEnv$fitted.param) <- NULL
  return(.GlobalEnv$fitted.param)
}

# the function join is use to join two object as rbind, but with different other equal
# row length.
join <- function(obj1, obj2){
  obj1 <- as.matrix(obj1)
  obj2 <- as.matrix(obj2)
  matLis <- list(obj1, obj2)
  n <- max(sapply(matLis, ncol))
  do.call(rbind, lapply(matLis, function (x) 
    cbind(x, matrix(, nrow(x), n-ncol(x))))) 
}


sampleValues <- function(a = object){
  name.dfrm <- c("m", "T", "pu", "yT=ln(ln(T/(T-1)))", "HQ")
  m <- 1:length(samp(a))
  pu <- round(1:length(samp(a)) / (1 + length(samp(a))), 4)
  t <- round(1 / (1 - pu), 4)
  yT <- round(- log(log(t/(t-1))), 4)
  HQ <- sort(samp(a))
  dat.frm <- cbind(m, t, pu, yT, HQ)
  dat.frm <- rbind(name.dfrm, dat.frm)
  colnames(dat.frm) <- NULL
  row.names(dat.frm) <- NULL
  dat.frm <- join("Stichprobenwerte", dat.frm)
  dat.frm <- rbind(NA, dat.frm)
  return(dat.frm)
}

# Probabilities for determinations of quantiles
.GlobalEnv$probabilities <- c(0.047, 0.5, 0.8, 0.9, 0.95, 0.96, 0.98, 0.99, 0.995, 0.998, 
                              0.999, 0.9995, 0.9998, 0.9999)
# The part of quantiles
quantileValues <- function(objects = .GlobalEnv$objects, probs = probabilities){
  
  getQuantile <- function(a = obj, probs = probabilities){
    sapply(probs, function(p){
      round(qdistr(a, p), 2)
    })
  }
  name <- c(c("T partiell", "Tn", "pu", "yT=ln(ln(T/(T-1)))"), .GlobalEnv$nmethod[.GlobalEnv$check.index])
  Tn <- round(1 / (1 - probs), 2)
  Tp <-round( 1 / log(Tn / (Tn - 1)), 4)
  yT <- round(log(log(Tn / (Tn - 1))), 2)
  quant <- sapply(1:length(objects), function(i){
    a = .GlobalEnv$objects[i][[1]]
    getQuantile(a = a, probs = probabilities)
  })
  values <- cbind(Tp, Tn, probs, yT)
  q.dfrm <- cbind(values, quant)
  q.dfrm <- rbind(name, q.dfrm)
  names(q.dfrm) <- NULL
  row.names(q.dfrm) <- NULL
  return(join("",q.dfrm))
}


# buildDataFrame joind all part of requiered data frame to make the final data frame that will be showed.
buildDataFrameEx <- function(file.name = ActiveDataSet(), sample.prop = setSamplePropEx(),
                           fitted.param = setFitTest(), estim.param = setEstimatedParamEx()){
  n.col <- ncol(estim.param)
  disp.head <- matrix(rep("", 2 * n.col), nrow = 2)
  disp.head[1, 1] <- "Auswertung Exanto"
  disp.head[2, 1] <- "Stichprobe"
  disp.head[2, 2] <- file.name
  to.complete <- (n.col - ncol(sample.prop)) * nrow(sample.prop)
  sample.prop <- cbind(sample.prop, matrix(rep("", to.complete), nrow = nrow(sample.prop)))
  print(ncol(disp.head))
  print(ncol(sample.prop))
  print(ncol(fitted.param))
  print(ncol(estim.param))
  #to.display <- rbind(disp.head, sample.prop, fitted.param, estim.param)
  to.display <- rbind(disp.head, join(sample.prop, fitted.param), estim.param)
  # let's add the sample values in the data frame
  dfrm.samp.val <- sampleValues(a = .GlobalEnv$objects[1][[1]])
  to.display <- join(to.display, dfrm.samp.val)
  #to.display <- join("", to.display)
  to.display <- join(to.display, quantileValues(objects = .GlobalEnv$objects,
                                                probs = probabilities))
  colnames(to.display) <- NULL
  return(to.display)
}

saveAsExcelFile <- function(datafrm = buildDataFrameEx(),
                            sheetName = names(.GlobalEnv$activeDataSet)[1],
                            file = ActiveDataSet(), appendd = FALSE){
  write.xlsx2(datafrm, file = file, sheetName = names(.GlobalEnv$activeDataSet)[1],
              col.names = FALSE, row.names = FALSE, append = appendd, showNA = FALSE)
  .GlobalEnv$appendd <- TRUE
}
