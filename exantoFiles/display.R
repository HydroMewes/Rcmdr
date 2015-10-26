# Data frame to display

.GlobalEnv$sample.prop <- c("Eigenschaften Stichprobe",
                            "Mittelwert",
                            "Standardabweichung:",
                            "Varianz:",
                            "Variantionskoeffizient:",
                            "Schiefe:",
                            "Kurtosis:", "")
.GlobalEnv$fit.test <- c("Anpassungstest","KS-Test dn =", "nw2-Test nw2 = ",
                         "Quantil-Korrel.Test r2",
                         "AIC", "BIC", "")
.GlobalEnv$fitted.param <- c("Geschaetzte Parameter", "Lageparameter",
                             "Streuungsparameter", "Formparameter")
.GlobalEnv$sample.name <- c("Auswertung Exanto", "Stichprobe", "")

computeSampleProp <- function(a = obj){
  .GlobalEnv$meann <- mean(distr(a))
  .GlobalEnv$sdd <- sd(distr(a))
  .GlobalEnv$varr <- var(distr(a))
  .GlobalEnv$var.coef <- sdd / meann
  .GlobalEnv$schiefe <- skewness(distr(a))
  .GlobalEnv$kurtosiss <- kurtosis(distr(a))
  param.names <- c("mean", "sd", "var", "var.coef", "schiefe", "kurtosis")
  param.values <- c(.GlobalEnv$meann, .GlobalEnv$sdd, .GlobalEnv$varr,
                    .GlobalEnv$var.coef, .GlobalEnv$schiefe, .GlobalEnv$kurtosiss)
  names(param.values) <- param.names
  return(round(param.values, 2))
}
setSampleProp <- function(a = .GlobalEnv$objects[1][[1]]){
  .GlobalEnv$sample.prop <- c("Eigenschaften Stichprobe",
                              "Mittelwert",
                              "Standardabweichung:",
                              "Varianz:",
                              "Variantionskoeffizient:",
                              "Schiefe:",
                              "Kurtosis:", "")
  prop.value <- computeSampleProp(a)[c("mean", "sd", "var", "var.coef", "schiefe", "kurtosis")]
  .GlobalEnv$sample.prop <- cbind(.GlobalEnv$sample.prop, c("", prop.value))
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
  for(i in 1:length(index)){
    .GlobalEnv$fit.test <- cbind(.GlobalEnv$fit.test, rep("", n),
                                 fitTest(.GlobalEnv$objects[i][[1]], nmeth = .GlobalEnv$nmethod[index[i]]))
  }
  return(.GlobalEnv$fit.test)
}
setEstimatedParam <- function(index = .GlobalEnv$check.index){
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

# buildDataFrame joind all part of requiered data frame to make the final data frame that will be showed.
buildDataFrame <- function(file.name = "filename", sample.prop = setSampleProp(),
                           fitted.param = setFitTest(), estim.param = setEstimatedParam()){
  n.col <- ncol(estim.param)
  disp.head <- matrix(rep("", 2 * n.col), nrow = 2)
  disp.head[1, 1] <- "Auswertung Exanto"
  disp.head[2, 1] <- "Stichprobe"
  disp.head[2, 2] <- file.name
  to.complete <- (n.col - ncol(sample.prop)) * nrow(sample.prop)
  sample.prop <- cbind(sample.prop, matrix(rep("", to.complete), nrow = nrow(sample.prop)))
  to.display <- rbind(disp.head, sample.prop, fitted.param, estim.param)
  colnames(to.display) <- NULL
  return(to.display)
}
# builDisplay build the data frame that will be display. It just remove the sample portion
# from the Excel data frame
builDisplay <- function(dfrm){
  i <- which(dfrm[,1] == "Stichprobenwerte")
  index.rm <- i + 1:(1 + length(samp(.GlobalEnv$objects[[1]])))
  display.dfrm <- dfrm[-index.rm,]
  return(display.dfrm)
}