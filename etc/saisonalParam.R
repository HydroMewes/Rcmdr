
computeSamplePropSaisonal <- function(winter = .GlobalEnv$awhq,
                                      sommer = .GlobalEnv$ashq,
                                      jahr = .GlobalEnv$ajhq){
  prop.winter <- computeSamplePropEx(winter)
  prop.sommer <- computeSamplePropEx(sommer)
  prop.jahr <- computeSamplePropEx(jahr)
  all.param <- cbind(prop.winter, prop.sommer, prop.jahr)
  all.param1 <- all.param[1:6, ]
  all.param1 <- rbind(c("winter", "sommer", "jahr"), all.param1)
  all.param2 <- all.param[7:nrow(all.param), ]
  all.param <- rbind(all.param1, 
                     "","",
                     all.param2,
                     "")
  #rownames(all.param)[which(rownames(all.param) == "")] <- "Wahrscheinlichkeitsgewichtete L-Momente"
  names.param <- c("Eigenschaften Stichprobe",
                  "Mittelwert",
                  "Standardabweichung:",
                  "Varianz:",
                  "Variationskoeffizient:",
                  "Schiefe:",
                  "Kurtosis:", "",
                  "L-Momente",
                  "L1:",
                  "L2:",
                  "L3:",
                  "L4:",
                  "")
  all.param <- cbind(names.param, all.param)
  return(all.param)
}
# Test the function computeSamplePropSaisonal

#.GlobalEnv$awhq <- distrFitMM("GEVDistr", whq.enf)
#.GlobalEnv$ashq <- distrFitMM("GEVDistr", shq.enf)
#.GlobalEnv$ajhq <- distrFitMM("GEVDistr", jhq)

#computeSamplePropSaisonal()

saisonalFitTest <- function(winter = .GlobalEnv$awhq,
                            sommer = .GlobalEnv$ashq,
                            jahr = .GlobalEnv$ajhq){
  fit.winter <- fitTest(winter)[2:7]
  fit.sommer <- fitTest(sommer)[2:7]
  fit.jahr <- fitTest(jahr)[2:7]
  all.fit <- cbind(fit.winter, fit.sommer, fit.jahr)
  fit.test.meth <- c("KS-Test dn =", "nw2-Test nw2 = ",
                           "Quantil-Korrel.Test r2",
                           "AIC", "BIC", "")
  all.fit <- cbind(fit.test.meth, fit.winter, fit.sommer, fit.jahr)
  colnames(all.fit) <- NULL
  return(all.fit)
}
# Test of the fucntion saisonalFitTest
#saisonalFitTest()

saisonalParam <- function(winter = .GlobalEnv$awhq,
                          sommer = .GlobalEnv$ashq,
                          jahr = .GlobalEnv$ajhq){
  parameter <- round(cbind(param(winter), param(sommer), param(jahr)), 2)
  param.desc <- c("Geschaetzte Parameter", "Lageparameter (u)",
                  "Streuungsparameter (a)", "Formparameter (k)")
  param.name <- names(param(winter))
  saisonal.param <- cbind(param.desc,  rbind(rep("", length(parameter)),parameter))
  row.names(saisonal.param) <- NULL
  colnames(saisonal.param) <- NULL
  return(saisonal.param)
}
# Test of the function saisonalPram
#saisonalParam()

saisonalSampleValues <- function(winter = .GlobalEnv$awhq,
                                 sommer = .GlobalEnv$ashq,
                                 jahr = .GlobalEnv$ajhq){
  name.dfrm <- c("m", "T", "pu", "yT=ln(ln(T/(T-1)))", "Winter-HQ", "Sommer-HQ", "Jahr-HQ")
  m <- 1:length(samp(winter))
  pu <- round(1:length(samp(winter)) / (1 + length(samp(winter))), 4)
  t <- round(1 / (1 - pu), 4)
  yT <- round(- log(log(t/(t-1))), 4)
  HQ <- cbind(sort(samp(winter)), sort(samp(sommer)), sort(samp(jahr)))
  dat.frm <- cbind(m, t, pu, yT, HQ)
  dat.frm <- rbind(name.dfrm, dat.frm)
  colnames(dat.frm) <- NULL
  row.names(dat.frm) <- NULL
  dat.frm <- join("Stichprobenwerte", dat.frm)
  dat.frm <- rbind(NA, dat.frm)
  return(dat.frm)
}
# Test of the function saisonalSampleValues
#saisonalSampleValues()

saisonalQuantile <- function(all.quantile = .GlobalEnv$saisonal.fit$quantiles,
                             probs = probabilities){
  all.quantile <- round(all.quantile, 2)
  name <- c(c("T partiell", "Tn", "pu", "yT=ln(ln(T/(T-1)))"), "Winter", "Sommer", "Jahr", "Mischverteilung")
  Tn <- round(1 / (1 - probs), 2)
  Tp <-round( 1 / log(Tn / (Tn - 1)), 2)
  yT <- round(log(log(Tn / (Tn - 1))), 2)
  values <- cbind(Tp, Tn, probs, yT)
  q.dfrm <- cbind(values, all.quantile)
  q.dfrm <- rbind(name, q.dfrm)
  names(q.dfrm) <- NULL
  row.names(q.dfrm) <- NULL
  return(join("",q.dfrm))
}

# buildDataFrame joind all part of requiered data frame to make the final data frame that will be showed.
buildSaisonalDataFrameEx <- function(file.name = "filename", sample.prop = computeSamplePropSaisonal(),
                             fitted.param = saisonalFitTest(), estim.param = saisonalParam()){
  n.col <- ncol(estim.param)
  disp.head <- matrix(rep("", 2 * n.col), nrow = 2)
  disp.head[1, 1] <- "Auswertung Exanto"
  disp.head[2, 1] <- "Stichprobe"
  disp.head[2, 2] <- file.name
  to.complete <- (n.col - ncol(sample.prop)) * nrow(sample.prop)
  sample.prop <- cbind(sample.prop, matrix(rep("", to.complete), nrow = nrow(sample.prop)))
  to.display <- rbind(disp.head, join(sample.prop, fitted.param), estim.param)
  dfrm.samp.val <- saisonalSampleValues()
  to.save <- join(to.display, dfrm.samp.val)
  to.display <- join(to.display, saisonalQuantile())
  to.save <- join(to.save, saisonalQuantile())
  colnames(to.display) <- NULL
  return(list(to.display = to.display, to.save = to.save))
}

# buildSaisonalDataFrameSave buid the data frame that will be saved.
buildSaisonalDataFrameSave <- function(to.display){
  n.col <- ncol(estim.param)
  disp.head <- matrix(rep("", 2 * n.col), nrow = 2)
  disp.head[1, 1] <- "Auswertung Exanto"
  disp.head[2, 1] <- "Stichprobe"
  disp.head[2, 2] <- file.name
  to.complete <- (n.col - ncol(sample.prop)) * nrow(sample.prop)
  sample.prop <- cbind(sample.prop, matrix(rep("", to.complete), nrow = nrow(sample.prop)))
  to.display <- rbind(disp.head, join(sample.prop, fitted.param), estim.param)
  dfrm.samp.val <- saisonalSampleValues()
  to.save <- join(to.display, dfrm.samp.val)
  to.display <- join(to.display, saisonalQuantile())
  to.save <- join(to.save, saisonalQuantile())
  colnames(to.display) <- NULL
  return(list(to.display = to.display, to.save = to.save))
}
