# compute the checked parameters
getCheckParam <- function(){
  schwellmeth <- if(svalue(.GlobalEnv$schwell.choose) == "min Jahres HQ") 
    "minjahr" else svalue(.GlobalEnv$schwell.MQ)
  if(svalue(.GlobalEnv$schwell.choose) == "2.5 MQ"){
    if(is.na(as.numeric(svalue(.GlobalEnv$schwell.MQ)))){
      print("error: falsche MQ Werte")
    }
    else {
      schwellmeth <- as.numeric(svalue(.GlobalEnv$schwell.MQ))
    }
  }
  print(schwellmeth)
  momentmeth <- if(svalue(.GlobalEnv$aev.choose) == "MM") distrFitMM 
  else{
    if(svalue(.GlobalEnv$aev.choose) == "LM") distrFitLM 
    else distrFitML
  }
  if(length(svalue(.GlobalEnv$HQ.choose)) == 0){
    print("error: Ergebnisse zeigen?")
  } else{
    HQ.index <- which(c("WHQ", "SHQ", "JHQ", "Misch. Vert") %in% svalue(.GlobalEnv$HQ.choose))
    #print(HQ.index)
  }
  return(list(HQ.index = HQ.index, momentmeth = momentmeth, schwellmeth = schwellmeth))
}
# to fit saisonal datas
#.GlobalEnv$hqdaten <- read.csv("C:/Users/Cesaire/Documents/Wise/boulot/HQs.csv")
saisonalFit <- function(daten = .GlobalEnv$saisonal.daten){
  einstellung <- getCheckParam()
  momentmeth <- einstellung$momentmeth
  schwellmeth <- einstellung$schwellmeth
  .GlobalEnv$HQ.index <- einstellung$HQ.index
  .GlobalEnv$whq <- daten[, "whq"]
  .GlobalEnv$shq <- daten[, "shq"]
  ng <- length(whq)
  .GlobalEnv$jhq <- sapply(1:ng, function(i) max(whq[i], shq[i]))
  t <- if(schwellmeth == "minjahr") min(jhq) else 2.5*schwellmeth
  .GlobalEnv$whq.enf <- whq[which(whq >= t)]
  .GlobalEnv$shq.enf <- shq[which(shq >= t)]
  nw <- length(whq.enf)
  ns <- length(shq.enf)
  Po.s <- (ng - ns) / ng
  Po.w <- (ng - nw) / ng
  HQ <- c("WHQ", "SHQ", "JHQ", "Misch.Vert")
  # fitte die saisonalen Daten
  for(i in HQ.index[which(HQ.index <= 3)]){
    .GlobalEnv$message.fit <- paste("Anpassung der", paste(HQ[i], "...", sep = ""), sep = "")
    Message(message = gettextRcmdr("Die Anpassung wird durchgefuerht...",
                                   type = "error"))
    .GlobalEnv$awhq <- momentmeth("GEVDistr", whq.enf)
    .GlobalEnv$ashq <- momentmeth("GEVDistr", shq.enf)
    .GlobalEnv$ajhq <- momentmeth("GEVDistr", .GlobalEnv$jhq)
    winter <- quantileGev(object = awhq, pu = .GlobalEnv$probabilities,
                                Po = Po.w)
    sommer <- quantileGev(object = ashq, pu = .GlobalEnv$probabilities,
                                Po = Po.s)
    jahres <- quantileGev(object = awhq, pu = .GlobalEnv$probabilities,
                                Po = 0)
    .GlobalEnv$param <- list(param.w = winter$param, param.s = sommer$param)
    # This will be revew!!!!!!!!!!!!!!!!!!!!!!!!!!!
    misch <- quantileMisch(pu = .GlobalEnv$probabilities, param.w = winter$param,
                           param.s = sommer$param, Po.w = Po.w, Po.s = Po.s)
    #HQS <- list(awhq = awhq, ashq = ashq, ajhq = ajhq)
    #misch <- NA
    HQs <- 0
    quantiles <- cbind(winter$quantile, sommer$quantile, jahres$quantile, misch)
    .GlobalEnv$Pos <- c(Po.w = Po.w, Po.s = Po.s, Po.j = 0)
  }
  obj.hq <- list(awhq, ashq, ajhq)
  print("Quantiles********************************")
  print(quantiles)

  return(list(quantiles = quantiles,
              Pos = .GlobalEnv$Pos,
              obj.hq = obj.hq,
              param.w = winter$param,
              param.s = sommer$param))
}

# Die AEV-Verteilungsfunktion
gevDensity <- function(x, distr.meth, saison = "W", Po.w = 0, Po.s = 0, ...){
  a <- distr.meth("GEVDistr", x, ...)
  if(saison == "W"){
    p <- Po.w + (1 - Po.w) * pdistr(a, x)
    return(list(p = p, a = a))
  } else{
    p <- Po.s + (1 - Po.s) * pdistr(a, x)
    return(list(p = p, a = a))
  }
}
# A test
#data(bakel_senegal)
#distr.meth = distrFitMM
#x<- bakel_senegal[1:5]
#gevdw <- gevDensity(x = x, distr.meth = distrFitMM, saison = "W", Po.w = 0.3)
#gevds <- gevDensity(x = x, distr.meth = distrFitMM, saison = "s", Po.2 = 0.2)
# Let's compute the quantiles

quantileGev <- function(object, pu, Po){
  u = object@distr@param[1]
  a = object@distr@param[2]
  k = object@distr@param[3]
  quan <- u + (a / k) * (1 - log((1 - Po) / (pu[pu > 1 | pu < 1] - Po))^k)
  param <- c(u, a, k)
  names(param) <- c("u", "a", "k")
  return(list(quantile = quan, param = param, object = object))
}
# Test of qgev
#qw <- quantileGev(object = gevdw$a, pu = gevdw$p, Po = 0.3)
#qs <- quantileGev(object = gevds$a, pu = gevds$p, Po = 0.2)

# puMisch rechnet die Mischverteilung
puMisch <- function(x, param.w, param.s, Po.w, Po.s, pu){
  uw <- param.w[1]
  aw <- param.w[2]
  kw <- param.w[3]
  g <- exp(-((1 - kw * ((x - uw) / aw))^(1 / kw)))
  us <- param.s[1]
  as <- param.s[2]
  ks <- param.s[3]
  f <- exp((-(1 - ks * ((x - us) / as))^(1 / ks)))
  pu <- (Po.w + (1 - Po.w) * g) * (Po.s + (1 - Po.s) * f)
  return(pu)
}
# Test puMisch
#puMisch(x = 400, param.w = qw$param, param.s = qs$param, Po.w = 0.3, Po.s = 0.2)

# quantileMisch rechnet die Quantiles der Mischverteilung
quantileMisch <- function(pu, param.w, param.s, ...){
  f <- function(x, pu, ...){
    return(puMisch(x, param.w, param.s, ...) - pu)
  }
  uw <- param.w[1]
  aw <- param.w[2]
  kw <- param.w[3]
  us <- param.s[1]
  as <- param.s[2]
  ks <- param.s[3]
  y <- .GlobalEnv$shq
  #prob.y <- sapply(y, function(i) puMisch(i, param.w, param.s,...))
  #plot(y,prob.y)
  unter <- if(kw < 0 & ks < 0) min(c(uw + (aw / kw), us + (as / ks))) else 0
  #upper <- if(kw > 0 & ks > 0) min(c(uw + (aw / kw), us + (as / ks))) else 1e20
  upper <- if(kw > 0 & ks > 0) min(c(uw + (aw / kw), us + (as / ks))) else max(.GlobalEnv$saisonal.daten)
  #print(unlist(list(upper = upper)))
  #qmisch <- sapply(pu, function(i){
  #  uniroot(f, pu = i, lower = unter, upper = us + (as / ks), ...)$root
  #})
  print("Grenze:")
  print(c(unter, upper))
  qmisch <- sapply(pu, function(i){
    #erg <- try(uniroot(f, pu = i, interval = c(unter, upper), ...)$root)##
    #if(inherits(erg, "try-error")) erg <- 1e20
    tryCatch({erg <- uniroot(f, pu = i, interval = c(unter, upper), ...)$root
              return(erg)},
             error = function(cond){
               return(NA)
               })
  })
  
  return(qmisch)
}

#quantileMisch(pu = 0.102328, param.w = qw$param, param.s = qs$param, Po.w = 0.3, Po.s = 0.2)

# Build the data frame for saisonal datas
buildSaisonalDataFrame <- function(a = .GlobalEnv$obj.whq, b = .GlobalEnv$obj.ashq, c = .GlobalEnv$obj.jhq){
  #file.name = "filename", sample.prop = setSamplePropEx(),
  #fitted.param = setFitTest(), estim.param = setEstimatedParamEx())
# Builde fuer die WHQs
.GlobalEnv$objects <- .GlobalEnv$obj.whq
display.whq <- buildDataFrameEx(file.name = "WHQ", sample.prop = setSamplePropEx(), 
                            fitted.param = setFitTest(),
                            estim.param = setEstimatedParamEx())
# Builde fuer die SHQs
.GlobalEnv$objects <- .GlobalEnv$obj.shq
display.shq <- buildDataFrameEx(file.name = "SHQ", sample.prop = setSamplePropEx(), 
                                fitted.param = setFitTest(),
                                estim.param = setEstimatedParamEx())
# Builde fuer die JHQs
.GlobalEnv$objects <- .GlobalEnv$obj.jhq
display.jhq <- buildDataFrameEx(file.name = "WHQ", sample.prop = setSamplePropEx(), 
                                fitted.param = setFitTest(),
                                estim.param = setEstimatedParamEx())
return(list(display.whq = display.whq, display.shq = display.shq, display.jhq = display.jhq))
}