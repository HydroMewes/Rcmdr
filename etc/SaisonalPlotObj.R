plotItSaisonal <- function(vect.obj = .GlobalEnv$saisonal.fit$obj.hq, x.low = .GlobalEnv$xlow,
                   x.up = .GlobalEnv$xup, y.low = .GlobalEnv$ylow, Pos = .GlobalEnv$Pos,
                   y.up = .GlobalEnv$yup, index = .GlobalEnv$HQ.index[which(.GlobalEnv$HQ.index <= 3)]){
  if(!.GlobalEnv$saiso.ana){
    plot.new(); grid()
  return(TRUE)
  }
  .GlobalEnv$colsaison <- c("red", "blue", "green", "black")
  pu <- round(1:length(samp(vect.obj[[1]])) / (1 + length(samp(vect.obj[[1]]))), 2)
  t <- round(1 / (1 - pu), 2)
  # try to set the xlim and ylim value
  #print("voici les index")
  #print(index)
  x.low <- if(svalue(x.low) == "") -2 else svalue(x.low)
  x.up <- if(svalue(x.up) == "") 7 else svalue(x.up)
  y.low <- if(svalue(y.low) == "") 0 else svalue(y.low)
  y.up <- if(svalue(y.up) == "") max(.GlobalEnv$saisonal.daten) else svalue(y.up)
  par(oma = c(8, 1, 0, 0))
  x.bool <- if(!is.na(as.numeric(x.low)) & !is.na(as.numeric(x.up))) TRUE else FALSE
  y.bool <- if(!is.na(as.numeric(y.low)) & !is.na(as.numeric(y.up))) TRUE else FALSE
  to.add <- FALSE
  print(length(index))
  for(i in 1:length(index)){
    a <- vect.obj[[index[i]]]
    if(x.bool & !y.bool){
      mydistrPlotSaisonal(a, Po = Pos[i], xlab = "", ylab = "HQ [m^3/s]", xlim = c(as.numeric(x.low), as.numeric(x.up)),
                  xaxt = "n", axes = FALSE, add = to.add, trans = TRUE, col = .GlobalEnv$colsaison[index[i]])
    } else{
      if(!x.bool & y.bool){
        mydistrPlotSaisonal(a, Po = Pos[i], xlab = "", ylab = "HQ [m^3/s]", ylim = c(as.numeric(y.low), as.numeric(y.up)), xlim = c(-2, 7),
                    xaxt = "n", axes = FALSE, add = to.add, trans = TRUE, col = .GlobalEnv$colsaison[index[i]])
      } else{
        ####
        if(x.bool & y.bool){
          mydistrPlotSaisonal(a, Po = Pos[i], xlab = "", ylab = "HQ [m^3/s]", xlim = c(as.numeric(x.low), as.numeric(x.up)),
                      ylim = c(as.numeric(y.low), as.numeric(y.up)),
                      xaxt = "n", axes = FALSE, add = to.add, trans = TRUE,
                      col = .GlobalEnv$colsaison[index[i]])
        } else{
          mydistrPlotSaisonal(a, Po = Pos[i], xlab = "", ylab = "HQ [m^3/s]", xlim = c(-2, 7),
                      xaxt = "n", axes = FALSE, add = FALSE, trans = TRUE,
                      col = .GlobalEnv$colsaison[index[i]])
          #lines(-2:7, seq(0,1500,by= 15001 / 10)[1:length(-2:7)])
          
          
        }
        ####
      }
    }
    if(svalue(.GlobalEnv$gitter.choose) == "Ja"){
      grid()
    }
    to.add <- TRUE
  }
  # Plotte die Mischverteilung
  if(length(which(.GlobalEnv$HQ.index == 4) != 0)){
    mydistrPlotSaisonal(object = NULL, misch = TRUE, Po = Pos[i], xlab = "", ylab = "HQ [m^3/s]", xlim = c(-2, 7),
                        xaxt = "n", axes = FALSE, add = to.add, trans = TRUE,
                        col = .GlobalEnv$colsaison[4])
  }
  x.low <- if(!is.na(as.numeric(x.low))) as.numeric(x.low) else -2
  x.up <- if(!is.na(as.numeric(x.up))) as.numeric(x.up) else 7
  axis(side = 2, lwd = 0.5)
  printXAxis(xlabe = .GlobalEnv$xlabel, xlow = as.numeric(x.low))
  legend(-2, max(saisonal.daten, na.rm = TRUE), col=c("white", .GlobalEnv$colsaison[.GlobalEnv$HQ.index]), bty = "o", bg = NA,
         legend=c(.GlobalEnv$legend.title,
                  c("WHQ", "SHQ", "JHQ", "Misch-HQ")[.GlobalEnv$HQ.index]),
         inset=c(-0.5, 0), cex=0.8, lty=rep(1, length(.GlobalEnv$HQ.index)),
         box.col=NA)
  title(main = svalue(.GlobalEnv$titlegrf))
  box()
  invisible(TRUE)
}

#---------- mydistrPlotSaisonal ----------
mydistrPlotSaisonal <- function(object, misch = FALSE, Po = 0, a = 0,
                                trans = FALSE, transfun = function(x){
  -log((log(1 / x)))
}, ylim = NULL, xlim = NULL, add = FALSE, ...){
if(!is.null(object)){
  if (!add) {
    x <- sort(samp(.GlobalEnv$ajhq))
    empProb <- ppoints(x, a = a)
    #distr = distr(object)
  }
  else {
    if (inherits(object, "DistrFit")) {
      #distr = distr(object)
    }
  }
  prob <- seq(1e-05, 0.999999, by = 1e-05)
  #xdistr <- qdistr(distr, prob)
  xdistr <- quantileGev(object = object, Po = Po, pu = prob)$quantile
  if (trans == TRUE) {
    if (!add) {
      empProb <- transfun(empProb)
    }
    prob <- transfun(prob)
  }
  if (!add && is.null(ylim)) {
    ylim = range(x, xdistr)
  }
  if (!add && is.null(xlim)) {
    if (trans == FALSE) {
      xlim = c(0, 1)
    }
    else {
      xlim = range(empProb, prob)
    }
  }
  if (!add) {
    plot(prob, xdistr, type = "l", xlim = xlim, ylim = ylim, 
         ...)
    #lines(prob, xdistr , type = "l",...)
    points(empProb, x)
  }
  else {
    lines(prob, xdistr, ...)
  }
}
  if(misch){
    prob <- seq(1e-05, 0.999999, by = 1e-05)
    param.w = .GlobalEnv$param$param.w
    param.s = .GlobalEnv$param$param.s
    #.GlobalEnv$message.fit <- "Geduld bitte..."
    #doItAndPrint("print(message.fit)")
    if(!.GlobalEnv$Misch.fitted.bool){
      print("voila ca")
      print(.GlobalEnv$Misch.fitted.bool)
      # Die gemischt quantile wurden noch nicht berechnet
    xdistr <- quantileMisch(pu = prob, param.w = param.w,
                  param.s = param.s, Po.w = .GlobalEnv$Pos[1],
                  Po.s = .GlobalEnv$Pos[2])
    .GlobalEnv$misch.fitted <- xdistr
    .GlobalEnv$Misch.fitted.bool <- TRUE
    }else{
      #Die gemischt quantile wurden schon berechnet
      xdistr <- .GlobalEnv$misch.fitted
    }
    if (!add) {
      plot(transfun(prob), xdistr, type = "l", xlim = xlim, ylim = ylim, 
           ...)
    }
    else{
      lines(transfun(prob), xdistr, ...)
    }
  }
}
