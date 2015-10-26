# PlotIt uses the vector of fitted objects to make a plot.
# Note that when the xlim and/ylim are Inf, the function just uses
# those that it computes from vector of objects.
plotIt <- function(vect.obj = .GlobalEnv$objects, x.low = .GlobalEnv$xlow,
                   x.up = .GlobalEnv$xup, y.low = .GlobalEnv$ylow,
                   y.up = .GlobalEnv$yup){
  pu <- round(1:length(samp(vect.obj[[1]])) / (1 + length(samp(vect.obj[[1]]))), 2)
  t <- round(1 / (1 - pu), 2)
  # try to set the xlim and ylim value
  x.low <- svalue(x.low)
  x.up <- svalue(x.up)
  y.low <- svalue(y.low)
  y.up <- svalue(y.up)
  par(oma = c(8, 1, 0, 0))
  x.bool <- if(!is.na(as.numeric(x.low)) & !is.na(as.numeric(x.up))) TRUE else FALSE
  y.bool <- if(!is.na(as.numeric(y.low)) & !is.na(as.numeric(y.up))) TRUE else FALSE
   to.add <- FALSE
    for(i in 1:length(.GlobalEnv$check.index)){
      a <- vect.obj[[i]]
     if(x.bool & !y.bool){
     mydistrPlot(a, xlab = "", ylab = "HQ [m^3/s]", xlim = c(as.numeric(x.low), as.numeric(x.up)),
               xaxt = "n", axes = FALSE, add = to.add, trans = TRUE, col = .GlobalEnv$colors[i])
     } else{
       if(!x.bool & y.bool){
         mydistrPlot(a, xlab = "", ylab = "HQ [m^3/s]", ylim = c(as.numeric(y.low), as.numeric(y.up)), xlim = c(-2, 7),
                      xaxt = "n", axes = FALSE, add = to.add, trans = TRUE, col = .GlobalEnv$colors[i])
       } else{
         ####
         if(x.bool & y.bool){
           mydistrPlot(a, xlab = "", ylab = "HQ [m^3/s]", xlim = c(as.numeric(x.low), as.numeric(x.up)),
                     ylim = c(as.numeric(y.low), as.numeric(y.up)),
                              xaxt = "n", axes = FALSE, add = to.add, trans = TRUE,
                     col = .GlobalEnv$colors[i])
         } else{
           mydistrPlot(a, xlab = "", ylab = "HQ [m^3/s]", xlim = c(-2, 7),
                     xaxt = "n", axes = FALSE, add = to.add, trans = TRUE,
                     col = .GlobalEnv$colors[i])
         }
         ####
       }
     }
      to.add <- TRUE
    }
 x.low <- if(!is.na(as.numeric(x.low))) as.numeric(x.low) else -2
 x.up <- if(!is.na(as.numeric(x.up))) as.numeric(x.up) else 7
   axis(side = 2, lwd = 0.5)
    printXAxis(xlabe = .GlobalEnv$xlabel, xlow = as.numeric(x.low))
    legend(-2, y.up, col=colors,
           legend=.GlobalEnv$nmethod[.GlobalEnv$check.index], bty = "o", bg = NA,
           inset=c(-0.5, 0), cex=0.8, lty=rep(1, length(.GlobalEnv$check.index)),
           box.col=NA)
  title(main = svalue(.GlobalEnv$titlegrf))
  box()
invisible(TRUE)
}

# printXAxis prints the x-axis
printXAxis <- function(xlabe = svalue(.GlobalEnv$xlabel), xlow = -1.5){
  xlabe = svalue(.GlobalEnv$xlabel)
  y <- seq(-2, 7, 1)
  pu <- round(1 / exp(exp(-y)), 3)
  t <- round(1/(1 - pu), 1)
  if(xlabe == "alle"){
  axis(side = 1, -2:7, line = 2, col = "black", type = "l", lwd = 2.5, cex.axis = 0.7)
  mtext("y = -ln(ln(1/Pu))", 1, line = 1, at = xlow, col = "black", cex = 0.7)

  axis(side = 1, -2:7, labels = pu, line = 5, col = "blue", col.axis = "blue",
       type = "l", lwd = 2.5 
       , cex.axis = 0.7)
  mtext("Pu", 1, line = 4, at = xlow, col = "blue", cex = 0.75)
  
  axis(side = 1, -2:7, labels = t, line = 8, col = "red", col.axis = "red", type = "l", lwd = 2.5,
        cex.axis = 0.7)
  mtext("T = 1/(1 - Pu)", 1, line = 7, at = xlow, col = "red", cex = 0.75)
  } else{
    if(xlabe == "y = - ln(ln(1/Pu))"){
      axis(side = 1, -2:7, line = 2, col = "black", type = "l", lwd = 2.5,  cex.axis = 0.7)
      mtext("y = -ln(ln(1/Pu))", 1, line = 1, at = xlow, col = "black", cex = 0.7)
    } else {
      if(xlabe == "Pu"){
        pu <- seq(0, 0.9, 0.1)
        axis(side = 1, -2:7, labels = pu, line = 2, col = "blue", col.axis = "blue", type = "l",
             lwd = 2.5,  cex.axis = 0.7)
        mtext("Pu", 1, line = 1, at = xlow, col = "blue", cex = 0.75)
      } else{# ylabe == "T"
        pu <- seq(0, 0.9, 0.1)
        t <- round(1/(1 - pu), 1)
        axis(side = 1, -2:7, labels = t, line = 2, col = "red", col.axis = "red", type = "l",
             lwd = 2.5,  cex.axis = 0.7)
        mtext("T = 1/(1 - Pu)", 1, line = 1, at = xlow, col = "red", cex = 0.75)
      }
    }
    
  }
  invisible(TRUE)
}
mydistrPlot <- function(object, a = 0, trans = FALSE, transfun = function(x){
  -log((log(1 / x)))
}, ylim = NULL, xlim = NULL, add = FALSE, ...){
  if (!add) {
    x <- sort(samp(object))
    empProb <- ppoints(x, a = a)
    distr = distr(object)
  }
  else {
    if (inherits(object, "DistrFit")) {
      distr = distr(object)
    }
  }
  prob <- seq(1e-05, 0.999999, by = 1e-05)
  xdistr <- qdistr(distr, prob)
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
    points(empProb, x)
  }
  else {
    lines(prob, xdistr, ...)
  }
}
