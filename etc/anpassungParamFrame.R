options(warn = -1)
# dialogAnpassung construct the mask-window with all distribution
# return nothing will be return
dialogAnpassung <- function()
{
  library(gWidgetstcltk)
  options(guiToolkit = "tcltk")
  options(warn = -1) 
  if("win" %in% ls(envir=.GlobalEnv)){
    dispose(win)
    rm(win, envir = .GlobalEnv)
  }
  if("window.saisonal" %in% ls(envir=.GlobalEnv)){
    dispose(window.saisonal)
    rm(window.saisonal, envir = .GlobalEnv)
  }
  .GlobalEnv$win <- gwindow("Statistische Analyse", visible = FALSE, 
                            handler=function(h,...) {
                              if("window" %in% ls(envir=.GlobalEnv)){
                                dispose(window)
                                rm(window, envir = .GlobalEnv)
                              }
                              rm(win, envir = .GlobalEnv)
                            })
  AEV.Weibull <- ggroup(horizontal=TRUE, container = .GlobalEnv$win)
  
  frm.AEV <- gframe("AEV", container = AEV.Weibull)
  .GlobalEnv$check.AEV.MM <- gcheckbox("MM", container = frm.AEV) 
  .GlobalEnv$check.AEV.LM <- gcheckbox("LM", container = frm.AEV)
  .GlobalEnv$check.AEV.ML <- gcheckbox("ML", container = frm.AEV)
  frm.Weibull <- gframe("Weibull", container = AEV.Weibull)
  .GlobalEnv$check.Weibull.MM <- gcheckbox("MM", container = frm.Weibull) 
  .GlobalEnv$check.Weibull.LM <- gcheckbox("LM", container = frm.Weibull)
  .GlobalEnv$check.Weibull.ML <- gcheckbox("ML", container = frm.Weibull)
  
  logW.gumbel <- ggroup(horizontal=TRUE, container=.GlobalEnv$win)
  frm.logW <- gframe("logWeibull", container = logW.gumbel)
  .GlobalEnv$check.logW.MM <- gcheckbox("MM", container = frm.logW) 
  .GlobalEnv$check.logW.LM <- gcheckbox("LM", container = frm.logW)
  .GlobalEnv$check.logW.ML <- gcheckbox("ML", container = frm.logW)
  frm.gumbel <- gframe("Gumbel-Verteilung", container = logW.gumbel)
  .GlobalEnv$check.gumbel.MM <- gcheckbox("MM", container = frm.gumbel) 
  .GlobalEnv$check.gumbel.LM <- gcheckbox("LM", container = frm.gumbel)
  .GlobalEnv$check.gumbel.ML <- gcheckbox("ML", container = frm.gumbel)
  
  expo.pears <- ggroup(horizontal=TRUE, container=.GlobalEnv$win)
  frm.expo <- gframe("Exponential-Verteilung", container = expo.pears)
  .GlobalEnv$check.expo.MM <- gcheckbox("MM", container = frm.expo) 
  .GlobalEnv$check.expo.LM <- gcheckbox("LM", container = frm.expo)
  .GlobalEnv$check.expo.ML <- gcheckbox("ML", container = frm.expo)
  frm.pears <- gframe("Pearson 3-Verteilung", container = expo.pears)
  .GlobalEnv$check.pears.MM <- gcheckbox("MM", container = frm.pears) 
  .GlobalEnv$check.pears.LM <- gcheckbox("LM", container = frm.pears)
  .GlobalEnv$check.pears.ML <- gcheckbox("ML", container = frm.pears)
  
  
  logP.gamma <- ggroup(horizontal=TRUE, container=.GlobalEnv$win)
  frm.logP <- gframe("logPearson 3-Verteil.", container = logP.gamma)
  .GlobalEnv$check.logP.MM <- gcheckbox("MM", container = frm.logP) 
  .GlobalEnv$check.logP.LM <- gcheckbox("LM", container = frm.logP)
  .GlobalEnv$check.logP.ML <- gcheckbox("ML", container = frm.logP)
  frm.gamma <- gframe("Gamma-Verteilung", container = logP.gamma)
  .GlobalEnv$check.gamma.MM <- gcheckbox("MM", container = frm.gamma) 
  .GlobalEnv$check.gamma.LM <- gcheckbox("LM", container = frm.gamma)
  .GlobalEnv$check.gamma.ML <- gcheckbox("ML", container = frm.gamma)
  
  
  frech.norm <- ggroup(horizontal=TRUE, container=.GlobalEnv$win)
  frm.frech <- gframe("Frechet-Verteilung", container = frech.norm)
  .GlobalEnv$check.frech.MM <- gcheckbox("MM", container = frm.frech) 
  .GlobalEnv$check.frech.LM <- gcheckbox("LM", container = frm.frech)
  .GlobalEnv$check.frech.ML <- gcheckbox("ML", container = frm.frech)
  frm.normal <- gframe("Normal-Verteilung", container = frech.norm)
  .GlobalEnv$check.normal.MM <- gcheckbox("MM", container = frm.normal) 
  .GlobalEnv$check.normal.LM <- gcheckbox("LM", container = frm.normal)
  .GlobalEnv$check.normal.ML <- gcheckbox("ML", container = frm.normal)
  
  logN.log3PN <- ggroup(horizontal=TRUE, container=.GlobalEnv$win)
  frm.logN <- gframe("log-Normal-Vert.", container = logN.log3PN)
  .GlobalEnv$check.logN.MM <- gcheckbox("MM", container = frm.logN) 
  .GlobalEnv$check.logN.LM <- gcheckbox("LM", container = frm.logN)
  .GlobalEnv$check.logN.ML <- gcheckbox("ML", container = frm.logN)
  frm.log3PN <- gframe("log-3P-Normal-Vert.", container = logN.log3PN)
  .GlobalEnv$check.log3PN.MM <- gcheckbox("MM", container = frm.log3PN) 
  .GlobalEnv$check.log3PN.LM <- gcheckbox("LM", container = frm.log3PN)
  .GlobalEnv$check.log3PN.ML <- gcheckbox("ML", container = frm.log3PN)
  
  if(length(names(.GlobalEnv$activeFile)) >= 1){
    wahl.stichprobe <- ggroup(horizontal=TRUE, container=.GlobalEnv$win)
    frm.wahl.stichprobe <- gframe("Stichprobe", container = wahl.stichprobe)
    stichproben <- c(names(.GlobalEnv$activeFile), "alle")
    .GlobalEnv$stichprobe.cbx <- gcombobox(stichproben, container = frm.wahl.stichprobe)
    
    # Which graphic will be schown
    samp.show.dfrm <- gframe("Ergebnisse anzeigen", container = wahl.stichprobe)
    .GlobalEnv$samp.show.cbx <- gcombobox(c(names(.GlobalEnv$activeFile)),
                                          container = samp.show.dfrm)
    svalue(samp.show.cbx) <- svalue(.GlobalEnv$stichprobe.cbx)
  } else{
    .GlobalEnv$stichprobe.cbx <- names(.GlobalEnv$activeFile)[1]
    .GlobalEnv$samp.show.cbx <- .GlobalEnv$stichprobe.cbx
  }
  
  grid.auswertung <- ggroup(horizontal=TRUE, container=.GlobalEnv$win)
  frm.grid <- gframe("Grafik mit Gitter", container = grid.auswertung)
  grid.option <- c("Ja", "Nein")
  .GlobalEnv$radio.btn.grid <- gradio(grid.option, container = frm.grid)
  addHandlerClicked(radio.btn.grid, handler=function(h,..) {
    cat(sprintf("You picked %s\n", 2))
    print(svalue(.GlobalEnv$radio.btn.grid))
  })
  
  frm.auswertung <- gframe("Auswertung", container = grid.auswertung,
                           horizpntal = FALSE)
  
  
  btnAuswet <- gbutton("Auswertung", container = frm.auswertung,
                       handler = function(h, ...)
                       {
                         is.num.var <- is.numeric(get(ActiveDataSet())[,.GlobalEnv$activeVar]) ||
                           is.integer(get(ActiveDataSet())[,.GlobalEnv$activeVar])
                         #is the active variable numerical?
                         
                         if(!is.num.var){
                           Message(message = gettextRcmdr("numeric Variable erwartet!",
                                                          type = "warning"))
                           
                           confirmDialog("Falsche Kodierung des Datensatzes!", handler = function(h,...) {
                             dispose(h$obj)
                           })
                           stop("error bei Einlesen des Datensatzes!")
                         }
                         
                         if(svalue(.GlobalEnv$stichprobe.cbx) != "alle" & 
                              svalue(.GlobalEnv$stichprobe.cbx) != svalue(.GlobalEnv$samp.show.cbx)){
 
                                     .GlobalEnv$error.St <- errorDialog("Stichproben: Falsche Wahl!",
                                                                        handler = function(h,...) {
                                       dispose(h$obj)
                                     }, parent = .GlobalEnv$win)

                         }
                         else{
                           .GlobalEnv$activeDataSet <- .GlobalEnv$activeFile[, 
                                                                             which(names(.GlobalEnv$activeFile) == svalue(.GlobalEnv$samp.show.cbx))]
                           .GlobalEnv$activeDataSet <- as.data.frame(.GlobalEnv$activeDataSet)
                           names(.GlobalEnv$activeDataSet) <- svalue(.GlobalEnv$samp.show.cbx)
                           .GlobalEnv$activeVar <- svalue(.GlobalEnv$samp.show.cbx)
                           .GlobalEnv$at.least.one.vert <- isSelectedMethod()
                           #  isSelectedMethod() tests, mhether at least one Method had been
                           #  selected
                           if(!.GlobalEnv$at.least.one.vert){
                             errorDialog("Waehlen Sie eine Methode!", handler = function(h,...) {
                               dispose(h$obj)
                             })
                           }# test wether at least one distribution was been selected
                           else{
                             # cas of: at least one method had been choosed
                             if(svalue(.GlobalEnv$stichprobe.cbx) != "alle"){
                               fitIt()
                               .GlobalEnv$to.save.dfrm <- buildDataFrameEx(file.name = ActiveDataSet(),
                                                                           sample.prop = setSamplePropEx(),
                                                                           fitted.param = setFitTest(),
                                                                           estim.param = setEstimatedParamEx())
                               #plotInWindow(fPlot = plotIt)
                             }
                             else{
                               fitIt()
                               .GlobalEnv$all.to.save.dfrm <- buildAllDataFrameEx(file = .GlobalEnv$activeFile)
                               #plotInWindow(fPlot = plotIt)
                             }
                             tryCatch({
                               if("window" %in% ls(envir=.GlobalEnv)){
                                 print("da!!!!!!!")
                                 dispose(window)
                                 print("do!!!!!!!")
                                 rm(window, envir = .GlobalEnv)
                               }
                             },
                             error = function(cond) "error",
                             warning = function(cond) {
                               message(paste("Graphical window:"))
                               message("attempt to destroy an absoluted object:
                                     funtion dialogAnpassung in anpassungParamFrame")
                               message(cond)
                               # Choose a return value in case of warning
                               return(NULL)
                             },
                             finally = {})
                             .GlobalEnv$message.fit <- "Die Grafik wird geplottet..."
                             doItAndPrint("print(message.fit)")
                             plotInWindow(fPlot = plotIt)
                             .GlobalEnv$message.fit <- "Die Grafik ist fertig!"
                             doItAndPrint("print(message.fit)")
                             #plotIt()
                             
                             # plotInWindow(fPlot = plotIt)
                             # plotIt()
                           }
                         }
                       }
  )
  
  btnClear <- gbutton("Clear", container = frm.auswertung,
                      handler = function(h, ...)
                      {
                        svalue(.GlobalEnv$check.AEV.MM) <- FALSE
                        svalue(.GlobalEnv$check.AEV.LM) <- FALSE
                        svalue(.GlobalEnv$check.AEV.ML) <- FALSE
                        svalue(.GlobalEnv$check.Weibull.MM) <- FALSE
                        svalue(.GlobalEnv$check.Weibull.LM) <- FALSE
                        svalue(.GlobalEnv$check.Weibull.ML) <- FALSE
                        svalue(.GlobalEnv$check.logW.MM) <- FALSE
                        svalue(.GlobalEnv$check.logW.LM) <- FALSE
                        svalue(.GlobalEnv$check.logW.ML) <- FALSE
                        svalue(.GlobalEnv$check.gumbel.MM) <- FALSE 
                        svalue(.GlobalEnv$check.gumbel.LM) <- FALSE
                        svalue(.GlobalEnv$check.gumbel.ML) <- FALSE
                        svalue(.GlobalEnv$check.expo.MM) <- FALSE 
                        svalue(.GlobalEnv$check.expo.LM) <- FALSE
                        svalue(.GlobalEnv$check.expo.ML) <- FALSE
                        svalue(.GlobalEnv$check.pears.MM) <- FALSE 
                        svalue(.GlobalEnv$check.pears.LM) <- FALSE
                        svalue(.GlobalEnv$check.pears.ML) <- FALSE
                        svalue(.GlobalEnv$check.logP.MM) <- FALSE
                        svalue(.GlobalEnv$check.logP.LM) <- FALSE
                        svalue(.GlobalEnv$check.logP.ML) <- FALSE
                        svalue(.GlobalEnv$check.gamma.MM) <- FALSE 
                        svalue(.GlobalEnv$check.gamma.LM) <- FALSE
                        svalue(.GlobalEnv$check.gamma.ML) <- FALSE
                        svalue(.GlobalEnv$check.frech.MM) <- FALSE 
                        svalue(.GlobalEnv$check.frech.LM) <- FALSE
                        svalue(.GlobalEnv$check.frech.ML) <- FALSE
                        svalue(.GlobalEnv$check.normal.MM) <- FALSE 
                        svalue(.GlobalEnv$check.normal.LM) <- FALSE
                        svalue(.GlobalEnv$check.normal.ML) <- FALSE
                        svalue(.GlobalEnv$check.logN.MM) <- FALSE 
                        svalue(.GlobalEnv$check.logN.LM) <- FALSE
                        svalue(.GlobalEnv$check.logN.ML) <- FALSE
                        svalue(.GlobalEnv$check.log3PN.MM) <- FALSE 
                        svalue(.GlobalEnv$check.log3PN.LM) <- FALSE
                        svalue(.GlobalEnv$check.log3PN.ML) <- FALSE
                        tryCatch({
                          if("window" %in% ls(envir=.GlobalEnv)){
                            dispose(window)
                            rm(window, envir = .GlobalEnv)
                          }
                        },
                        error = function(c){
                          message("attempt to destroy an absoluted object:
                                     funtion dialogAnpassung in anpassungParamFrame")
                        },                           warning = function(cond) {
                          message(paste("Graphical window:"))
                          message("attempt to destroy an absoluted object:
                                     funtion dialogAnpassung in anpassungParamFrame")
                          message(cond)
                          # Choose a return value in case of warning
                          return(NULL)
                        },
                        finally = {}
                        )  
                      }
  )
  
  visible(.GlobalEnv$win) <- TRUE
}

#dialogAnpassung()
isSelectedMethod <- function(){
  svalue(.GlobalEnv$check.AEV.MM) ||
    svalue(.GlobalEnv$check.AEV.LM) ||
    svalue(.GlobalEnv$check.AEV.ML) ||
    svalue(.GlobalEnv$check.Weibull.MM) ||
    svalue(.GlobalEnv$check.Weibull.LM) ||
    svalue(.GlobalEnv$check.Weibull.ML) ||
    svalue(.GlobalEnv$check.logW.MM) ||
    svalue(.GlobalEnv$check.logW.LM) ||
    svalue(.GlobalEnv$check.logW.ML) ||
    svalue(.GlobalEnv$check.gumbel.MM) || 
    svalue(.GlobalEnv$check.gumbel.LM) ||
    svalue(.GlobalEnv$check.gumbel.ML) ||
    svalue(.GlobalEnv$check.expo.MM) || 
    svalue(.GlobalEnv$check.expo.LM) ||
    svalue(.GlobalEnv$check.expo.ML) ||
    svalue(.GlobalEnv$check.pears.MM) || 
    svalue(.GlobalEnv$check.pears.LM) ||
    svalue(.GlobalEnv$check.pears.ML) ||
    svalue(.GlobalEnv$check.logP.MM) ||
    svalue(.GlobalEnv$check.logP.LM) ||
    svalue(.GlobalEnv$check.logP.ML) ||
    svalue(.GlobalEnv$check.gamma.MM) || 
    svalue(.GlobalEnv$check.gamma.LM) ||
    svalue(.GlobalEnv$check.gamma.ML) ||
    svalue(.GlobalEnv$check.frech.MM) || 
    svalue(.GlobalEnv$check.frech.LM) ||
    svalue(.GlobalEnv$check.frech.ML) ||
    svalue(.GlobalEnv$check.normal.MM) || 
    svalue(.GlobalEnv$check.normal.LM) ||
    svalue(.GlobalEnv$check.normal.ML) ||
    svalue(.GlobalEnv$check.logN.MM) || 
    svalue(.GlobalEnv$check.logN.LM) ||
    svalue(.GlobalEnv$check.logN.ML) ||
    svalue(.GlobalEnv$check.log3PN.MM) || 
    svalue(.GlobalEnv$check.log3PN.LM) ||
    svalue(.GlobalEnv$check.log3PN.ML)
}