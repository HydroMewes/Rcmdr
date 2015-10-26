windowSaisoAna <- function(fPlot = plotItSaisonal(vect.obj = .GlobalEnv$saisonal.fit$obj.hq)){
  library(gWidgetstcltk)
  tclRequire("Tktable")
  options(guiToolkit = "tcltk")
  options(warn = -1)
  #.GlobalEnv$hqdaten <- read.csv("C:/Users/Cesaire/Documents/Wise/boulot/HQs.csv")
  if("window.saisonal" %in% ls(envir=.GlobalEnv)){
    dispose(window.saisonal)
    rm(window.saisonal, envir = .GlobalEnv)
  }
  if("win" %in% ls(envir=.GlobalEnv)){
    dispose(win)
    rm(win, envir = .GlobalEnv)
  }
  .GlobalEnv$saiso.ana <- FALSE
  updatePlot <- function(h,...) {
    fPlot()
  }
  
  fileChooseGrph <- function(action="setwd", text = "Speichern unter...",
                             type="save",...) {
    
    gfile(text=text, type=type, ..., action = action, handler =
            function(h,...) {
              file.without.ext <- sub("^([^.]*).*", "\\1", h$file) 
              filen <- paste(file.without.ext, ".",
                             svalue(typecbx), sep = "")
              tryCatch({
                .GlobalEnv$filename <- filen
                eval(parse(text = svalue(typecbx)))(filen)
                fPlot()
                title(main = svalue(titlegrf))
                dev.off()
                .GlobalEnv$message.save <- "Grafik gespeichert!"
                doItAndPrint("print(message.save)")
              },#code
              error = function(c){
                .GlobalEnv$message.save <- "error: Das Speichernprozess wurde abgebruchen: schliessen Sie bitte Ihre Datei bevor!"
                doItAndPrint("print(message.save)")
                message(c)
              })
            })
  }
  #@CompleteMe.......
  fileChoosePar <- function(action="setwd", text = "Speichern unter...",
                            type="save",...) {
    gfile(text=text, initialfilename = "Ergebnisse.CSV", type=type, ..., action = action, handler =
            function(h,...) {
              file.without.ext <- sub("^([^.]*).*", "\\1", h$file) 
              filen <- paste(file.without.ext, ".",
                             "xlsx", sep = "")
              tryCatch({
                if(TRUE)
                {
                  saveAsExcelFile(datafrm = .GlobalEnv$res.to.save, file = filen)
                }
                else{

                }
                .GlobalEnv$message.save <- "Parameter gespeichert!"
                doItAndPrint("print(message.save)")
              }, #code
              error = function(c){
                .GlobalEnv$message.save <- "Warnung: Das Speichernprozess wurde abgebruchen: schliessen Sie bitte Ihre Datei bevor!"
                doItAndPrint("print(message.save)")
                message("error: try to save in an opened file!")
                message("occured in fileChoosePar")
                message("file: plotInWin")
              })
            })
  }
  type.vect <- c("png", "bmp", "pdf", "jpeg", "tiff")
  # Now the Layout
  #gw <- gwindow("Anpassung", visible = FALSE, horinzontale = TRUE)
  .GlobalEnv$window.saisonal <- gwindow("Saisonale Analyse", visible = FALSE, 
                               handler=function(h,...) {
                                 if("window" %in% ls(envir=.GlobalEnv)){
                                   rm(window.saisonal, envir = .GlobalEnv)
                                 }
                               })
  GG <- ggroup(horizontal=TRUE, container = .GlobalEnv$window.saisonal)
# -------------- Einstellungsgrupp  --------------------------------------
  einstellunggroup <- ggroup(horizontal=FALSE, spacing = 10, container = GG)
  einstel.lab <- glabel("Einstellung", horizontal = FALSE, container = einstellunggroup)
  schwell <- gframe("Schwellwert", horizontal = FALSE, container=einstellunggroup)
.GlobalEnv$schwell.choose <- gradio(c("min Jahres HQ", "2.5 MQ"), selected = 1, horizontal = FALSE,
            container = schwell)
.GlobalEnv$schwell.MQ <- gedit(text = "",width = 15, initial.msg="MQ-Werte", container = schwell)
  aev <- gframe("AEV", horizontal = FALSE, container=einstellunggroup)
.GlobalEnv$aev.choose <- gradio(c("MM", "ML", "LM"), selected = 1, horizontal = FALSE,
                         container = aev)
  ergeb <- gframe("Ergebnisse anzeigen", horizontal = FALSE, container=einstellunggroup)
.GlobalEnv$HQ.choose <- gcheckboxgroup(c("WHQ", "SHQ", "JHQ", "Misch. Vert"), horizontal = FALSE,
                     container = ergeb, handler = function(h,...){
                       .GlobalEnv$HQ.index <- which(c("WHQ", "SHQ", "JHQ", "Misch. Vert") %in% svalue(.GlobalEnv$HQ.choose))
                     })
  gitter <- gframe("Gitter im Plot", horizontal = FALSE, container=einstellunggroup)
.GlobalEnv$gitter.choose <- gradio(c("Ja", "Nein"), selected = 1, horizontal = FALSE,
                     container = gitter, handler = function(h,...){
                       #print(svalue(.GlobalEnv$gitter.choose))
                     })
  auswertung <- gframe("Auswertung", horizontal = FALSE, container = einstellunggroup)
.GlobalEnv$auswert.btn <- gbutton(text = "Auswertung", border=TRUE, container = auswertung, handler = function(h,...){
  getCheckParam()
  .GlobalEnv$legend.title <- svalue(.GlobalEnv$aev.choose)
  .GlobalEnv$Misch.fitted.bool <- FALSE
  .GlobalEnv$message.fit <- "Anpassung der saisonalen Daten..."
  doItAndPrint("print(message.fit)")
  .GlobalEnv$saisonal.fit <- saisonalFit()
  #.GlobalEnv$obj.hq <- .GlobalEnv$saisonal.fit$obj.hq
  analyse.result <- buildSaisonalDataFrameEx(file.name = ActiveDataSet())
  .GlobalEnv$res.to.display <- analyse.result$to.display
  .GlobalEnv$res.to.save <- analyse.result$to.save
  #.GlobalEnv$to.save <- buildSaisonalDataFrameSave()
  .GlobalEnv$saiso.ana <- TRUE
  .GlobalEnv$message.fit <- "Die Grafik wird geplottet..."
  doItAndPrint("print(message.fit)")
  tkrreplot(.GlobalEnv$graph)
  .GlobalEnv$message.fit <- "Die Grafik ist fertig!"
  doItAndPrint("print(message.fit)")
  .GlobalEnv$saiso.ana <- TRUE
})
# -----------------------------------------------------------------------------
  group <- ggroup(horizontal=FALSE, container = GG)
  .GlobalEnv$groupgraph <- ggroup(horizontal=FALSE, container = GG, width = 1000,  height = 600)
  options <- glabel("Grafikoptionen", horizontal = FALSE, container = group)
  
  titlefrm <- gframe("Überschrift", horizontal = FALSE, container=group)
  .GlobalEnv$titlegrf <-gedit("Exanto", container = titlefrm)
  x.frm <- gframe("Y Untere und Obere Gr.", horizontal = FALSE, container=group)
  .GlobalEnv$xlow <-gedit(initial.msg="-2", container = x.frm)
  .GlobalEnv$xup <-gedit(initial.msg="7", container = x.frm)
  y.frm <- gframe("HQ Untere und Obere Gr.", horizontal = FALSE, container=group)
  .GlobalEnv$ylow <-gedit(initial.msg="0", container = y.frm)
  .GlobalEnv$yup <-gedit(initial.msg = as.character(max(saisonal.daten, na.rm = TRUE)), container = y.frm)
  lab.frm <- gframe("X-Achse", horizontal = FALSE, container=group)
  .GlobalEnv$xlabel <-gcombobox(c("alle", "y = - ln(ln(1/Pu))", "Pu", "T"), container = lab.frm)
  dateifrm <- gframe("Typ Datei", horizontal = FALSE, container=group)
  typecbx <- gcombobox(type.vect, container = dateifrm)
  group.refresh.grf <- ggroup(horizontal = TRUE, container=group)
  group.save.grf <- ggroup(horizontal = TRUE, container=group)
  saveGrfLbl <- glabel("Grafik speichern...           ",
                       handler   = function(h,...) {
                         tryCatch({fileChooseGrph()},
                                  error = function(c){
                                    message("Error in btnSaveGrph")
                                    message("File: PlotInWin")
                                    message("Save was not be completed!")
                                    message(c)
                                  })
                       },container = group.save.grf)
  btnSaveGrph <- gimage("save", 
                        dirname   = "stock",
                        handler   = function(h,...) {
                          tryCatch({fileChooseGrph()},
                                   error = function(c){
                                     message("Error in btnSaveGrph")
                                     message("File: PlotInWin")
                                     message("Save was not be completed!")
                                     message(c)
                                   })
                        }   
                        , container = group.save.grf)
  refreshGrfLbl <- glabel("Grafik anzeigen...            ", container = group.refresh.grf,
                          handler = function(h,...) {
                            tryCatch({ 
                              # update the graphic
                              tkrreplot(.GlobalEnv$graph)
                              
                            },
                            error = function(c){
                              message("Error in btnRefreshGrph")
                              message("File: saisonaleAnalyse")
                              message("Save was not be completed!")
                              message(c)
                            })
                          })
  btnRefreshGrph <- gimage("refresh", 
                           dirname   = "stock",
                           handler   = function(h,...) {
                             tryCatch({ 
                               # update the graphic
                               tkrreplot(.GlobalEnv$graph)
                               
                             },
                             error = function(c){
                               message("Error in btnRefreshGrph")
                               message("File: saisonaleAnalyse")
                               message("Save was not be completed!")
                               message(c)
                             })
                           }, container = group.refresh.grf   
  )
  # first plot
  if(svalue(.GlobalEnv$gitter.choose) == "Ja"){
    .GlobalEnv$graph <- tkrplot(getToolkitWidget(.GlobalEnv$groupgraph),
                                function(){par(bg="white");
                                           if(!.GlobalEnv$saiso.ana){
                                           #plot.new(); grid()
                                            plotItSaisonal(vect.obj = .GlobalEnv$saisonal.fit$obj.hq)
                                           } else{
                                             plotItSaisonal(vect.obj = .GlobalEnv$saisonal.fit$obj.hq)
                                           }
                                           },
                                hscale=2, vscale=1.4)
    tkrreplot(.GlobalEnv$graph)
  } else{
    .GlobalEnv$graph <- tkrplot(getToolkitWidget(.GlobalEnv$groupgraph),
                                function(){par(bg="white"); plotItSaisonal(vect.obj = .GlobalEnv$obj.hq)},
                                hscale=2, vscale=1.4)
  }

######

######
  add(groupgraph, .GlobalEnv$graph)
  group.prm <- ggroup(container=group, horizontale = TRUE)
  prm.meth <- glabel("Param. Vert. speichern...",handler   = function(h,...) {
    tryCatch({fileChoosePar()
    },
    error = function(c){
      message("File: PlotInWin")
      message("Save was not be completed!")
      message(c)
    })                   
  }, container = group.prm)
  btnSavePrm <- gimage("save", 
                       dirname   = "stock",
                       handler   = function(h,...) {
                         tryCatch({fileChoosePar()
                         },
                         error = function(c){
                           message("File: PlotInWin")
                           message("Save was not be completed!")
                           message(c)
                         })                   
                       }, container =  group.prm 
  )
  group.prm.display <- ggroup(container=group, horizontale = TRUE)
  prm.meth <- glabel("Ergebnis zeigen...       ",
                     handler   = function(h,...) {
                       if(!.GlobalEnv$saiso.ana) return()#keine Berechnung wurde durchgefuhrt.
                       tryCatch({
                         if(FALSE){
                           #  only the selected index will be displayed
                           dfrm <- .GlobalEnv$res.to.display[,c(1:4, 4 + .GlobalEnv$HQ.index)]
                         }else{
                           #  only the selected index will be displayed
                           dfrm <- .GlobalEnv$res.to.display[,c(1:4, 4 + .GlobalEnv$HQ.index)]
                         }
                         #dfrm <- buildSaisonalDataFrameEx(file.name = "filename")
                         #  only the selected index will be displayed
                         dfrm <- .GlobalEnv$res.to.display[,c(1:4, 4 + .GlobalEnv$HQ.index)]
                         dfrm <- as.matrix(dfrm)
                         dfrm[is.na(dfrm)] <- ""
                         dfrm <- as.data.frame(dfrm)
                         table.res <- gtable(dfrm, multiple = FALSE, chosencol = 0, 
                                             container = gwindow("Displayausgabe", 
                                                                 parent = .GlobalEnv$window.saisonal))
                       },
                       error = function(c){
                         message(paste("This error occurs in this bloc: Ergebnis anzeigen..."))
                         message(paste("Datei: saisonalAnalyse"))
                         message(c)
                       }#,
                       #warning = function(c){
                       #  message(paste("This warning occurs in this bloc: Ergebnis anzeigen..."))
                       #  message(paste("Datei: PlotInWin"))
                       #  message(c)
                       #}
                       )
                       
                     }, container = group.prm.display)
  btnShowPrm <- gimage("refresh", 
                       dirname   = "stock",
                       handler   = function(h,...) {
                         tryCatch({
                           if(FALSE){
                             #  only the selected index will be displayed
                             dfrm <- .GlobalEnv$res.to.display[,c(1:4, 4 + .GlobalEnv$HQ.index)]
                           }else{
                             #  only the selected index will be displayed
                             dfrm <- .GlobalEnv$res.to.display[,c(1:4, 4 + .GlobalEnv$HQ.index)]
                           }
                           #dfrm <- buildSaisonalDataFrameEx(file.name = "filename")
                           #  only the selected index will be displayed
                           dfrm <- .GlobalEnv$res.to.display[,c(1:4, 4 + .GlobalEnv$HQ.index)]
                           dfrm <- as.matrix(dfrm)
                           dfrm[is.na(dfrm)] <- ""
                           dfrm <- as.data.frame(dfrm)
                           table.res <- gtable(dfrm, multiple = FALSE, chosencol = 0, 
                                               container = gwindow("Displayausgabe", 
                                                                   parent = .GlobalEnv$window.saisonal))
                         },
                         error = function(c){
                           message(paste("This error occurs in this bloc: Ergebnis anzeigen..."))
                           message(paste("Datei: saisonalAnalyse"))
                           message(c)
                         }#,
                         #warning = function(c){
                         #  message(paste("This warning occurs in this bloc: Ergebnis anzeigen..."))
                         #  message(paste("Datei: PlotInWin"))
                         #  message(c)
                         #}
                         )
                         
                       }   
                       , container = group.prm.display)
  visible(.GlobalEnv$window.saisonal) <- TRUE
  tkrreplot(.GlobalEnv$graph)
}
#windowSaisoAna()