plotInWindow <- function(fPlot = plotIt){
  library(gWidgetstcltk)
  tclRequire("Tktable")
  options(guiToolkit = "tcltk")
  options(warn = -1)
  
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
    gfile(text=text, initialfilename = "Ergebnisse.CSV", filter = list("All files" = list(patterns = ".CVS")), type=type, ..., action = action, handler =
            function(h,...) {
              file.without.ext <- sub("^([^.]*).*", "\\1", h$file) 
              filen <- paste(file.without.ext, ".",
                             "xlsx", sep = "")
              tryCatch({
                if(svalue(.GlobalEnv$stichprobe.cbx) != "alle")
                {
                  saveAsExcelFile(datafrm = to.save.dfrm, file = filen)
                }
                else{
                  saveAll(all.dfrm = all.to.save.dfrm, file = filen)
                  print(filen)
                }
                .GlobalEnv$message.save <- "Parameters gespeichert!"
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
  .GlobalEnv$window <- gwindow("Anpassung", visible = FALSE, 
                               handler=function(h,...) {
                                 if("window" %in% ls(envir=.GlobalEnv)){
                                   rm(window, envir = .GlobalEnv)
                                 }
                               })
  GG <- ggroup(horizontal=TRUE, container = .GlobalEnv$window)
  group <- ggroup(horizontal=FALSE, container = GG)
  .GlobalEnv$groupgraph <- ggroup(horizontal=FALSE, container = GG, width = 1000,  height = 600)
  options <- glabel("Grafikoptionen", horizontal = FALSE, container = group)
  
  titlefrm <- gframe("Überschrift", horizontal = FALSE, container=group)
  .GlobalEnv$titlegrf <-gedit("Exanto", container = titlefrm)
  x.frm <- gframe("Y Untere und Obere Gr.", horizontal = FALSE, container=group)
  .GlobalEnv$xlow <-gedit(initial.msg="untere", container = x.frm)
  .GlobalEnv$xup <-gedit(initial.msg="obere", container = x.frm)
  y.frm <- gframe("HQ Untere und Obere Gr.", horizontal = FALSE, container=group)
  .GlobalEnv$ylow <-gedit(initial.msg="untere", container = y.frm)
  .GlobalEnv$yup <-gedit(initial.msg="obere", container = y.frm)
  # mache die Y-Achse-Skalierung automatisch
  svalue(.GlobalEnv$ylow) <- 0
  svalue(.GlobalEnv$yup) <- max(samp(.GlobalEnv$objects[[1]]))
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
                       },   
                       container = group.save.grf)
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
  refreshGrfLbl <- glabel("Grafik anzeigen...            ",
                          handler   = function(h,...) {
                            tryCatch({ 
                              # update the graphic
                              tkrreplot(.GlobalEnv$graph)
                              
                            },
                            error = function(c){
                              message("Error in btnRefreshGrph")
                              message("File: PlotInWin")
                              message("Save was not be completed!")
                              message(c)
                            })
                          }, container = group.refresh.grf)
  btnRefreshGrph <- gimage("refresh", 
                           dirname   = "stock",
                           handler   = function(h,...) {
                             tryCatch({ 
                               # update the graphic
                               tkrreplot(.GlobalEnv$graph)
                               
                             },
                             error = function(c){
                               message("Error in btnRefreshGrph")
                               message("File: PlotInWin")
                               message("Save was not be completed!")
                               message(c)
                             })
                           }, container = group.refresh.grf   
  )
  # first plot
  if(svalue(.GlobalEnv$radio.btn.grid) == "Ja"){
  .GlobalEnv$graph <- tkrplot(getToolkitWidget(.GlobalEnv$groupgraph),
                              function(){par(bg="white"); plotIt(); grid()},
                              hscale=2, vscale=1.4)
  } else{
    .GlobalEnv$graph <- tkrplot(getToolkitWidget(.GlobalEnv$groupgraph),
                                function(){par(bg="white"); plotIt()},
                                hscale=2, vscale=1.4)
  }
  add(groupgraph, .GlobalEnv$graph)
  group.prm <- ggroup(container=group, horizontale = TRUE)
  prm.meth <- glabel("Param. Vert. speichern...",
                     handler   = function(h,...) {
                       tryCatch({fileChoosePar()
                       },
                       #warning = function(c){
                       #  message("warning in btnSavePrm")
                       # message("File: PlotInWin")
                       #  message(c)
                       #},
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
                         #warning = function(c){
                         #  message("warning in btnSavePrm")
                          # message("File: PlotInWin")
                         #  message(c)
                         #},
                         error = function(c){
                           message("File: PlotInWin")
                           message("Save was not be completed!")
                           message(c)
                         })                   
                       }, container =  group.prm 
  )
  group.prm.display <- ggroup(container=group, horizontale = TRUE)
  prm.meth <- glabel("Ergebnis anzeigen...       ",
                     handler   = function(h,...) {
                       tryCatch({
                         if(svalue(.GlobalEnv$stichprobe.cbx) == "alle"){
                           dfrm <- .GlobalEnv$all.to.save.dfrm[[svalue(.GlobalEnv$samp.show.cbx)]]
                         }else{
                           # that will be the default action: already loaded!
                           dfrm <- .GlobalEnv$to.save.dfrm
                         }
                         dfrm <- builDisplay(dfrm = dfrm)
                         dfrm <- as.matrix(dfrm)
                         dfrm[is.na(dfrm)] <- ""
                         dfrm <- as.data.frame(dfrm)
                         table.res <- gtable(dfrm, multiple = FALSE, chosencol = 0, 
                                             container = gwindow("Displayausgabe", 
                                                                 parent = .GlobalEnv$window))
                       },
                       error = function(c){
                         message(paste("This error occurs in this bloc: Ergebnis Anzeigen..."))
                         message(paste("Datei: PlotInWin"))
                         message(c)
                       },
                       warning = function(c){
                         message(paste("This warning occurs in this bloc: Ergebnis Anzeigen..."))
                         message(paste("Datei: PlotInWin"))
                         message(c)
                       })
                       
                     }   
                     ,  container = group.prm.display)
  btnShowPrm <- gimage("refresh", 
                       dirname   = "stock",
                       handler   = function(h,...) {
                         tryCatch({
                           if(svalue(.GlobalEnv$stichprobe.cbx) == "alle"){
                             dfrm <- .GlobalEnv$all.to.save.dfrm[[svalue(.GlobalEnv$samp.show.cbx)]]
                           }else{
                             # that will be the default action: already loaded!
                             dfrm <- .GlobalEnv$to.save.dfrm
                           }
                           dfrm <- builDisplay(dfrm = dfrm)
                           dfrm <- as.matrix(dfrm)
                           dfrm[is.na(dfrm)] <- ""
                           dfrm <- as.data.frame(dfrm)
                           table.res <- gtable(dfrm, multiple = FALSE, chosencol = 0, 
                                               container = gwindow("Displayausgabe", 
                                                        parent = .GlobalEnv$window))
                         },
                         error = function(c){
                           message(paste("This error occurs in this bloc: Ergebnis Anzeigen..."))
                           message(paste("Datei: PlotInWin"))
                           message(c)
                         },
                         warning = function(c){
                           message(paste("This warning occurs in this bloc: Ergebnis Anzeigen..."))
                           message(paste("Datei: PlotInWin"))
                           message(c)
                         })
                         
                       }   
                       , container = group.prm.display)
  visible(.GlobalEnv$window) <- TRUE
  tkrreplot(.GlobalEnv$graph)
}