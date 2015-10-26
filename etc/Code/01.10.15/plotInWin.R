plotInWindow <- function(fPlot = fitIt){
  options ( guiToolkit = "RGtk2" )
  require(cairoDevice)
  
  updatePlot <- function(h,...) {
    fPlot()
    title(main = svalue(titlegrf))
  }
  
  fileChooseGrph <- function(action="setwd", text = "Speichern unter...",
                         type="save",...) {

    gfile(text=text, type=type, ..., action = action, handler =
            function(h,...) {
              #do.call(h$action, list(h$file))
              #eval(parse(text = svalue(typecbx)))(path)
              file.without.ext <- sub("^([^.]*).*", "\\1", h$file) 
              filen <- paste(file.without.ext, ".",
                            svalue(typecbx), sep = "")
              eval(parse(text = svalue(typecbx)))(filen)
              fPlot()
              title(main = svalue(titlegrf))
              dev.off()
            })
  }
  #@CompleteMe.......
  fileChoosePar <- function(action="setwd", text = "Speichern unter...",
                            type="save",...) {
    gfile(text=text, type=type, ..., action = action, handler =
            function(h,...) {
              file.without.ext <- sub("^([^.]*).*", "\\1", h$file) 
              filen <- paste(file.without.ext, ".",
                             "xlsx", sep = "")
              .GlobalEnv$append.GEV <- FALSE
              for(i in 1:length(.GlobalEnv$objects)){
                ob <- .GlobalEnv$objects[i]
                obj <- ob[[1]]
                addToDataFrame(a = obj)
              }
              obj.class <- sapply(.GlobalEnv$objects, function(a) class(a@distr))
              datafrm.tosave <- unique(obj.class)
              print(datafrm.tosave)
              .GlobalEnv$appendd <- FALSE
              sapply(datafrm.tosave, function(dfrm){
                datafrm <- eval(parse(text = dfrm))
                saveAsExcelFile(datafrm = datafrm, filename = filen, append = .GlobalEnv$appendd)
                print("j execute le save")
              })
              .GlobalEnv$appendd <- FALSE
            })
  }
  type.vect <- c("png", "bmp", "pdf", "jpeg", "tiff")
  ## now layout
  window <- gwindow("Anpassung")
  BigGroup <- ggroup(cont=window)
  group <- ggroup(horizontal=FALSE, container=BigGroup)
  options <- glabel("Grafiksoptionen", horizontal = FALSE, container=group)
  #btnShow <- gbutton("anzeigen", handler = updatePlot)

  refreshGrfLbl <- glabel("Grafik anzeigen...            ")
  btnRefreshGrph <- gimage("refresh", 
                        dirname   = "stock",
                        handler   = function(h,...) {updatePlot()}   
  )
  titlefrm <- gframe("Ueberschrift", horizontal = FALSE, container=group)
  titlegrf <-gedit("Exanto", container = titlefrm)
  #xlabfrm <- gframe("Xlab", horizontal = FALSE, container=group)
  #xlabv <-gedit("x axis", container = xlabfrm)
  #ylabfrm <- gframe("Ylab", horizontal = FALSE, container=group)
  #ylabv <-gedit("y axis", container = ylabfrm) 
  dateifrm <- gframe("Datei Type", horizontal = FALSE, container=group)
  typecbx <- gcombobox(type.vect)
  
  group.refresh.grf <- ggroup(container=group)
  group.save.grf <- ggroup(container=group)
  saveGrfLbl <- glabel("Grafik speichern...           ")
  btnSaveGrph <- gimage("save", 
                        dirname   = "stock",
                        handler   = function(h,...) {fileChooseGrph()}   
  )
  #btnSaveGrph <- gbutton("Speichern", handler = function(h,...) {fileChooseGrph()})
  #add(options, btnShow)
  #add(options, btnSaveGrph)
  add(dateifrm, typecbx)
  add(BigGroup, ggraphics())
  add(group.refresh.grf, refreshGrfLbl)
  add(group.refresh.grf, btnRefreshGrph)
  add(group.save.grf, saveGrfLbl)
  add(group.save.grf, btnSaveGrph)
  # Parameter der Verteilungsfunktionen
  #btnSavePrm <- gbutton("Speichern", handler = function(h,...) {fileChoosePar()})
  group.prm <- ggroup(container=group)
  prm.meth <- glabel("Param. Vert. speichern...")
  btnSavePrm <- gimage("save", 
                    dirname   = "stock",
                    handler   = function(h,...) {fileChoosePar()}   
  )
  add(group.prm, prm.meth)
  add(group.prm, btnSavePrm)
  
  #Ergebnisse der Anpassungstests
  group.anp.test <- ggroup(container=group)
  anp.test.meth <- glabel("Ergeb. Anpassungstest...")
  btnSaveAnp <- gimage("save", 
                       dirname   = "stock",
                       handler   = function(h,...) {fileChoosePar()}   
  )
  add(group.anp.test, anp.test.meth)
  add(group.anp.test, btnSaveAnp)
  
}

#plotInWindow(fPlot = function(){plot(1:5, 1:5)})
