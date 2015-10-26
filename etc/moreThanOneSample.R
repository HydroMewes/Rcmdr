# The function build the data frames to be saved for all the data set.
buildAllDataFrameEx <- function(file = .GlobalEnv$activeFile){
  temp.objects <- .GlobalEnv$objects
  temp.activeDataSet <- .GlobalEnv$activeDataSet
  temp.activeVar  <- .GlobalEnv$activeVar
  .GlobalEnv$index.i <- 1
  n <- names(file)
  all.dfrm <- lapply(file, function(dfrm){
    .GlobalEnv$activeDataSet <- as.data.frame(dfrm)
    .GlobalEnv$activeVar  <- .GlobalEnv$variables[index.i]
    names(.GlobalEnv$activeDataSet) <- n[.GlobalEnv$index.i]
    fitIt()#a vector of objects will be generated as global object
    .GlobalEnv$index.i <- .GlobalEnv$index.i + 1
    #View(file[,.GlobalEnv$i])
    #View(dfrm)
   # cat("la reele valeur de i est la suivant:\n")
  #  print(.GlobalEnv$index.i)
   # View(buildDataFrameEx())
    return(buildDataFrameEx())
  })
  rm(index.i)
  .GlobalEnv$objects <- temp.objects
  .GlobalEnv$activeDataSet <- temp.activeDataSet
  temp.activeVar  <- .GlobalEnv$activeVar
  return(all.dfrm)
}

saveAll <- function(all.dfrm, file){
  .GlobalEnv$app <- FALSE
  .GlobalEnv$j <- 1
  lapply(all.dfrm, function(dfrm){
    write.xlsx2(dfrm, file = file, append = .GlobalEnv$app, 
                    sheetName = names(.GlobalEnv$activeFile)[.GlobalEnv$j], 
                col.names = FALSE, row.names = FALSE, showNA = FALSE)
    .GlobalEnv$j <- .GlobalEnv$j + 1
    .GlobalEnv$app <- TRUE
    invisible(TRUE)
  })
  rm(app)
  rm(j)
}
