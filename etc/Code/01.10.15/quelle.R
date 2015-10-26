required.packages <- c("hyddistr", "cairoDevice", "gWidgetstcltk", "xlsx")

library("hyddistr", lib.loc="C:/Users/Cesaire/Documents/R/win-library/3.1")
required.packages <- c("cairoDevice", "gWidgetstcltk")
installpkg <- function(x){
  if(x %in% rownames(installed.packages()) == FALSE) {
    if(x %in% rownames(available.packages()) == FALSE) {
      paste(x,"is not a valid package - please check again...")
    } else {
      install.packages(x)           
    }
    
  } else {
    paste(x,"package already installed...")
  }
}
load.packages <- function(){
  lapply(required.packages,installpkg)
  library(hyddistr)
  library(cairoDevice)
  library(gWidgetstcltk)
  library(xlsx)
  options ( guiToolkit = "RGtk2" )
  require(cairoDevice)
}
load.packages()
