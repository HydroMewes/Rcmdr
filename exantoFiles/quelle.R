required.packages <- c("hyddistr", "cairoDevice", "gWidgetstcltk", "xlsx", "plyr",
                       "gWidgetstcltk", "tcltk", "tkrplot", "lmom")

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
  Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_25')
  lapply(required.packages,installpkg)
  library(hyddistr)
  library(tkrplot)
  tclRequire("Tktable")
  library(gWidgetstcltk)
  library(xlsx)
  library(plyr)
  options ( guiToolkit = "RGtk2" )
  require(cairoDevice)
}
load.packages()
kurtosis  <- function(a){
  kurt <- (1 / length(samplee)) * sum((samplee - mean(samplee))^4) / 
    (1 / length(samplee)) * sum((samplee - mean(samplee))^2)^2
  return(unlist(list(kurt = kurt)))
}
hilfe <- function(){ browseURL(paste(file.path(path.package(package = "Rcmdr")[1], 
                                              "doc"), "/", gettextRcmdr("dokuexantov1 15"), 
                                    ".pdf", sep = ""))
}
