saisonalCheckFile <- function(){
  if(is.null(ActiveDataSet())){
    Message(message = gettextRcmdr("kein aktives Datensatz"), 
            type = "error")
    confirmDialog("Keine Stichprobe!", handler = function(h,...) {
      dispose(h$obj)
    })
    invisible(TRUE)
    
  } else{
    .GlobalEnv$activeFile <- eval(parse(text = ActiveDataSet()))
    .GlobalEnv$variables <- names(.GlobalEnv$activeFile)
    if(length(.GlobalEnv$variables) == 0){
      errorDialog("leeres Datensatz!", handler = function(h,...) {
        dispose(h$obj)
      })
    }
    else{
      # The saisonal daten will be construst such that the names variables
      # are "whq" und "shq". When there are not charaster that matche
      # these string, the default two first column will be supposed to be
      # winter and summer daten respetively.
      check.var <- prod(c("whq", "shq") %in% .GlobalEnv$variables)
      if(check.var){
        .GlobalEnv$saisonal.daten <- .GlobalEnv$activeFile[, c("whq", "shq")]
        .GlobalEnv$saisonal.daten <- .GlobalEnv$saisonal.daten[complete.cases(.GlobalEnv$saisonal.daten),]
        .GlobalEnv$saiso.ana <- FALSE
        windowSaisoAna()
      }else{
        if(length(variables) >= 2){
          .GlobalEnv$saisonal.daten <- .GlobalEnv$activeFile[,1:2]
          .GlobalEnv$saisonal.daten <- .GlobalEnv$saisonal.daten[complete.cases(.GlobalEnv$saisonal.daten),]
          names(.GlobalEnv$saisonal.daten) <- c("whq", "shq")
          # test that all variables are numerical
          is.num.var <- is.numeric(get(ActiveDataSet())[,1]) ||
            is.integer(get(ActiveDataSet())[,1])||
            is.numeric(get(ActiveDataSet())[,2]) ||
            is.integer(get(ActiveDataSet())[,2])
          .GlobalEnv$saiso.ana <- FALSE
          windowSaisoAna()
        }else{
          confirmDialog("Zwei Stichproben erwartet!", handler = function(h,...) {
            dispose(h$obj)
          })
        }
      }
    }
  }
}