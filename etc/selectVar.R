selectVariable <- function(){
  .GlobalEnv$activeFile <- eval(parse(text = ActiveDataSet()))
  .GlobalEnv$variables <- names(.GlobalEnv$activeFile)
  if(length(.GlobalEnv$variables) == 0){
    errorDialog("leeres Datensatz!", handler = function(h,...) {
      dispose(h$obj)
    })
  }#test that the data set contains at least one variable
  else{
    #at least one variable in data set
    .GlobalEnv$activeVar <- .GlobalEnv$variables[1]
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
    if(length(.GlobalEnv$variables) > 1){

        }else{
          is.num.var <- TRUE

    }
  }
  return(is.num.var)
}