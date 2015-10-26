selectVariable <- function(){
  .GlobalEnv$activeDataSet <- eval(parse(text = ActiveDataSet()))
  .GlobalEnv$variables <- names(get(ActiveDataSet()))
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
      
      confirmDialog("Die gewaehlte Variable ist nicht numeric!", handler = function(h,...) {
        print("convert the variable")
        dispose(h$obj)
      })
    }#else{ is.num.var <- TRUE}
    if(length(.GlobalEnv$variables) > 1){
      win <- gwindow("", visible = FALSE)
      var.group <- ggroup(horizontal=TRUE, container=win)
      frm.var <- gframe("Waehle eine Variable aus!", container = var.group)
      
      radio.variable <- gradio(.GlobalEnv$variables, container = frm.var)
      addHandlerClicked(radio.variable, handler=function(h,..) {
        cat(sprintf("in select variable, you picked %s\n", svalue(h$obj)))
        .GlobalEnv$activeVar <- svalue(h$obj)
        Message(message = gettextRcmdr(paste("Die aktive Variable ist: ",
                                             svalue(h$obj), "")), type = "note")
        if(!is.numeric(get(ActiveDataSet())[,.GlobalEnv$activeVar]) &&
             !is.integer(get(ActiveDataSet())[,.GlobalEnv$activeVar])){
          Message(message = gettextRcmdr(paste("Die gewaehlte Variable ist nicht numeric! ",
                                               svalue(h$obj), "")), type = "warning")
          
          confirmDialog("Die gewaehlte Variable ist nicht numeric!", handler = function(h,...) {
            print("convert the variable")
            dispose(h$obj)
          })
        }else{
          is.num.var <- TRUE
        }
      })
      
      
      visible(win) <- TRUE
    }
  }
  return(is.num.var)
}
#selectVariable()