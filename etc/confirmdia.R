confirmDialog <- function(message, handler=NULL, parent = NULL) {
  library(gWidgetstcltk)
  options(guiToolkit = "tcltk")
  options(warn = -1) 
  window <- gwindow("Confirm")
  group <- ggroup(container = window, parent = parent)
  gimage("info", dirname="stock", size="dialog", container=group)
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group, expand=TRUE)
  
    ## A group to organize the buttons
    button.group <- ggroup(container = inner.group)
  ## Push buttons to right
    addSpring(button.group)
  gbutton("ok", handler=handler, container=button.group)
  gbutton("cancel", handler = function(h,...) dispose(window),
            container=button.group)
  
   invisible(TRUE)
}

