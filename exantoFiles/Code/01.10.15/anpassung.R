
paramAnpassung <- function(){
  
  if(is.null(ActiveDataSet())){
    Message(message = gettextRcmdr("There is no active data set."), 
            type = "error")
    confirmDialog("keine Datensatz!!", handler = function(h,...) {
      dispose(h$obj)
    })
    invisible(TRUE)
    
  }else{

    .GlobalEnv$aev.MM <- FALSE
    .GlobalEnv$aev.LM <- FALSE
    .GlobalEnv$aev.ML <- FALSE
    # set the the default adjusted parameters for AEV
    
    .GlobalEnv$weibull.MM <- FALSE
    .GlobalEnv$weibull.LM <- FALSE
    .GlobalEnv$weibull.ML <- FALSE
    #set the default parameters for weibull
    
    .GlobalEnv$logweibull.MM <- FALSE
    .GlobalEnv$logweibull.LM <- FALSE
    .GlobalEnv$logweibull.ML <- FALSE
    # set the the default adjusted parameters for logweibull
    
    .GlobalEnv$gumbel.MM <- FALSE
    .GlobalEnv$gumbel.LM <- FALSE
    .GlobalEnv$gumbel.ML <- FALSE
    # set the the default adjusted parameters for gumbel
    
    .GlobalEnv$epxponential.MM <- FALSE
    .GlobalEnv$epxponential.LM <- FALSE
    .GlobalEnv$epxponential.ML <- FALSE
    # set the the default adjusted parameters for epxponential
    
    .GlobalEnv$pers.MM <- FALSE
    .GlobalEnv$pers.MM.LM <- FALSE
    .GlobalEnv$pers.ML <- FALSE
    # set the the default adjusted parameters for Person
    
    .GlobalEnv$logPers.MM <- FALSE
    .GlobalEnv$logPers.LM <- FALSE
    .GlobalEnv$logPers.ML <- FALSE
    # set the the default adjusted parameters for log-Person
    
    .GlobalEnv$gamma.MM <- FALSE
    .GlobalEnv$gamma.LM <- FALSE
    .GlobalEnv$gamma.ML <- FALSE
    # set the the default adjusted parameters for gamma
    
    .GlobalEnv$frechet.MM <- FALSE
    .GlobalEnv$frechet.LM <- FALSE
    .GlobalEnv$frechet.ML <- FALSE
    # set the the default adjusted parameters for frechet
    
    .GlobalEnv$normal.MM <- FALSE
    .GlobalEnv$normal.LM <- FALSE
    .GlobalEnv$normal.ML <- FALSE
    # set the the default adjusted parameters for normal
    
    .GlobalEnv$logNormal.MM <- FALSE
    .GlobalEnv$logNormal.LM <- FALSE
    .GlobalEnv$logNormal.ML <- FALSE
    # set the the default adjusted parameters for log-normal
    
    .GlobalEnv$log3PNormal.MM <- FALSE
    .GlobalEnv$log3PNormal.LM <- FALSE
    .GlobalEnv$log3PNormal.ML <- FALSE
    # set the the default adjusted parameters for log-3P-normal
    
    .GlobalEnv$axes.skal.ln <- TRUE
    .GlobalEnv$non.ex.prob <- TRUE
    
    is.num.var <- selectVariable()
    if(is.num.var){
      print(is.num.var)
    dialogAnpassung()
    }
  }
}
