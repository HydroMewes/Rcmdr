options(warn = -1)
dialogAnpassung <- function()
{
  library(gWidgetstcltk)
  options(guiToolkit = "tcltk")
  options(warn = -1) 
  
  win <- gwindow("Statistische Analyse", visible = FALSE)
  AEV.Weibull <- ggroup(horizontal=TRUE, container=win)
  
  frm.AEV <- gframe("AEV", container = AEV.Weibull)
  .GlobalEnv$check.AEV.MM <- gcheckbox("MM", container = frm.AEV) 
  .GlobalEnv$check.AEV.LM <- gcheckbox("LM", container = frm.AEV)
  .GlobalEnv$check.AEV.ML <- gcheckbox("ML", container = frm.AEV)
  frm.Weibull <- gframe("Weibull", container = AEV.Weibull)
  .GlobalEnv$check.Weibull.MM <- gcheckbox("MM", container = frm.Weibull) 
  .GlobalEnv$check.Weibull.LM <- gcheckbox("LM", container = frm.Weibull)
  .GlobalEnv$check.Weibull.ML <- gcheckbox("ML", container = frm.Weibull)
  
  logW.gumbel <- ggroup(horizontal=TRUE, container=win)
  frm.logW <- gframe("logWeibull", container = logW.gumbel)
  .GlobalEnv$check.logW.MM <- gcheckbox("MM", container = frm.logW) 
  .GlobalEnv$check.logW.LM <- gcheckbox("LM", container = frm.logW)
  .GlobalEnv$check.logW.ML <- gcheckbox("ML", container = frm.logW)
  frm.gumbel <- gframe("Gumbel Verteilung", container = logW.gumbel)
  .GlobalEnv$check.gumbel.MM <- gcheckbox("MM", container = frm.gumbel) 
  .GlobalEnv$check.gumbel.LM <- gcheckbox("LM", container = frm.gumbel)
  .GlobalEnv$check.gumbel.ML <- gcheckbox("ML", container = frm.gumbel)
  
  expo.pears <- ggroup(horizontal=TRUE, container=win)
  frm.expo <- gframe("ExponentialVerteilung", container = expo.pears)
  .GlobalEnv$check.expo.MM <- gcheckbox("MM", container = frm.expo) 
  .GlobalEnv$check.expo.LM <- gcheckbox("LM", container = frm.expo)
  .GlobalEnv$check.expo.ML <- gcheckbox("ML", container = frm.expo)
  frm.pears <- gframe("Pearson 3-Verteilung", container = expo.pears)
  .GlobalEnv$check.pears.MM <- gcheckbox("MM", container = frm.pears) 
  .GlobalEnv$check.pears.LM <- gcheckbox("LM", container = frm.pears)
  .GlobalEnv$check.pears.ML <- gcheckbox("ML", container = frm.pears)
 
  
  logP.gamma <- ggroup(horizontal=TRUE, container=win)
  frm.logP <- gframe("logPearson 3-Verteil.", container = logP.gamma)
  .GlobalEnv$check.logP.MM <- gcheckbox("MM", container = frm.logP) 
  .GlobalEnv$check.logP.LM <- gcheckbox("LM", container = frm.logP)
  .GlobalEnv$check.logP.ML <- gcheckbox("ML", container = frm.logP)
  frm.gamma <- gframe("Gamma-Verteilung", container = logP.gamma)
  .GlobalEnv$check.gamma.MM <- gcheckbox("MM", container = frm.gamma) 
  .GlobalEnv$check.gamma.LM <- gcheckbox("LM", container = frm.gamma)
  .GlobalEnv$check.gamma.ML <- gcheckbox("ML", container = frm.gamma)
  
  
  frech.norm <- ggroup(horizontal=TRUE, container=win)
  frm.frech <- gframe("Frechet Verteilung", container = frech.norm)
  .GlobalEnv$check.frech.MM <- gcheckbox("MM", container = frm.frech) 
  .GlobalEnv$check.frech.LM <- gcheckbox("LM", container = frm.frech)
  .GlobalEnv$check.frech.ML <- gcheckbox("ML", container = frm.frech)
  frm.normal <- gframe("Normal-Verteilung", container = frech.norm)
  .GlobalEnv$check.normal.MM <- gcheckbox("MM", container = frm.normal) 
  .GlobalEnv$check.normal.LM <- gcheckbox("LM", container = frm.normal)
  .GlobalEnv$check.normal.ML <- gcheckbox("ML", container = frm.normal)
  
  logN.log3PN <- ggroup(horizontal=TRUE, container=win)
  frm.logN <- gframe("log-Normal-Vert.", container = logN.log3PN)
  .GlobalEnv$check.logN.MM <- gcheckbox("MM", container = frm.logN) 
  .GlobalEnv$check.logN.LM <- gcheckbox("LM", container = frm.logN)
  .GlobalEnv$check.logN.ML <- gcheckbox("ML", container = frm.logN)
  frm.log3PN <- gframe("log-3P-Normal-Vert.", container = logN.log3PN)
  .GlobalEnv$check.log3PN.MM <- gcheckbox("MM", container = frm.log3PN) 
  .GlobalEnv$check.log3PN.LM <- gcheckbox("LM", container = frm.log3PN)
  .GlobalEnv$check.log3PN.ML <- gcheckbox("ML", container = frm.log3PN)
  
  achsenskal.auswertung <- ggroup(horizontal=TRUE, container=win)
  frm.achsenskal <- gframe("Achsenskalierung", container = achsenskal.auswertung)
  achsenskal <- c("yT=ln(ln(T/(T-1)))", "Non-Exceedance Prob")
  .GlobalEnv$radio.btn.achsenskal <- gradio(achsenskal, container = frm.achsenskal)
  addHandlerClicked(radio.btn.achsenskal, handler=function(h,..) {
    cat(sprintf("You picked %s\n", 2))
  })
  
  frm.auswertung <- gframe("Auswertung", container = achsenskal.auswertung,
                           horizpntal = FALSE)
  
  
  btnAuswet <- gbutton("Auswertung", container = frm.auswertung,
                     handler = function(h, ...)
                     {
                       
                       .GlobalEnv$at.least.one.vert <- isSelectedMethod()
                       #  isSelectedMethod() tests, mhether at least one Method had been
                       #  selected
                       if(!.GlobalEnv$at.least.one.vert){
                         errorDialog("Waehlen Sie eine Methode!", handler = function(h,...) {
                           dispose(h$obj)
                         })
                       }# test wether at least one distribution was been selected
                       else{
                         # cas of: at least one method had been choosed
                         plotInWindow(fPlot = fitIt)
                       }
                       
                     }
  )

  btnClear <- gbutton("Clear", container = frm.auswertung,
                       handler = function(h, ...)
                       {
                         svalue(.GlobalEnv$check.AEV.MM) <- FALSE
                         svalue(.GlobalEnv$check.AEV.LM) <- FALSE
                         svalue(.GlobalEnv$check.AEV.ML) <- FALSE
                         svalue(.GlobalEnv$check.Weibull.MM) <- FALSE
                         svalue(.GlobalEnv$check.Weibull.LM) <- FALSE
                         svalue(.GlobalEnv$check.Weibull.ML) <- FALSE
                         svalue(.GlobalEnv$check.logW.MM) <- FALSE
                         svalue(.GlobalEnv$check.logW.LM) <- FALSE
                         svalue(.GlobalEnv$check.logW.ML) <- FALSE
                         svalue(.GlobalEnv$check.gumbel.MM) <- FALSE 
                         svalue(.GlobalEnv$check.gumbel.LM) <- FALSE
                         svalue(.GlobalEnv$check.gumbel.ML) <- FALSE
                         svalue(.GlobalEnv$check.expo.MM) <- FALSE 
                         svalue(.GlobalEnv$check.expo.LM) <- FALSE
                         svalue(.GlobalEnv$check.expo.ML) <- FALSE
                         svalue(.GlobalEnv$check.pears.MM) <- FALSE 
                         svalue(.GlobalEnv$check.pears.LM) <- FALSE
                         svalue(.GlobalEnv$check.pears.ML) <- FALSE
                         svalue(.GlobalEnv$check.logP.MM) <- FALSE
                         svalue(.GlobalEnv$check.logP.LM) <- FALSE
                         svalue(.GlobalEnv$check.logP.ML) <- FALSE
                         svalue(.GlobalEnv$check.gamma.MM) <- FALSE 
                         svalue(.GlobalEnv$check.gamma.LM) <- FALSE
                         svalue(.GlobalEnv$check.gamma.ML) <- FALSE
                         svalue(.GlobalEnv$check.frech.MM) <- FALSE 
                         svalue(.GlobalEnv$check.frech.LM) <- FALSE
                         svalue(.GlobalEnv$check.frech.ML) <- FALSE
                         svalue(.GlobalEnv$check.normal.MM) <- FALSE 
                         svalue(.GlobalEnv$check.normal.LM) <- FALSE
                         svalue(.GlobalEnv$check.normal.ML) <- FALSE
                         svalue(.GlobalEnv$check.logN.MM) <- FALSE 
                         svalue(.GlobalEnv$check.logN.LM) <- FALSE
                         svalue(.GlobalEnv$check.logN.ML) <- FALSE
                         svalue(.GlobalEnv$check.log3PN.MM) <- FALSE 
                         svalue(.GlobalEnv$check.log3PN.LM) <- FALSE
                         svalue(.GlobalEnv$check.log3PN.ML) <- FALSE
                         
                         
                         
                       }
  )
  
  visible(win) <- TRUE
  
}

#dialogAnpassung()
isSelectedMethod <- function(){
svalue(.GlobalEnv$check.AEV.MM) ||
  svalue(.GlobalEnv$check.AEV.LM) ||
  svalue(.GlobalEnv$check.AEV.ML) ||
  svalue(.GlobalEnv$check.Weibull.MM) ||
  svalue(.GlobalEnv$check.Weibull.LM) ||
  svalue(.GlobalEnv$check.Weibull.ML) ||
  svalue(.GlobalEnv$check.logW.MM) ||
  svalue(.GlobalEnv$check.logW.LM) ||
  svalue(.GlobalEnv$check.logW.ML) ||
  svalue(.GlobalEnv$check.gumbel.MM) || 
  svalue(.GlobalEnv$check.gumbel.LM) ||
  svalue(.GlobalEnv$check.gumbel.ML) ||
  svalue(.GlobalEnv$check.expo.MM) || 
  svalue(.GlobalEnv$check.expo.LM) ||
  svalue(.GlobalEnv$check.expo.ML) ||
  svalue(.GlobalEnv$check.pears.MM) || 
  svalue(.GlobalEnv$check.pears.LM) ||
  svalue(.GlobalEnv$check.pears.ML) ||
  svalue(.GlobalEnv$check.logP.MM) ||
  svalue(.GlobalEnv$check.logP.LM) ||
  svalue(.GlobalEnv$check.logP.ML) ||
  svalue(.GlobalEnv$check.gamma.MM) || 
  svalue(.GlobalEnv$check.gamma.LM) ||
  svalue(.GlobalEnv$check.gamma.ML) ||
  svalue(.GlobalEnv$check.frech.MM) || 
  svalue(.GlobalEnv$check.frech.LM) ||
  svalue(.GlobalEnv$check.frech.ML) ||
  svalue(.GlobalEnv$check.normal.MM) || 
  svalue(.GlobalEnv$check.normal.LM) ||
  svalue(.GlobalEnv$check.normal.ML) ||
  svalue(.GlobalEnv$check.logN.MM) || 
  svalue(.GlobalEnv$check.logN.LM) ||
  svalue(.GlobalEnv$check.logN.ML) ||
  svalue(.GlobalEnv$check.log3PN.MM) || 
  svalue(.GlobalEnv$check.log3PN.LM) ||
  svalue(.GlobalEnv$check.log3PN.ML)
}