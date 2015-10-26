
distributionQuantiles <- function (nameVar) 
{    doItAndPrint(paste(nameVar))
    fVar <- get(paste(nameVar, "Distribution", sep = ""))#this concatenation return the name of the required function to be called
														#e.g. suppose that we call the function normalQuantile <- 
														#function(){distributionQuantiles("normal")}...
														#doItAndPrint(paste("fVar$params"))
  
    nnVar <- length(fVar$params)
    dialogName <- paste(nameVar, "Quantiles", sep = "")
    defaults <- list(initialValues = fVar$initialValues, tail = "lower", 
        quantiles = "")
    initial <- getDialog(dialogName, defaults = defaults)
    initializeDialog(title = gettextRcmdr(paste(fVar$titleName, 
        "Quantiles", sep = " ")))
    entryFrame <- tkframe(top)
    quantilesVar <- tclVar(initial$quantiles)
    quantilesEntry <- ttkentry(entryFrame, width = "30", textvariable = quantilesVar)
    paramsVar <- paste(fVar$params, "Var", sep = "")
    paramsEntry <- paste(fVar$params, "Entry", sep = "")
    for (i in 1:nnVar) {
        eval(parse(text = paste(paramsVar[i], "<-tclVar('", initial$initialValues[i], 
            "')", sep = "")))
        eval(parse(text = paste(paramsEntry[i], "<-ttkentry(entryFrame, width='6', textvariable=", 
            paramsVar[i], ")", sep = "")))
    }
    tailVar <- tclVar(initial$tail)
    buttonFrame <- tkframe(top)
    lowerTailButton <- ttkradiobutton(buttonFrame, variable = tailVar, 
        value = "lower")
    upperTailButton <- ttkradiobutton(buttonFrame, variable = tailVar, 
        value = "upper")
    onOK <- function() {
        nameVarF <- get(paste(nameVar, "Quantiles", sep = ""), 
            mode = "function")
        closeDialog()
        quantiles <- gsub(" +", ",", gsub(",", " ", tclvalue(quantilesVar)))
        if ("" == quantiles) {
            errorCondition(recall = nameVarF, message = gettextRcmdr("No probabilities specified."))
            return()
        }
        warn <- options(warn = -1)
        vars <- numeric(nnVar)
        for (i in 1:nnVar) {
            vars[i] <- as.numeric(tclvalue(get(paramsVar[i])))
        }
        if (length(fVar$paramsRound) > 0) {
            for (j in fVar$paramsRound) {
                vars[j] <- round(vars[j])
            }
        }
        options(warn)
        for (i in 1:length(fVar$errorConds)) {
            if (eval(parse(text = fVar$errorConds[i]))) {
                errorCondition(recall = nameVarF, message = gettextRcmdr(fVar$errorTexts[i]))
                return()
            }
        }
        tail <- tclvalue(tailVar)
        pasteVar <- ""
        for (i in 1:nnVar) {
            pasteVar <- paste(pasteVar, fVar$params[i], "=", 
                vars[i], ", ", sep = "")
        }
        if (nameVar == "Gumbel") {
            doItAndPrint(paste("log(q", fVar$funName, "(c(", 
                quantiles, "), ", pasteVar, "lower.tail=", tail == 
                  "lower", ")) # Gumbel distribution", sep = ""))
        }
        else {
            doItAndPrint(paste("q", fVar$funName, "(c(", quantiles, 
                "), ", pasteVar, "lower.tail=", tail == "lower", 
                ")", sep = ""))
        }
		#> qnorm(c(0.5), mean=0, sd=1, lower.tail=TRUE)
        putDialog(dialogName, list(initialValues = vars, tail = tclvalue(tailVar), 
            quantiles = tclvalue(quantilesVar)), resettable = FALSE)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = paste("q", fVar$funName, sep = ""), 
        reset = dialogName, apply = dialogName)
    tkgrid(labelRcmdr(entryFrame, text = gettextRcmdr("Probabilities")), 
        quantilesEntry, sticky = "w", padx = 6)
    for (i in 1:nnVar) {
        tkgrid(labelRcmdr(entryFrame, text = gettextRcmdr(fVar$paramsLabels[i])), 
            get(paramsEntry[i]), sticky = "w", padx = 6)
    }
    tkgrid(lowerTailButton, labelRcmdr(buttonFrame, text = gettextRcmdr("Lower tail")), 
        sticky = "w")
    tkgrid(upperTailButton, labelRcmdr(buttonFrame, text = gettextRcmdr("Upper tail")), 
        sticky = "w")
    tkgrid(entryFrame, sticky = "w")
    tkgrid(buttonFrame, sticky = "w")
    tkgrid.configure(quantilesEntry, sticky = "w")
    for (i in 1:nnVar) {
        tkgrid.configure(get(paramsEntry[i]), sticky = "w")
    }
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix(focus = quantilesEntry)
}
