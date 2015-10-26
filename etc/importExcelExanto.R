importExcelExanto <- function () 
{
  Library("XLConnect")
  initializeDialog(title = gettextRcmdr("Import Excel Data Set"))
  dsname <- tclVar(gettextRcmdr("Dataset"))
  dsnameFrame <- tkframe(top)
  entryDsname <- ttkentry(dsnameFrame, width = "35", textvariable = dsname)
  checkBoxFrame <- tkframe(top)
  variableNames <- tclVar("1")
  variableNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable = variableNames)
  rowNames <- tclVar("0")
  rowNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable = rowNames)
  onOK <- function() {
    closeDialog()
    setBusyCursor()
    on.exit(setIdleCursor())
    dsnameValue <- trim.blanks(tclvalue(dsname))
    variableNamesValue <- tclvalue(variableNames)
    rowNamesValue <- tclvalue(rowNames)
    if (dsnameValue == "") {
      errorCondition(recall = importExcel, message = gettextRcmdr("You must enter the name of a data set."))
      return()
    }
    if (!is.valid.name(dsnameValue)) {
      errorCondition(recall = importExcel, message = paste("\"", 
                                                           dsnameValue, "\" ", gettextRcmdr("is not a valid name."), 
                                                           sep = ""))
      return()
    }
    if (is.element(dsnameValue, listDataSets())) {
      if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))) {
        importExcel()
        return()
      }
    }
    File <- tclvalue(tkgetOpenFile(filetypes = gettextRcmdr("{\"All Files\" {\"*\"}} {\"MS Excel 2007 file\" {\".xlsx\" \".XLSX\"}} {\"MS Excel file\" {\".xls\" \".XLS\"}}"), 
                                   parent = CommanderWindow()))
    if (File == "") {
      tkfocus(CommanderWindow())
      return()
    }
    command <- paste("loadWorkbook(\"", File, "\")", sep = "")
    doItAndPrint(paste(".Workbook <- ", command, sep = ""))
    worksheets <- getSheets(.Workbook)
    if (length(worksheets) > 1) 
      worksheet <- tk_select.list(worksheets, title = gettextRcmdr("Select one table"))
    else worksheet <- worksheets
    if (worksheet == "") {
      errorCondition(message = gettextRcmdr("No table selected"))
      return()
    }
    command <- paste("readWorksheet(XLConnect::loadWorkbook(\"",File, "\"),sheet = 1, header=", if (variableNamesValue == "1") 
                       "TRUE"
                     else "FALSE", ", rownames=", if (rowNamesValue == 
                                                      "1") 
                       "1"
                     else "NULL", ")", sep = "")
    logger(paste(dsnameValue, " <- ", command, sep = ""))
    result <- justDoIt(command)
    print(result)
    if (class(result)[1] != "try-error") {
      gassign(dsnameValue, result)
    }
    logger("remove(.Workbook)")
    justDoIt("remove(.Workbook, envir=.GlobalEnv)")
    if (class(result)[1] != "try-error") {
      factors <- sapply(get(dsnameValue, envir = .GlobalEnv), 
                        is.character)
      if (any(factors)) {
        factors <- which(factors)
        command <- paste(dsnameValue, "[, c(", paste(factors, 
                                                     collapse = ", "), ")] <- lapply(", dsnameValue, 
                         "[, c(", paste(factors, collapse = ", "), "), drop=FALSE], as.factor)", 
                         sep = "")
        doItAndPrint(command)
      }
      activeDataSet(dsnameValue)
    }
  }
  OKCancelHelp(helpSubject = "readWorksheet")
  tkgrid(labelRcmdr(dsnameFrame, text = gettextRcmdr("Enter name of data set: ")), 
         entryDsname, sticky = "w")
  tkgrid(dsnameFrame, sticky = "w")
  tkgrid(variableNamesCheckBox, labelRcmdr(checkBoxFrame, text = gettextRcmdr("Variable names in first row of spreadsheet")), 
         sticky = "w")
  tkgrid(rowNamesCheckBox, labelRcmdr(checkBoxFrame, text = gettextRcmdr("Zeilennamen in erster Spalte des Spreadsheet")), 
         sticky = "w")
  tkgrid(checkBoxFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix(focus = entryDsname)
}