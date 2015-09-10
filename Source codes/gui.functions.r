modalDialog <- function(title, result, 
                        returnValOnCancel = "CANCEL") {
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg, title)
  tkgrid(tklabel(dlg, text = "       "))
  tkgrid(tklabel(dlg, text = 'RESULT PANEL'))
  
  scr <- tkscrollbar(dlg, repeatinterval=5,
                     command=function(...)tkyview(txt,...))
  txt <- tktext(dlg,bg="white",font="courier",yscrollcommand=function(...)tkset(scr,...))
  tkgrid(txt,scr)
  tkgrid.configure(scr,sticky="ew")
  
  tkinsert(txt,"end",'1. Winning Percentage Under each discount\n')
  for(i in (1:length(colnames(result$win.matrix)))){
    #tkinsert(txt,"end",i)
    tkinsert(txt,"end",paste(colnames(result$win.matrix)[i], ':', result$winning.percentage[i], "\n"))
  }
  
  tkinsert(txt,"end",'\n')
  
  tkinsert(txt,"end",'2. Mean profit under each discount\n')
  #for(i in (1:colnames(result$win.matrix)))
  for(i in (1:length(colnames(result$win.matrix)))){
    tkinsert(txt,"end",paste(colnames(result$win.matrix)[i], ':', result$profit.mean[i], "\n"))
  }
  
  #tkinsert(txt,"end",as.character(result))
  tkconfigure(txt, state="disabled")
  
  tkgrid(tklabel(dlg, text = "       "))
  
  ReturnVal <- returnValOnCancel
  
  onExport <- function() {
    ReturnVal <- 'Succeed'
    
    fileName <- tclvalue(tkgetSaveFile(initialfile = "BlockTradeAnalysis.csv",
                                       filetypes = "{{EXCEL Files} {.csv}} {{All files} *}"))
    if (!nchar(fileName)) {
      tkmessageBox(message = "No file was selected!")
    } else { 
      output <- rbind(winning.percentage = result$winning.percentage,mean.profit = result$profit.mean,result$win.matrix)
      write.csv(x = output, file = fileName, append = TRUE)
    }
    
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkconfigure(OK.but, bg='9AFF9A', text='Exported')
    tkfocus(tt)
  }
  onCancel <- function() {
    ReturnVal <- returnValOnCancel
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkconfigure(OK.but, bg='red', text='Canceled')
    tkfocus(tt)
  }
  
  Export.but <- tkbutton(dlg, text = "Export Full Result", command = onExport)
  Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
  tkgrid(Export.but, Cancel.but)
  tkgrid(tklabel(dlg, text = "    "))
  
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(tt)})
  tkbind(txt, "<Return>", onExport)
  tkwait.window(dlg)
  
  return(ReturnVal)
}

ShowResultPanel <- function()
{
  tkbind(OK.but, "<ButtonPress>", function() {tkconfigure(OK.but, text = 'LOADING...')})
  CodeVal <- tclvalue(Code)
  StartVal <- tclvalue(Start)
  EndVal <- tclvalue(End)
  AMTVal <- as.double(strsplit(tclvalue(AMT), ',')[[1]])
  ScheduleVal <- strsplit(tclvalue(Schedule), ',')[[1]]
  ImpactVal <- as.double(tclvalue(Impact))
  
  result <- blockTradeAnalysisInterval2(code = CodeVal, start.date = StartVal, end.date = EndVal,deal.amt = AMTVal,trading.schedule = ScheduleVal, impact.cost = ImpactVal)
  #result <- blockTradeAnalysisInterval2(rawData = rawData,day.data = day.data ,deal.amt = AMTVal,trading.schedule = ScheduleVal, impact.cost = ImpactVal)
  
  ReturnVal <- modalDialog("Block Trade Analysis Result", result)
  
  if (ReturnVal == "CANCEL") return()
  tkmessageBox(title = "Greeting",
               message = paste("Hello, ", ReturnVal, ".", sep = ""))
  
  #tkfocus(txt)
}

onInputAMTDestory <- function(){
  tkgrab.release(dlg); 
  tkfocus(tt);
  tkconfigure(amt.but, text = 'Change AMT');
  tkconfigure(amt.val, text = ReturnVal);
}

input.amt <- function(entryCount, entryInit, entryWidth = 20,
                        returnValOnCancel = "CANCEL") {
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg, 'Input AMTs')
  
  CodeVal <- tclvalue(Code)
  VMA5<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=5;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  VMA10<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=10;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  VMA20<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=20;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  
  tkgrid(tklabel(dlg, text = ""))
  frameVMA <- tkframe(dlg,borderwidth=3)
  tkgrid(tklabel(frameVMA, text = "       Date "), tklabel(dlg, text = paste(as.Date(Sys.Date()-1),'')), tklabel(frameVMA, text = "    "))
  tkgrid(tklabel(frameVMA, text = "       VMA5 : "), tklabel(dlg, text = as.integer(VMA5)), tklabel(frameVMA, text = "    "))
  tkgrid(tklabel(frameVMA, text = "       VMA10:  "), tklabel(dlg, text = as.integer(VMA10)),  tklabel(frameVMA, text = "    "))
  tkgrid(tklabel(frameVMA, text = "       VMA20:  "), tklabel(dlg, text = as.integer(VMA20)),  tklabel(frameVMA, text = "    "))
  tkgrid(frameVMA, sticky = 'w', columnspan= 2)
  textEntryVarTcl <<- list(length = entryCount)
  textEntryWidget <<- list(length = entryCount)
  for(i in 1: entryCount){
    textEntryVarTcl[[i]] <- tclVar(paste(entryInit))
    textEntryWidget[[i]] <- tkentry(dlg, width = paste(entryWidth),
                               textvariable =textEntryVarTcl[[i]])
    tkgrid(tklabel(dlg, text = "       "))
    tkgrid(tklabel(dlg, text = paste('AMT', i)), textEntryWidget[[i]], tklabel(dlg, text = 'Shares  '))
    tkgrid(tklabel(dlg, text = "       "))
  }
  
  ReturnVal <- returnValOnCancel
  
  onOK <- function() {
    temp <- c()
    for(i in 1: length(textEntryVarTcl)){
      amtval <- tclvalue(textEntryVarTcl[[i]])
      if(grepl('VMA5',amtval)){
        amtval <- as.double(sub(pattern = 'VMA5', replacement = '', amtval)) * VMA5
      }else if(grepl('VMA10',amtval)){
        amtval <- as.double(sub(pattern = 'VMA10', replacement = '', amtval)) * VMA10
      }else if(grepl('VMA20',amtval)){
        amtval <- as.double(sub(pattern = 'VMA20', replacement = '', amtval)) * VMA20
      }else{
        
      }
      temp <- c(temp, amtval)
    }
    ReturnVal <<- toString(temp)
    print(ReturnVal)
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  onCancel <- function() {
    ReturnVal <<- returnValOnCancel
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
  Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
  tkgrid(OK.but, Cancel.but)
  tkgrid(tklabel(dlg, text = "    "))
  
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() { 
    tkgrab.release(dlg); 
    tkfocus(tt);
    tkconfigure(amt.but, text = 'Change AMT');
    tkconfigure(amt.val, text = paste('AMT:',ReturnVal));
  })
  tkbind(textEntryWidget, "<Return>", onOK)
  tkwait.window(dlg)
  
  return(ReturnVal)
}

launchInputAMT <- function()
{
  entryCount <- length(which(strsplit(tclvalue(Schedule), ',')[[1]] == 'X')) + 1
  ReturnVal <- input.amt(entryCount, "")
  if (ReturnVal == "CANCEL"){
    AMT <- ''
  }else{
    AMT <- ReturnVal
  }
}

chedule.Reference <- function(){
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg, 'Schedule Reference')
  
  CodeVal <- tclvalue(Code)
  VMA5<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=5;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  VMA10<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=10;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  VMA20<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=20;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  
  tkgrid(tklabel(dlg, text = ""))
  textEntryVarTcl <- tclVar()
  textEntryWidget <- tkentry(dlg, width = '20', textvariable =textEntryVarTcl)
  tkgrid(tklabel(dlg, text = "       "))
  tkgrid(tklabel(dlg, text = 'Total AMT', textEntryWidget, tklabel(dlg, text = 'Shares  ')))
  tkgrid(tklabel(dlg, text = "       "))
  
  onOK <- function() {
    amtval <- as.double(tclvalue(textEntryVarTcl))
    print(amtval)
    time.5 <- amtval/VMA5
    time.10 <- amtval/VMA10
    time.20 <- amtval/VMA20
    
    ReturnVal <- paste(time.5, 'times VMA5\n',time.10, 'times VMA10\n',time.20, 'times VMA20\n')

    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  onCancel <- function() {
    ReturnVal <- "ID_CANCEL"
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
  Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
  tkgrid(OK.but, Cancel.but)
  tkgrid(tklabel(dlg, text = "    "))
  
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(tt)})
  tkbind(textEntryWidget, "<Return>", onOK)
  tkwait.window(dlg)
  
  return(ReturnVal)
}


schedule.Reference <- function(title, question, entryInit, entryWidth = 20,
                        returnValOnCancel = "ID_CANCEL") {
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg, title)
  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
                             textvariable = textEntryVarTcl)
  tkgrid(tklabel(dlg, text = "       "))
  tkgrid(tklabel(dlg, text = question), textEntryWidget)
  tkgrid(tklabel(dlg, text = "       "))
  ReturnVal <- returnValOnCancel
  
  CodeVal <- tclvalue(Code)
  VMA5<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=5;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  VMA10<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=10;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  VMA20<-w.wsd(CodeVal,"VMA",Sys.Date()-1,Sys.Date()-1,"VMA_N=20;Fill=Previous;PriceAdj=F")$Data[1,2] * 100
  
  onOK <- function() {
    total.amt <- as.integer(tclvalue(textEntryVarTcl))
    time.5 <- ceiling(total.amt/VMA5)
    time.10 <- ceiling(total.amt/VMA10)
    time.20 <- ceiling(total.amt/VMA20)
    
    ReturnVal <<- paste(time.5, 'times VMA5\n',time.10, 'times VMA10\n',time.20, 'times VMA20\n')

    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  onCancel <- function() {
    ReturnVal <<- returnValOnCancel
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
  Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
  tkgrid(OK.but, Cancel.but)
  tkgrid(tklabel(dlg, text = "    "))
  
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(tt)})
  tkbind(textEntryWidget, "<Return>", onOK)
  tkwait.window(dlg)
  
  return(ReturnVal)
}

launchReference <- function() {
  ReturnVal <- schedule.Reference("REFERENCE", "Total AMT", "")
  if (ReturnVal == "ID_CANCEL") return()
  tkmessageBox(title = "Result",
               message = ReturnVal)
}
