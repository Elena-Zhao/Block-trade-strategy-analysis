###########################
#ATTENTION:Make sure to change the 'Path' in the following two lines (source...) to the local path in your computer where the R files are stored!

###########################
source('D:/Zhao Qing/blocktrade/gui.functions.r')
source('D:/Zhao Qing/blocktrade/blockTradeAnalysis.R')

require(tcltk)
require(WindR)
options(digits = 3)

w.start()

tt<-tktoplevel()
tkwm.title(tt,"Block Trade Analysis")

frameOverall <- tkframe(tt,relief="groove",borderwidth=5)

fontHeading <- tkfont.create(family="times",size=20,weight="bold",slant="italic")
fontItem <- tkfont.create(size=11,slant="italic")

tkgrid(tklabel(tt,text=""))
tkgrid(tklabel(tt,text="  Block Trade Analysis Tool",font=fontHeading))
tkgrid(tklabel(tt,text=""))
tkgrid(tklabel(frameOverall,text=""))

Code <- tclVar("000725.SZ")
entry.Code <-tkentry(frameOverall,width="20",textvariable=Code)
tkgrid(tklabel(frameOverall,text="Code",font=fontItem), entry.Code,tklabel(frameOverall,text="           "))
tkgrid(tklabel(frameOverall,text=""))

Start <- tclVar("2015-1-1")
entry.Start <-tkentry(frameOverall,width="20",textvariable=Start)
tkgrid(tklabel(frameOverall,text="Start Date",font=fontItem), entry.Start)
tkgrid(tklabel(frameOverall,text=""), tklabel(frameOverall,text="e.g. 2015-2-1"),sticky = 'w')
tkgrid(tklabel(frameOverall,text=""))

End <- tclVar("2015-7-1")
entry.End <-tkentry(frameOverall,width="20",textvariable=End)
tkgrid(tklabel(frameOverall,text="End Date",font=fontItem), entry.End)
tkgrid(tklabel(frameOverall,text=""), tklabel(frameOverall,text="e.g. 2015-5-1"),sticky = 'w')
tkgrid(tklabel(frameOverall,text=""))

Schedule <- tclVar("T,T,T,X,T,T,T")
schedule.but <-tkbutton(frameOverall,text=" Ref ", bg = '#6B6B6B', command= launchReference)
entry.Schedule <-tkentry(frameOverall,width="20",textvariable=Schedule)
tkgrid(tklabel(frameOverall,text="Trading Schedule",font=fontItem), entry.Schedule, schedule.but)
tkgrid(tklabel(frameOverall,text=""), tklabel(frameOverall,text="e.g. T,T,X,T,T"), sticky = 'w')
tkgrid(tklabel(frameOverall,text=""))

#tkbind(entry.Code, "<Return>",OnOK)
#tkgrid(tklabel(frameOverall,text=""),amt.but,sticky = 'we')
#tkgrid(tklabel(frameOverall,text=""))

AMT <- tclVar("400000000,300000000")
amt.but <-tkbutton(frameOverall,text="   Set AMT   ", bg = '#6B6B6B', command= launchInputAMT)
tkgrid(tklabel(frameOverall,text="AMT",font=fontItem), amt.but)
amt.val <- tklabel(frameOverall,text="")
tkgrid(tklabel(frameOverall,text=""), amt.val,sticky = 'w')
tkgrid(tklabel(frameOverall,text=""))

Impact <- tclVar("0.97")
entry.Impact <-tkentry(frameOverall,width="20",textvariable=Impact)
tkgrid(tklabel(frameOverall,text="Impact Cost",font=fontItem), entry.Impact)
tkgrid(tklabel(frameOverall,text=""), tklabel(frameOverall,text="0.9 < IF < 1"), sticky = 'w')
tkgrid(tklabel(frameOverall,text=""))

OK.but <-tkbutton(frameOverall,text="   OK   ", bg = '#54FF9F', command = ShowResultPanel)
#tkbind(OK.but, "<ButtonPress>", function() {tkconfigure(OK.but, text = 'LOADING...')})
tkgrid(tklabel(frameOverall,text=""),OK.but,sticky = 'we')
tkgrid(tklabel(frameOverall,text=""))

tkgrid(tklabel(frameOverall,text=""))
tkgrid(tklabel(frameOverall,text="Copyright @ 2015 DONGXING SECURITIES. All Rights Reserved"),columnspan=2)
tkgrid(tklabel(frameOverall,text=""))

tkgrid(frameOverall)


tkfocus(tt)

