#Copyright (c) 2015 david hilton shanabrook. All rights reserved. 
#library(shinyIncubator)
library(shiny)
library(ggplot2)
dow <<- c("Mon","Tue", "Wed","Thu","Fri")
shinyUI(fluidPage(
	title = "Trade Modeling",

	fluidRow(
	hr(),
#	column(6,
		# h3(textOutput("caption"))
		# ),
	# column(6,
		# h5(textOutput("subCaption"))
		# ),
	plotOutput("thePlot"),
	column(3,
		h4("Stock"),
		textInput("ticker","Stock symbol:", value="AAPL"),	
   		numericInput("upylim","Fix upper y axis to:", value=NA),
   		numericInput("cost", "trade cost per share $", value=0.007)
   		),
    column(4, offset=1,
    	h4("Time Period"),
   		dateRangeInput("dateRange", "Date range:", start="2015-03-11", end=NULL, format="yyyy-mm-dd", separator="/",startview="year"),
		checkboxGroupInput("keepDays", "days to keep", dow, selected=dow, inline =T),
		checkboxGroupInput("dontSellDays", "days don't sell on open", dow, selected=dow, inline =F),
		radioButtons("graphType",label="",
        choices = list("cummulative return" = "cumm", "drawdown" = "draw"),selected = "cumm")),
	column(3, offset=1,
		h4("Models"),
		checkboxInput("bN","1 nights", value=T),
		h6("reversal models"),
		checkboxInput("dNbD","2 hold days after bad nights", value=F),
		checkboxInput("uDnN","3 nights only after bad days",value=F),
		h6("momentum models"),
		checkboxInput("uNbD","4 hold days after good nights", value=F),
		checkboxInput("uDbN", "5 nights only after good days",  value=F),
		h6("short"),
		checkboxInput("bNsD","6 short days",value=F),
		h6("debug"),
		checkboxInput("debug","debug", value=F)
		)
	)
))
  		 # helpText("Models 1) buy and hold, 2) hold only at night (buy close, sell open) 3) momentum, if it goes up at night, don't sell that open.  4) reversion, if it goes down at night, don't sell at open")
	    
	# h3(textOutput("caption")), 
    # h5(textOutput("subCaption")),
    # plotOutput("cummPlot"),
    # plotOutput("drawdownPlot")

