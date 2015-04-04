#Copyright (c) 2015 david hilton shanabrook. All rights reserved. 
#library(shinyIncubator)
library(shiny)
library(ggplot2)
dow <- c("Mon","Tue", "Wed","Thu","Fri")
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
   		dateRangeInput("dateRange", "Date range:", start="2012-01-01", end=NULL, format="yyyy-mm-dd", separator="/",startview="year"),
		checkboxGroupInput("keepDays", "days to keep", dow, selected=dow, inline =T),
		radioButtons("graphType",label="",
        choices = list("cummulative return" = "cumm", "drawdown" = "draw"),selected = "cumm")),
	column(3, offset=1,
		h4("Model"),
		checkboxInput("downDayNoNight", "up day buy night",  value=F),
		checkboxInput("upDayNoNight","down day buy night",value=F),
		checkboxInput("upNightKeepDay","up night, keep day", value=F),
		checkboxInput("downNightKeepDay","down night, keep day", value=F),
		checkboxInput("sellDay","buy night, sell day",value=F)
		)
	)
))
  		 # helpText("Models 1) buy and hold, 2) hold only at night (buy close, sell open) 3) momentum, if it goes up at night, don't sell that open.  4) reversion, if it goes down at night, don't sell at open")
	    
	# h3(textOutput("caption")), 
    # h5(textOutput("subCaption")),
    # plotOutput("cummPlot"),
    # plotOutput("drawdownPlot")

