#Copyright (c) 2015 david hilton shanabrook. All rights reserved. 
#library(shinyIncubator)
library(shiny)
library(ggplot2)
dow <<- c("Mon","Tue", "Wed","Thu","Fri")
shinyUI(fluidPage(
	title = "Trade Modeling",
	fluidRow(
	hr(),
	plotOutput("thePlot"),
	column(3,
		h4("Stock"),
		textInput("ticker","Stock symbol:", value="^GSPC")),	
    column(4, offset=1,
    	h4("Time Period"),
   		dateRangeInput("dateRange", "Date range:", start="2000-01-01", end=NULL, format="yyyy-mm-dd", separator="/",startview="year"),
		checkboxGroupInput("keepDays", "Hold these days only", dow, selected=dow, inline =T)),
		column(3,

		radioButtons("graphType",label="",
        choices = list("cummulative return" = "cumm", "drawdown" = "draw"),selected = "cumm"))
	)
))
