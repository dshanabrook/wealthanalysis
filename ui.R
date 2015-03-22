#Copyright (c) 2013 david hilton shanabrook. All rights reserved. 
#library(shinyIncubator)
	

shinyUI(
  pageWithSidebar(
    headerPanel("Wealth index"),  
    sidebarPanel(  
    textInput("ticker","Stock symbol:", value="AAPL"),		
 #   textInput("yearsBack","Number of years in analysis:", value='3' ), 
#    dateRangeInput("dates", label = h3("Date range")),
    dateRangeInput("dateRange", "Date range:",
               start = "2012-01-01",end=NULL, format="yyyy-mm-dd", separator="/",startview="year"),
    helpText("Set the ticker symbol, number of years back to analyze, and whether to analyze by day or week.  After hours trading: change from previous day close to open."),
    submitButton("Plot Now")
    ),
	mainPanel(
	h3(textOutput("caption")), 
    h5(textOutput("subCaption")),
    plotOutput("cummPlot"),
    plotOutput("drawdownPlot")
 	)
 ))
