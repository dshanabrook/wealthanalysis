#Copyright (c) 2013 david hilton shanabrook. All rights reserved. 
#library(shinyIncubator)
	
models <- c("Momentum: good night, hold day"="mom","Reversion:bad night, hold day"="revert","short day"="short")
shinyUI(
  pageWithSidebar(
    headerPanel("Wealth index"),  
    sidebarPanel(  
    textInput("ticker","Stock symbol:", value="AAPL"),	
    textInput("cost", "trade cost per share $", value=0.007),	
 #   textInput("yearsBack","Number of years in analysis:", value='3' ), 
#    dateRangeInput("dates", label = h3("Date range")),
    dateRangeInput("dateRange", "Date range:",
               start = "2012-01-01",end=NULL, format="yyyy-mm-dd", separator="/",startview="year"),
    selectInput("model", "Model:", models),
    helpText("Models 1) buy and hold, 2) hold only at night (buy close, sell open) 3) momentum, if it goes up at night, don't sell that open.  4) reversion, if it goes down at night, don't sell at open"),
    submitButton("Plot Now")
    ),
	mainPanel(
	h3(textOutput("caption")), 
    h5(textOutput("subCaption")),
    plotOutput("cummPlot"),
    plotOutput("drawdownPlot")
 	)
 ))
