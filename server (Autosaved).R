#cef analysis 
#use abbyy finereader to get the data from the pdf.  specify the table and output.  should work ok
rm(list=ls())
library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
require(reshape2)
#setwd("~/ShinyApps/periodAnalysis")
source("source/functions.R", local=TRUE)

doDebug <<- T
useWealth <<- T
Sys.setenv(TZ='EST')
shinyServer(function(input, output, session) {
	ticker <- reactive({toupper(input$ticker)})
	period <- reactive({input$period})
	yearsBack <- reactive({as.numeric(input$yearsBack)})
	dateRange <- reactive({input$dateRange})
#	keepDays <- reactive({input$keepDays})
	cost <- reactive({input$cost*2})
	tickerData <- reactive({getTickerData(ticker(), dateRange())})	
	clcl <- reactive({ROCCl(tickerData())})
#	clcl2 <- reactive({ClCl(tickerData())})
	clop <- reactive({ClOp(tickerData(), cost())})
	opcl <- reactive({OpCl(tickerData(), cost())})
	theYLim <- reactive({input$ylimFix})

	model <- reactive({
	if (input$downDayNoNight)
		downDayNoNightF(tickerData(), clop())
	else if (input$upDayNoNight)
		upDayNoNightF(tickerData(), clop())
	else if (input$upNightKeepDay)
		upNightKeepDayF(tickerData(),clop())
	else if (input$downNightKeepDay)
		downNightKeepDayF(tickerData(), clop())
	else if (input$sellDay)
		ClOpLongOpClShort(tickerData())
	else clop()})
	

	modelB <- reactive({keepDaysF(model(),input$keepDays)})
	clopB <-  reactive({keepDaysF(clop(), input$keepDays)})	
	clclB <-  reactive({keepDaysF(clcl(), input$keepDays)})	
	 theYlim <- reactive({getylim(input$upylim)}) 	
	 
	output$caption <-renderText({getCaption(period(),ticker())})
	output$subCaption <- renderText({paste("Between: ",getDateStr(tickerData()))})
	
#to get the upper ylim use x<-par("usr"); x[4]
output$drawdownPlot <- renderPlot({
	chart.Drawdown(cbind(clcl(),clopB(),modelB()), main="Drawdown", legend.loc="topleft", colorset=c(8,1,3), ylim=theYlim() )})	
		
output$cummPlot <- renderPlot({
	chart.CumReturns(cbind(clclB(),clopB(), modelB()), wealth.index=useWealth, main="Cummulative return", legend.loc="topleft", colorset=c(8,1,3), ylim=theYlim())
	})
})

