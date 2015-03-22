#cef analysis 
#use abbyy finereader to get the data from the pdf.  specify the table and output.  should work ok
rm(list=ls())
library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
require(reshape2)
#setwd("~/ShinyApps/periodAnalysis")
source("source/functions.R")

doDebug <<- F
useWealth <- T
Sys.setenv(TZ='EST')
shinyServer(function(input, output, session) {
	ticker <- reactive({toupper(input$ticker)})
	period <- reactive({input$period})
	yearsBack <- reactive({as.numeric(input$yearsBack)})
	dateRange <- reactive({input$dateRange})
	tickerData <- reactive({getTickerData(ticker(), dateRange())})	
	clcl <- reactive({ROCCl(tickerData())})
	clcl2 <- reactive({ClCl(tickerData())})
	clop <- reactive({ClOp(tickerData())})
	opcl <- reactive({OpCl(tickerData())})	
	longThenShort <- reactive({shortLong(tickerData())})
	output$caption <-renderText({getCaption(period(),ticker())})
	output$subCaption <- renderText({paste("Between: ",getDateStr(tickerData()))})
	
output$drawdownPlot <- renderPlot({
	chart.Drawdown(cbind(clcl(),clop(),longThenShort()), main="Drawdown", legend.loc="topleft", colorset=c(8,1,3))
	})

			
output$cummPlot <- renderPlot({
	chart.CumReturns(cbind(clcl(),clop(), longThenShort()),
	wealth.index=useWealth, main="Cummulative return", legend.loc="topleft", colorset=c(8,1,3,4))
	})
	


})
