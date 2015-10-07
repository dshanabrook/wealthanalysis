library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(RColorBrewer)
require(reshape2)
source("source/functions.R", local=TRUE)
modelNames <<- c('m1_buyNights',"m2_buyDayReversal", "m3_reversal","m4_keepDayMomentum", "m5_momentum","m6_shortDays",'m7_someDays')
modelActions <- c( "m_bN","m_dNbD", "m_dDbN", "m_uNbD", "m_uDbN", "m_sD")		 
doDebug <<- T
useWealth <<- T
Sys.setenv(TZ='EST')
shinyServer(function(input, output, session) {
	ticker <- reactive({toupper(input$ticker)})
	name <- reactive(paste(getQuote(ticker(), what=yahooQF("Name"))[,2]))
	dateRange <- reactive({input$dateRange})
	
	tickerData <- reactive({getTickerData(ticker(), dateRange())})	
	graphType <- reactive({input$graphType})
		
	 m_buy <- reactive({ClCl(tickerData())})	
	 m_keepDays <- reactive({keepDaysF(m_buy(), input$keepDays)})
	 models <- reactive({cbind(m_buy(),m_keepDays())})
	 
output$thePlot <- renderPlot({
	if (doDebug) cat("plotting\n")
	switch(graphType(),
		"cumm" = chart.CumReturns(models(), wealth.index=useWealth, main=paste(name(),"cummulative return"), legend.loc="topleft", 
		colorset=rainbow(8)),
		"draw" = chart.Drawdown(models(), main=paste(name(),"drawdown"), legend.loc="topleft", colorset=rainbow(9)))
	})
})

