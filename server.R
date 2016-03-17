
rm(list=ls())
library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(RColorBrewer)#colorset=brewer.pal(9, "BrBG")
require(reshape2)
#setwd("~/shinyapps/"); runApp("wealthanalysis");setwd("wealthAnalysis")
source("source/functions.R", local=TRUE)
modelNames <<- c('m1_buyNights',"m2_buyDayReversal", "m3_reversal","m4_keepDayMomentum", "m5_momentum","m6_shortDays",'m7_someDays')
modelActions <- c( "m_bN","m_dNbD", "m_dDbN", "m_uNbD", "m_uDbN", "m_sD")		 
doDebug <<- T
useWealth <<- T
Sys.setenv(TZ='EST')
shinyServer(function(input, output, session) {
	ticker <- reactive({toupper(input$ticker)})
	name <- reactive(paste(getQuote(ticker(), what=yahooQF("Name"))[,2]))
	period <- reactive({input$period})
	yearsBack <- reactive({as.numeric(input$yearsBack)})
	dateRange <- reactive({input$dateRange})
	cost <- reactive({input$cost*2})
	
	tickerData <- reactive({
		if (input$debug)
			getTestData()
		else
			getTickerData(ticker(), dateRange())
		})	
	graphType <- reactive({input$graphType})
	nights <- reactive({ClOp(tickerData(), cost())})
	days <- reactive({OpCl(tickerData())})
	
	m_b <- reactive({ClCl2(tickerData())})	
	m_bN <-  reactive({if (input$bN)
		nights()			else NULL})
	m_bNSomeD <- reactive({if (!is.null(input$dontSellDays))
	  daysToKeep(days(),nights(),input$dontSellDays)   else NULL})
	m_dNbD <- reactive({if (input$dNbD)
		dNbD(days(), nights())			else NULL})
	m_uDnN <- reactive({if (input$uDnN)
		uDnN(tickerData(), nights())			else NULL})		
	m_uNbD <- reactive({if (input$uNbD)
		uNbD(days(),nights())			else NULL})
	m_uDbN <- reactive({if (input$uDbN)
		uDbN(tickerData(), nights())          else NULL})	
	m_bNsD <- reactive({if (input$bNsD)
		bNsD(days(), nights())			else NULL})

	 models2 <- reactive({cbind(m_b(), m_bN(), m_dNbD(),  m_uDnN(), m_uNbD(), m_uDbN(), m_bNsD())})
	 models2b <- reactive({keepDaysF(models2(), input$keepDays)})
	 models <- reactive({cbind(m_b(),models2b(),m_bNSomeD())})
	 
	theYlim <- reactive({getylim(input$upylim)}) 

#to get the upper ylim use x<-par("usr"); x[4]	
#output$rawData <- renderTable({
	#indexFormat(models()) <- "%d%a"
#	models()*100
#})	
output$thePlot <- renderPlot({
	gc()
	switch(graphType(),
		"cumm" = chart.CumReturns(models(), wealth.index=useWealth, main=paste(name(),"Cummulative return"), legend.loc="topleft", colorset=rainbow(8), ylim=theYlim()),
		"draw" = chart.Drawdown(models(), main=paste(name(),"Drawdown"), legend.loc="topleft", colorset=rainbow(9)))
	})
})

