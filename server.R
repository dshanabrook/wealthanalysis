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
modelNames <<- c('m1_buyNights',"m2_buyDayReversal", "m3_buyNightReversal","m4_keepDayMomentum", "m5_buyNightMomentum","m6_buyNightsSellDays",'m7_buyNightSomeDays')
modelActions <- c( "m_bN","m_dNbD", "m_dDbN", "m_uNbD", "m_uDbN", "m_sD")
		 
		 
doDebug <<- T
useWealth <<- T
Sys.setenv(TZ='EST')
shinyServer(function(input, output, session) {
	ticker <- reactive({toupper(input$ticker)})
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
	
	clop <- reactive({ClOp(tickerData(), cost())})
	m_b <- reactive({buyAndHold(tickerData())})
	
	m_bN <-  reactive({clop()})
	
	m_bNMod <-  reactive({clop() + daysToKeep(tickerData(),input$dontSellDays)})
	m_dNbD <- reactive({if (input$dNbD)
		dNbD(tickerData(), clop())			else NULL})
	m_uDnN <- reactive({if (input$uDnN)
		uDnN(tickerData(), clop())			else NULL})		

	m_uNbD <- reactive({if (input$uNbD)
		uNbD(tickerData(),clop())			else NULL})
	m_uDbN <- reactive({if (input$uDbN)
		uDbN(tickerData(), clop())          else NULL})	
	m_bNsD <- reactive({if (input$bNsD)
		bNsD(tickerData(), clop())			else NULL})

	m2_b <- reactive({keepDaysF(m_b(), input$keepDays)})
	 m2_bN <- reactive({keepDaysF(m_bN(), input$keepDays)})
	 m2_dNbD <- reactive({keepDaysF(m_dNbD(),input$keepDays)})
	 m2_uDnN <-  reactive({keepDaysF(m_uDnN(), input$keepDays)})	
	 m2_uNbD <-  reactive({keepDaysF(m_uNbD(), input$keepDays)})	
	 m2_uDbN <-  reactive({keepDaysF(m_uDbN(), input$keepDays)})
	 m2_bNsD <-  reactive({keepDaysF(m_bNsD(), input$keepDays)})
	 
	theYlim <- reactive({getylim(input$upylim)}) 

		 models <- reactive({cbind(m_b(), m2_b(), m2_bN(), m2_dNbD(),  m2_uDnN(), m2_uNbD(), m2_uDbN(), m2_bNsD(), m_bNMod())})
#		 colnames(models) <- c('buy&hold','m1_buyNights',"m2_buyDayReversal", "m3_buyNightReversal", "m4_keepDayMomentum", "m5_buyNightMomentum", "m6_buyNightsSellDays")
		 
		 	 
#	 models <- reactive({data.frame(hold=m_bNbD(), m1_buyNight=m2_bN(), m2_downNightKeepDay=m2_uDbN(), m3_upDayNoNight= m2_dDbN(), m4_upNightKeepDay= m2_uNbD(), m5_downDayNoNight=m2_dNbD(), m6_sellDay =m2_sD())})
	 
#	 names(models()) <- c("hold","1buyNight","2downNightKeepDay", "3upDayNoNight", "4upNightKeepDay", "5downDayNoNight", "6sellDay")
	#models <- reactive({cbind(modelbN(), modelbN2())})
#	output$caption <-renderText({getCaption(period(),ticker())})
#	output$subCaption <- renderText({paste("Between: ", getDateStr(tickerData()))})
	
#to get the upper ylim use x<-par("usr"); x[4]		
output$thePlot <- renderPlot({
	switch(graphType(),
		"cumm" = chart.CumReturns(models(), wealth.index=useWealth, main="Cummulative return", legend.loc="topleft", colorset=rainbow(9), ylim=theYlim()),
		"draw" = chart.Drawdown(models(), main="Drawdown", legend.loc="topleft", colorset=rainbow(9)))
	})
})

