
rm(list=ls())
library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
require(reshape2)
setwd("~/ShinyApps/wealthAnalysis")
source("../periodAnalysis/source/functions.R")
mon<-1;tue<-2;wed<-3;thu<-4;fri<-5
doDebug <<- F
Sys.setenv(TZ='')
ticker <- "PST"
getSymbols(ticker, src="yahoo", from="2013-01-01",to="2014-02-03")
tickerOHLC <- get(ticker)
tickerOHLC <- adjustOHLC(tickerOHLC, symbol.name=ticker)
tickerClOp <- ClOp(tickerOHLC)
data <- tickerClOp
dow <- c(1,2,3,4,5)
days <- NULL
for (i in dow){
	days <- append(days,i)
	print(days)
	return <- Return.cumulative(keepWDay(tickerClOp, days)) 
	print(return)
}
Return.cumulative(keepDays(tickerClOp, c(mon)))
Return.cumulative(keepDays(tickerClOp, c(mon,tue)))
Return.cumulative(keepDays(tickerClOp, c(wed,thu,fri)))
Return.cumulative(keepDays(tickerClOp, c(mon,tue,wed)))
Return.cumulative(keepDays(tickerClOp, c(mon,tue,wed,thu)))
Return.cumulative(keepDays(tickerClOp, c(mon,tue,wed, thu, fri)))
chart.CumReturns(keepDays(tickerClOp, c(mon)))
chart.CumReturns(keepDays(tickerClOp, c(mon,tue)))
chart.CumReturns(keepDays(tickerClOp, c(wed,thu,fri)))
chart.CumReturns(keepDays(tickerClOp, c(mon,tue,wed)))
chart.CumReturns(keepDays(tickerClOp, c(mon,tue,wed,thu)))
chart.CumReturns(keepDays(tickerClOp, c(mon,tue,wed, thu, fri)))

Return.annualized(tickerMT)
chart.CumReturns(tickerMT)

mdat <- getData(ticker, period, yearsBack, afterHours)


period <- "DayOfWeek"
	yearsBack <- 3
	plotCummulativeReturn <- F
	
	tickerData <- getTickerData(ticker, yearsBack)
	mdat <- getData(ticker, period, yearsBack, afterHours)
	 

		if (plotCummulativeReturn) {
			if (afterHours) 
				chart.CumReturns(ClOp(tickerData), main="Cummulative Return")
			else if (period=="DayOfWeek")
				chart.CumReturns(ROC(Cl(tickerData)), main="Cummulative Return")
			else if (period=="MonthOfYear")
				chart.CumReturns(monthlyReturn(tickerData),main="Cummulative Return")
			else print("error in plotcummulativereturn")
			}  else {
			p <- ggplot(mdat,aes(per,value))  
			p <- p + labs(x="", y = "% per day")
			p <- p +  stat_summary(fun.data=myBoxPlotSummary, geom="boxplot")
			#+stat_summary(fun.y = myBoxPlotOutliers, geom="point") 	
			p
			}
charts.PerformanceSummary(ClOp(tickerData))

summary(ClOp(tickerData))
summary(ROC(Cl(tickerData)))

