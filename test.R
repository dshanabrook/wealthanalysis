
rm(list=ls)
library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
require(reshape2)
setwd("~/ShinyApps/wealthAnalysis")
source("source/functions.R")
#cost <- function{return(0)}
ticker <- "AAPL"
yearsBack <- 3
doDebug<<-T
cost <<- 0.00
useWealth <- T
Sys.setenv(TZ='EST')
keepDays <- c("Mon","Tue","Wed","Thu","Fri")

dateRange <- c("2015-03-01","2015-04-22")
tickerData <- getTickerData(ticker, dateRange,yearsBack)
modelNames <<- c('m1_buyNights',"m2_buyDayReversal", "m3_buyNightReversal", "m4_keepDayMomentum", "m5_buyNightMomentum", "m6_buyNightsSellDays")

	clop <- ClOp(tickerData, cost)
	m_b <- ROCCl(tickerData)

	m_bN <-  clop
	m_dNbD <- dNbD(tickerData, clop)			
	m_uDnN <- uDnN(tickerData, clop)						
	m_uNbD <- uNbD(tickerData,clop)			
	m_uDbN <- uDbN(tickerData, clop)          	
	m_bNsD <- bNsD(tickerData, clop)					

	 m2_b <-  keepDaysF(m_b, keepDays)
	 m2_bN <-    keepDaysF(m_bN, keepDays)
	 m2_dNbD <- 	 keepDaysF(m_dNbD,keepDays)
	 m2_uDnN <-  keepDaysF(m_uDnN, keepDays)	
	 m2_uNbD <-  keepDaysF(m_uNbD, keepDays)	
	 m2_uDbN <-  keepDaysF(m_uDbN, keepDays)
	 m2_bNsD <-  keepDaysF(m_bNsD, keepDays)
	
	models <- cbind(m_b, m2_b, m2_bN,  m2_dNbD,  m2_uDnN, m2_uNbD, m2_uDbN, m2_bNsD)
	indexFormat(models) <- "%d%a" 
	theYlim <- getylim(NA) 

	chart.CumReturns(models, wealth.index=useWealth, main="Cummulative return", legend.loc="topleft", colorset=rainbow(7), ylim=theYlim)

###################
#test by making all nights zero
head(tickerData)
tickerData$AAPL.Open <- lag(tickerData$AAPL.Close)
Return.cumulative(m_bNbD)
m_bN <-  ClOp(tickerData)
Return.cumulative(m_bN)
#now do the above
###################

modelB <- ClOp(tickerData)


chart.CumReturns(model)
par("usr")
goodDayNoNight(tickerData,model)
badDayNoNight(tickerData,model)
goodNightBuyDay(tickerData,model)
badDayNoNight(tickerData,model)
goodNightSellDay(tickerData,model)

clclB <- ROCCl(tickerData)
clopB <- ClOp(tickerData)
clop <- ClOp(tickerData)
opop <- OpOp(tickerData)
Return.cumulative(clop)
Return.cumulative(opop)
Return.cumulative(ClOpMom(tickerData)$mod)
Return.cumulative(ClOpRevert(tickerData)$mod)

	chart.CumReturns(cbind(clclB,clopB, modelB),,main="Cummulative return", wealth.index =TRUE,legend.loc="topleft", colorset=c(8,1,3), ylim=NULL)

########################




keepDays <- c("Mon","Wed","Fri")
x <- keepDaysF(tickerData, keepDays)

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
chart.CumReturns(clop, yLim=NULL, wealth.index=T, ylim=NULL)
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

 max(Return.cumulative(modelB), na.rm=T)
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


