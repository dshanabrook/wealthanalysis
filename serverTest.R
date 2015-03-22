#cef analysis 
#use abbyy finereader to get the data from the pdf.  specify the table and output.  should work ok
rm(list=ls())
library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
require(reshape2)
#setwd("~/ShinyApps/wealthanalysis")
source("source/functions.R")


#new testing

ticker <- "AAPL"
yearsBack <- 3
tickerData <- getTickerData(ticker, yearsBack)
x <- cbind(ROCCl(tickerData),Cl(tickerData),ClOp(tickerData))
xx <- cbind(ClCl(tickerData),ROCCl(tickerData), ClOp(tickerData))
xclop <- ClOp(tickerData)
xcl <- Cl(tickerData)
xROCCl <- ROCCl(tickerData)

#difference between log and arith
cbind(ClCl(tickerData,"log"),ROC(Cl(tickerData)),ClCl(tickerData,"arithmetic"))

clop <- ClOp(tickerData)
plot(density(clop$CloseToOpen, na.rm=T), xlim=c(-.02,.02))
#histogram for 300 shares
x <- tickerData
shares <- 300
inoutCost <- 2
high <- shares* 2; low <- -high
xx <- (Op(x)-Lag(Cl(x)))*shares-inoutCost
plot(density(xx[,1], na.rm=T), xlim=c(-1.5,1.5))
hist(xx[,1],breaks=500,  xlim=c(low,high),main=paste(ticker, shares, "shares daily trade $", inoutCost, "fee", "outliers below $", low, "above $", high), xlab="$ per day")

chart.Boxplot(xx)

roc<- diff(log(x))
Delt(Lag(Cl(x)),Op(x))

doDebug <<- F
Sys.setenv(TZ='EST')

	afterHours <- T
	ticker <- "AAPL"
	period <- "DayOfWeek"
	yearsBack <- 3
	plotCummulativeReturn <- T
	
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
charts.PerformanceSummary(ROC(Cl(tickerData)))
table.DownsideRisk(ClOp(tickerData))
table.DownsideRisk(ROC(Cl(tickerData)))
SemiDeviation(ROC(Cl(tickerData)))
SemiDeviation(ClOp(tickerData))
SharpeRatio(ClOp(tickerData))
SharpeRatio(ROC(Cl(tickerData)))
chart.Drawdown(ROC(Cl(tickerData)))
chart.Drawdown(ClOp(tickerData))
all <- cbind(ClOp(tickerData),ROC(Cl(tickerData)))
chart.Drawdown(all)
chart.CumReturns(all)


data(edhec)
chart.CumReturns(edhec[,"Funds of Funds"],main="Cumulative Returns")
chart.CumReturns(edhec[,"Funds of Funds"],wealth.index=TRUE, main="Growth of $1")
data(managers)
chart.CumReturns(managers,main="Cumulative Returns",begin="first")
chart.CumReturns(managers,main="Cumulative Returns",begin="axis")