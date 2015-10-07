#testor for wealth

library(shiny)
library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(RColorBrewer)
require(reshape2)
setwd("~/ShinyApps/wealthAnalysis")
source("source/functions.R", local = TRUE)
doDebug <<- T
useWealth <<- T
upylim <- NA
Sys.setenv(TZ = "EST")
ticker <- "^IXIC"
ticker <- "%5EIXIC"
ticker <- "^GSPC"
ticker <- "%5EGSPC"
ticker <- "%5EDJI"
ticker <- "%5EFTSE"
ticker <- "%5EN225"
ticker <- "%5EGDAXI"

name <- paste(getQuote(ticker, what = yahooQF("Name"))[, 2])
dateRange <- c("2000-01-01", "2015-01-01")
tickerData <- getTickerData(ticker, dateRange)
theYlim <- getylim(upylim) 
m_buy <- ClCl(tickerData)
m_keepDays <- keepDaysF(m_buy, keepDays)
if (doDebug) 
	cat("cbindModels\n")
models <- cbind(m_buy, m_keepDays)
chart.CumReturns(models, wealth.index=useWealth, main=paste(name,"Cummulative return"),  colorset=rainbow(8))

getSymbols(ticker)