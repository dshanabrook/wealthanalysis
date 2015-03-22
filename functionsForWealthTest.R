
addDOW <- function(tickerOHLC){
y <- head(tickerOHLC)
dowRange <- 0:4
dow <- c("Mon","Tue","Wed","Thu","Fri")
y$dow<-factor(.indexwday(y), dowRange,labels=dow, ordered=T)
return(y)
}

keepWDay <- function(data, daysToKeep=c(1,2,3,4,5)) {
	daysToKeep <- daysToKeep-1
#	print(daysToKeep)
	x <- subset(data, .indexwday(data) %in% daysToKeep)	
	return(x)
}

keepMDay <- function(data, daysToKeep=c(1:31)){
	print(daysToKeep)
	x <- subset(data, .indexmday(data) %in% daysToKeep)	
	return(x)
}
#test
keepDays(tickerOHLC, c(1,2))

getDataOHLC <- function(ticker, daysBack) {
	getSymbols(ticker, src="yahoo", from =(Sys.Date()-daysBack))
	tickerOHLC <- get(ticker)
	tickerAdj <- adjustOHLC(tickerOHLC, symbol.name=ticker)
	return(tickerAdj)
	}
	a30day <- getDataOHLC(ticker,30)
tickerData <- list(
	day30=getDataOHLC(ticker,30),
	day90=getDataOHLC(ticker,90),
	day180Day=getDataOHLC(ticker,180),
	year1=getDataOHLC(ticker,365),
	year3=getDataOHLC(ticker,365*3),
	year5=getDataOHLC(ticker,365*5)
	)
results <- vector()
for (i in 1:length(tickerData)){
	cr <- Return.cumulative(ClOp(tickerData[[i]]))
	results <- c(results,cr)
	}
