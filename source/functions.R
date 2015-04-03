myBoxPlotSummary <- function(x) {
  r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9),na.rm=TRUE)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

myBoxPlotOutliers <- function(x) {
  tmp<-quantile(x,probs=c(.05,.95),na.rm=TRUE)
  subset(x, x < tmp[1] | tmp[2] < x)
}

putYearAtEnd <- function(aDate){
	year <- substr(aDate,1,4)
	monthDay <- substr(aDate,6,10)
	paste(monthDay,"-",year, sep="")
}

getDateStr <- function(tickerData){
		mdat <- data.frame(tickerData)
		start <- rownames(mdat[1,])
		end <- rownames(mdat[nrow(mdat),])
		start <- putYearAtEnd(start)
		end <- putYearAtEnd(end)
		paste(start, "to", end)}

getAfterHoursStr <- function(afterHours) {
	if (afterHours)
		theString <- "buy at close, sell at open"
	else
		theString <- " "
	return(theString)
}

getCaption <- function(period, ticker="AAPL",plotCumm=T){
	if(doDebug) cat("getCaption\n")
	if (plotCumm)
		theString <- paste("Compare investment to close->open trade:", ticker)
	else
		theString <- paste(ticker, "percent changed by ", period)
	return(theString)
}
ClOp <-function(x, cost = 0, logOrArith="log") {
	if(doDebug) cat("ClOp cost:\n")
	if (doDebug) cat(cost, "\n")
	
	xx <- Delt(Lag(Cl(x)),Op(x)-cost, type=logOrArith)
	#colnames(xx) <- paste("ClOp", deparse(substitute(x)), sep='.')
	colnames(xx) <- "CloseToOpen"
	return(xx)}
	
ClCl <- function(x, logOrArith="log") {
	if(doDebug) cat("ClCl\n")
	xx <- Delt(Lag(Cl(x)),Cl(x)-cost, type=logOrArith)
	colnames(xx) <- "CloseToClose"
	return(xx)
}
OpCl <- function(x, cost=0,logOrArith="log"){
	if(doDebug) cat("OpCl\n")
	xx <- Delt(Op(x),Cl(x)-cost, type=logOrArith)
	colnames(xx) <- "OpenToClose"
	return(xx)
}
#remove nights after bad day (momentum)
downDayNoNightF <- function(data, model){
	if(doDebug) cat("downDayNoNightF\n")
	day <- lag(OpCl(data))
	model[day<0] <-  0
	colnames(model) <- paste(colnames(model), "downDayNoNight")
	return(model)
}
#remove nights after good day (reverse to mean)
upDayNoNightF <- function(data, model){
	if(doDebug) cat("upDayNoNightF\n")
	day <- lag(OpCl(data))
	model[day>0] <-  0
	colnames(model) <- paste(colnames(model), "upDayNoNight")
	return(model)
}
#good night buy day (forward momentum)
upNightKeepDayF <- function(data, model){
	if(doDebug) cat("upNightKeepDayF\n")
	day <- OpCl(data)
	day <- day[model>0]
	model[model>0] <-  model[model>0] + day
	colnames(model) <- "upNightKeepDay"
	return(model)
}
downNightKeepDayF <- function(data, model){
	if(doDebug) cat("downNightKeepDayF\n")
	day <- OpCl(data)
	day <- day[model<0]
	model[model<0] <-  model[model<0] + day
	colnames(model) <- "downNightKeepDayF"
	return(model)
}
upNightSellDayF <- function(data, model, cost){
	if(doDebug) cat("upNightSellDayF\n")
	day <- OpCl(data)
	night <- ClOp(data)
	model <- night - day[night>0] #night plus some days
#	model[model>0] <-  model[model>0] - day
	colnames(model) <- paste(colnames(model), "upNightSellDay")
	return(model)
}

#not implemented
keepDaysF<- function(x, keepDays){
	if(doDebug) cat("keepDaysF\n")
	xx <- x[format(index(x), "%a") %in% keepDays]
	return(xx)
}

ClOpMom <- function(x){
	if(doDebug) cat("ClOpMom\n")
	xx <- ClOp(x)
	xx[xx>0,] <-  xx[xx>0,] + OpCl(x[ClOp(x)>0,]) 
	colnames(xx) <- "momentum"
	return(xx)
}

ClOpLongOpClShort <- function(x, logOrArith ="log"){
	if(doDebug) cat("ClOpLongOpClShort\n")
	long <- ClOp(x)
	short <- OpClShort(x)
	xx <- long[,1]+short[,1]
	colnames(xx) <- "short"
	return(xx)
}

OpClShort <- function(x, logOrArith ="log"){
	if(doDebug) cat("OpClShort\n")
	xx <- Delt(Cl(x),Op(x), type= logOrArith)
	colnames(xx) <- "shortDay"
	return(xx)
}

ROCCl <- function(x){
	if(doDebug) cat("ROCCl\n")
	xx <- ROC(Cl(x))
	colnames(xx) <- "NormalROC"
 	return(xx)
}

getTickerData <- function(ticker="AAPL",dateRange, yearsBack=0.1){
	if(doDebug) cat("getTickerData\n")
	Sys.setenv(TZ='UTC');
	theDaysBack <- yearsBack*251
	 getSymbols(ticker,src="yahoo", from=dateRange[1],to=dateRange[2])
#	 temp1 <- get(ticker)
#	 temp2  <- tail(temp1, theDaysBack)
	 tickerData <- adjustOHLC(get(ticker), symbol.name=ticker)
	 return(tickerData)	
}

# x <- data.frame(c(1:5,-1:-5),c(1:10))
# names(x) <- c("clop", "opcl")
# attach(x)
# x[x$clop>0,]$clop <- x[x$clop>0,]$clop+x[x$clop>0,]$opcl
# #it just duplicates clop, then adds opcl when it is positive




	

	


