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
	if (plotCumm)
		theString <- paste("Compare investment to close->open trade:", ticker)
	else
		theString <- paste(ticker, "percent changed by ", period)
	return(theString)
}
ClOp <-function(x, logOrArith="log") {
	print(cost)
	xx <- Delt(Lag(Cl(x)),Op(x)-cost(), type=logOrArith)
	#colnames(xx) <- paste("ClOp", deparse(substitute(x)), sep='.')
	colnames(xx) <- "CloseToOpen"
	return(xx)}
	
ClCl <- function(x, logOrArith="log") {
	xx <- Delt(Lag(Cl(x)),Cl(x), type=logOrArith)
	colnames(xx) <- "CloseToClose"
	return(xx)
}
OpCl <- function(x,logOrArith="log" ){
	xx <- Delt(Op(x),Cl(x)-cost(), type=logOrArith)
	colnames(xx) <- "OpenToClose"
	return(xx)
}
ClOpMom <- function(x){
	xx <- ClOp(x)
	xx[xx>0,] <-  xx[xx>0,] + OpCl(x[ClOp(x)>0,]) 
	colnames(xx) <- "momentum"
	return(xx)
}
ClOpRevert <- function(x){
	xx <- ClOp(x)
	xx[xx<0,] <-  xx[xx<0,] + OpCl(x[ClOp(x)<0,]) 
	colnames(xx) <- "reversion"
	return(xx)
}
ClOpLongOpClShort <- function(x, logOrArith ="log"){
	long <- ClOp(x)
	short <- OpClShort(x)
	xx <- long[,1]+short[,1]
	colnames(xx) <- "short"
	return(xx)
}

OpClShort <- function(x, logOrArith ="log"){
	xx <- Delt(Cl(x),Op(x), type= logOrArith)
	colnames(xx) <- "shortDay"
	return(xx)
}

ROCCl <- function(x){
	xx <- ROC(Cl(x))
	colnames(xx) <- "NormalROC"
 	return(xx)
}

doreturn <- function(){ print("\n")} 

getTickerData <- function(ticker="AAPL",dateRange, yearsBack=0.1){
	Sys.setenv(TZ='UTC');
	theDaysBack <- yearsBack*251
	 getSymbols(ticker,src="yahoo", from=dateRange[1],to=dateRange[2])
#	 temp1 <- get(ticker)
#	 temp2  <- tail(temp1, theDaysBack)
	 tickerData <- adjustOHLC(get(ticker), symbol.name=ticker)
	 return(tickerData)	
}

x <- data.frame(c(1:5,-1:-5),c(1:10))
names(x) <- c("clop", "opcl")
attach(x)
x[x$clop>0,]$clop <- x[x$clop>0,]$clop+x[x$clop>0,]$opcl
#it just duplicates clop, then adds opcl when it is positive




	

	


