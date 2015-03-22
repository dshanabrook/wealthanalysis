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
	xx <- Delt(Lag(Cl(x)),Op(x), type=logOrArith)
	#colnames(xx) <- paste("ClOp", deparse(substitute(x)), sep='.')
	colnames(xx) <- "CloseToOpen"
	return(xx)}
	
ClCl <- function(x, logOrArith="log") {
	xx <- Delt(Lag(Cl(x)),Cl(x), type=logOrArith)
	colnames(xx) <- "CloseToClose"
	return(xx)
}
OpCl <- function(x, logOrArith="log" ){
	xx <- Delt(Op(x),Cl(x), type=logOrArith)
	colnames(xx) <- "OpenToClose"
	return(xx)
}

shortLong <- function(x, logOrArith ="log"){
	long <- ClOp(x)
	short <- OpClShort(x)
	xx <- long[,1]+short[,1]
	colnames(xx) <- "longNightShortDay"
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
 
getTickerData <- function(ticker="AAPL", yearsBack=0.1){
	Sys.setenv(TZ='UTC');
	theDaysBack <- yearsBack*251
	 getSymbols(ticker,src="yahoo", from="1900-01-01")
	 temp <- get(ticker)
	 temp2  <- tail(temp1, theDaysBack)
	 tickerData <- adjustOHLC(temp2, symbol.name=ticker)
	 return(tickerData)	
}

#not used:
# getData <-function(ticker="AAPL",period="DayOfWeek", yearsBack=0.1, afterHours=F){
	# Sys.setenv(TZ='UTC');
	# dowRange <- 1:5
	# dow <- c("Mon","Tue","Wed","Thu","Fri")
    # moy<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
	# tickerData <- getTickerData(ticker, yearsBack)
	 
	# if (afterHours) {
	 	# freq<-factor(.indexwday(tickerData), dowRange,labels=dow, ordered=T)
		# mydf <- data.frame(per=freq,ClOp(tickerData))
	# }else if (period=="DayOfWeek"){
		# freq<-factor(.indexwday(tickerData), dowRange,labels=dow, ordered=T)
		# mydf <- data.frame(per=freq,ROCCl(tickerData))
	# }else if (period=="MonthOfYear"){
		# mondf <- monthlyReturn(tickerData)
		# monthly<-factor(.indexmon(mondf),levels=0:11,labels= moy, ordered=T)
		# mydf <- data.frame(per=monthly, mondf)
		# }
	# mdat<- melt(mydf)
	# mdat$value <- mdat$value*100
	# rownames(mdat) <- rownames(mydf)
	# return(mdat)	
	# }
	

	


