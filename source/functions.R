getTestData <- function(){
	#data(sample_matrix)
	#x <- as.xts(sample_matrix, descr='my new xts object')
	#all at night, up then down
	#first 7 days up 1, nights up .5
	open <- seq(1.0, 1.9, by=0.15)
	close <- open + 0.1
	#opposite *2
	open2 <- seq(open[7], 0.82, by=-0.18)
	close2 <- open2 - 0.1
	open <- c(open,open2); close <- c(close,close2)
	x <- xts(cbind(Open=open, Close=close), order=Sys.Date() + 0:(length(open)-1))
	#xx <- keepDaysF(x,c("Mon","Tue", "Wed","Thu","Fri"))
	return(x)	
}
getylim <- function(x){
	if (doDebug) cat(x)
 	if (is.na(x))
 		x <- NULL
 	else
		x <-  c(1,x)
	if (doDebug) cat(x)
	return(x)
	}

ClOp <-function(x, cost = 0, logOrArith="log") {
	if(doDebug) cat("ClOp cost:\n")
	xx <- Delt(Lag(Cl(x)),Op(x)-cost, type=logOrArith)
	colnames(xx) <- modelNames[1]
	return(xx)}
	
OpCl <- function(x, cost=0,logOrArith="log"){
	if(doDebug) cat("OpCl\n")
	xx <- Delt(Op(x),Cl(x)-cost, type=logOrArith)
	colnames(xx) <- "OpenToClose"
	return(xx)
}

getTickerData <- function(ticker="AAPL",dateRange, yearsBack=0.1){
	if(doDebug) cat("getTickerData\n")
	Sys.setenv(TZ='UTC');
	theDaysBack <- yearsBack*251
	 getSymbols(ticker,src="yahoo",from=dateRange[1], to=dateRange[2])
	 ticker1 <- gsub("\\^","",ticker)
	# ticker1 <- gsub("\\%5E","",ticker1)
	 tickerData <- adjustOHLC(get(ticker), symbol.name=ticker)
	 return(tickerData)	
}


ClCl <- function(x){
	if (doDebug) cat("buyAndHold\n")
	xx <- ROC(Cl(x))
	colnames(xx) <- "buyAndHold"
	return(xx)
}

dNbD <- function(days, nights){
	if(doDebug) cat("dNbD: 2 hold days after bad nights\n")
	badN <- nights<0
	nights[badN] <-  nights[badN] + days[badN]
	colnames(nights) <- modelNames[2]
	return(nights)
}
#remove nights after good day (reverse to mean)
uDnN <- function(data, nights){
	if(doDebug) cat("uDnN:3 nights only after bad days\n")
	day <- lag(OpCl(data))
	nights[day>0] <-  0
	colnames(nights) <- modelNames[3]
	return(nights)
}

#good night buy day (forward momentum)
uNbD <- function(days, nights){
	if(doDebug) cat("uNbD:4 hold days after good nights\n")
	goodN <- nights>0
	nights[goodN] <-  nights[goodN] + days[goodN]
	colnames(nights) <- modelNames[4]
	return(nights)
}

uDbN <- function(data, nights){
	if(doDebug) cat("uDbN:5 nights only after good days\n")
	day <- lag(OpCl(data))
	nights[day<0] <-  0
	colnames(nights) <- modelNames[5]
	return(nights)
}

bNsD <- function(days, nights){
	if(doDebug) cat("bNsD: 6 short days\n")
	model <- nights - days
	colnames(model) <- modelNames[6]
	return(model)
}

keepDaysF<- function(x, keepDays){
	if(doDebug) cat("keepDaysF\n")	
	if (!is.null(x)) 
		xx <- x[format(index(x), "%a") %in% keepDays]
	else
		xx <- NULL	
			colnames(xx) <- "day of week model"

	return(xx)
}

daysToKeep <- function(days, nights, dontSellDays){
	if(doDebug) cat("dontSellDays\n")
	days[!format(index(days), "%a") %in% dontSellDays] <- 0
	model <- days + nights
	colnames(model) <- modelNames[7]
	return(model)	
}
######################################
ClCl <- function(x, cost=0, logOrArith="log") {
	if(doDebug) cat("ClCl\n")
	xx <- Delt(Lag(Cl(x)),Cl(x)-cost, type=logOrArith)
	colnames(xx) <- "Buy And Hold"
	return(xx)
}

bNsDAlt <- function(x, logOrArith ="log"){
	if(doDebug) cat("bNsD: 6 short days\n")
	long <- ClOp(x)
	short <- OpClShort(x)
	xx <- long[,1]+short[,1]
	colnames(xx) <- modelNames[7]
	return(xx)
}

dNbDAlt <- function(data, model, cost){
	if(doDebug) cat("upNightSellDayF\n")
	day <- OpCl(data)
	night <- ClOp(data)
	model <- night - day[night>0] #night plus some days
#	model[model>0] <-  model[model>0] - day
	colnames(model) <- paste(colnames(model), "upNightSellDay")
	return(model)
}

ClOpMom <- function(x){
	if(doDebug) cat("ClOpMom\n")
	xx <- ClOp(x)
	xx[xx>0,] <-  xx[xx>0,] + OpCl(x[ClOp(x)>0,]) 
	colnames(xx) <- "momentum"
	return(xx)
}

OpClShort <- function(x, logOrArith ="log"){
	if(doDebug) cat("OpClShort\n")
	xx <- Delt(Cl(x),Op(x), type= logOrArith)
	colnames(xx) <- "shortDay"
	return(xx)
}

# getylimOld <- function(ylimfix, m1, m2, m3){
	# m1 <- Return.cumulative(m1)
 	# if (is.null(ylimfix))
		# theYlim  <-  NULL
	# else
		# theYlim <-  c(min(m1,m2,m3,na.rm=T), max(m1,m2,m3, na.rm=T))
	# if (doDebug) cat(theYlim)
	# return(theYlim)
	# }
	
# x <- data.frame(c(1:5,-1:-5),c(1:10))
# names(x) <- c("clop", "opcl")
# attach(x)
# x[x$clop>0,]$clop <- x[x$clop>0,]$clop+x[x$clop>0,]$opcl
# #it just duplicates clop, then adds opcl when it is positive