getTestData <- function(){
	#data(sample_matrix)
	#x <- as.xts(sample_matrix, descr='my new xts object')
	x <- xts(cbind(Open=1:40, Close=2:41), order=Sys.Date() + 1:40)
	#xx <- keepDaysF(x,c("Mon","Tue", "Wed","Thu","Fri"))
	return(x)	
}
getylim <- function(x){
	if (doDebug) cat(x)
 	if (is.na(x))
 		x <- NULL
 	else
		x <-  c(-.5,x)
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
	 tickerData <- adjustOHLC(get(ticker), symbol.name=ticker)
	 return(tickerData)	
}

ROCCl <- function(x){
	if(doDebug) cat("ROCCl\n")
	xx <- ROC(Cl(x))
	colnames(xx) <- "NormalROC"
 	return(xx)
}

dayOnly <- function(x){
	if (doDebug) cat("ROCClAlt\n")
	xx <- opcl(x)
	colnames(xx) <- "NormalROCALT"
	return(xx)
}
dNbD <- function(data, model){
	if(doDebug) cat("dNbD: 2 hold days after bad nights\n")
	day <- OpCl(data)
	badN <- model<0
	model[badN] <-  model[badN] + day[badN]
	colnames(model) <- modelNames[2]
	return(model)
}
#remove nights after good day (reverse to mean)
uDnN <- function(data, model){
	if(doDebug) cat("uDnN:3 nights only after bad days\n")
	day <- lag(OpCl(data))
	model[day>0] <-  0
	colnames(model) <- modelNames[3]
	return(model)
}

#good night buy day (forward momentum)
uNbD <- function(data, model){
	if(doDebug) cat("uNbD:4 hold days after good nights\n")
	day <- OpCl(data)
	goodN <- model>0
	model[goodN] <-  model[goodN] + day[goodN]
	colnames(model) <- modelNames[4]
	return(model)
}

uDbN <- function(data, model){
	if(doDebug) cat("uDbN:5 nights only after good days\n")
	day <- lag(OpCl(data))
	model[day<0] <-  0
	colnames(model) <- modelNames[5]
	return(model)
}

bNsD <- function(data, night){
	if(doDebug) cat("bNsD: 6 short days\n")
	day <- OpCl(data)
	night <- ClOp(data)
	model <- night - day
	colnames(model) <- modelNames[6]
	return(model)
}

keepDaysF<- function(x, keepDays){
	if(doDebug) cat("keepDaysF\n")	
	if (!is.null(x)) 
		xx <- x[format(index(x), "%a") %in% keepDays]
	else
		xx <- NULL	
	return(xx)
}
######################################
ClCl <- function(x, logOrArith="log") {
	if(doDebug) cat("ClCl\n")
	xx <- Delt(Lag(Cl(x)),Cl(x)-cost, type=logOrArith)
	colnames(xx) <- "CloseToClose"
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