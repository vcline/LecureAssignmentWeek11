###############################################################################
#Author:  Vishi Cline
#Created Date:  11/13/2016
#Description:  Downloads stock data for TC Pipelines, 
#               calculates log returns, 
#               calculates volatility measure
#               calculates volatility measure with a continuous lookback window
#               Plots the results with a volatility curve overlay
###############################################################################

#Install the tseries package
install.packages("tseries")
library(tseries)

#Download stock data for TC Pipelines
SNPdata<-get.hist.quote(instrument = 'TCP',quote="Close")
length(SNPdata)

#Calculate log return
SNPret<-log(lag(SNPdata))-log(SNPdata)
length(SNPret)

#Calculate volatility measure
SNPvol<-sd(SNPret) * sqrt(250)*100
SNPvol

#calculates volatility measure with a continuous lookback window
Vol<-function(d,logrets){
  var=0
  lam=0
  varlist<-c()
  for (r in logrets){
    lam=lam*(1-1/d)+1
    var=(1-1/lam)*var+(1/lam)*r^2
    varlist<-c(varlist,var)
  }
  sqrt(varlist)
}

#volatility with different weights
volest<-Vol(10, SNPret)
volest2<-Vol(30, SNPret)
volest3<-Vol(100, SNPret)

#Plots the results with a volatility curve overlay
plot(volest, type="l")
lines(volest2, type="l", col="red")
lines(volest3, type="l", col="blue")