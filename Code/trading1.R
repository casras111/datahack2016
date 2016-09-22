library(zoo)
library(xts)
library(PerformanceAnalytics)
load("DataWork/VixDat.Rdata")

Hefresh = 1+(as.numeric(VixDat[-1,"PUT"]) - as.numeric(VixDat[-nrow(VixDat),"PUT"]))/VixDat[-nrow(VixDat),"PUT"]
aggRet<-cumprod(Hefresh)
x<-ifelse(runif(nrow(VixDat$PUTRet[-1]),-1,1)<0.5,-1,1)
#hef2<-1+cumprod(Hefresh*x)
plot(hef2)
hef2<-cumprod((VixDat$PUTRet[-1])*x)+1

plot 


