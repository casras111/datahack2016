library(zoo)
library(xts)
load("DataWork/VixDat.Rdata")

Hefresh = 1+(as.numeric(VixDat[-1,"PUT"]) - as.numeric(VixDat[-nrow(VixDat),"PUT"]))/VixDat[-nrow(VixDat),"PUT"]
aggRet<-cumprod(Hefresh)
x<-ifelse(runif(nrow(aggRet),-1,1)<0.5,-1,1)
hef2<-1+cumprod(Hefresh*x)
dat<-merge(aggRet,x)
plot(hef2)





 


