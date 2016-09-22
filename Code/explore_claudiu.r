#Explore features and predicted indexes

library(quantmod)
library(ggplot2)
library(reshape2)

load("DataWork/VixDat.Rdata")

plotdat <- as.data.frame(cumprod(VixDat[-1,c("SPXRet","PUTRet","BFLYRet","VXORet")]/100+1))
plotdat$Date <- index(VixDat[-1,])
plotdat <- melt(plotdat,id="Date",value.name = "Price")
g <- ggplot(plotdat,aes(x=Date,y=Price,colour=variable))+geom_line()
print(g)
