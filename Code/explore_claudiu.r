#Explore features and predicted indexes

library(quantmod)
library(ggplot2)
library(gridExtra)
library(reshape2)
#library(PerformanceAnalytics)

load("DataWork/VixDat.Rdata")

#chart.RelativePerformance(VixDat$SPXRet[-1],VixDat$PUTRet[-1]) #does not work

plotdat1 <- as.data.frame(cumprod(VixDat[-1,c("SPXRet","PUTRet","BFLYRet")]/100+1))
plotdat1$Date <- index(VixDat[-1,])
plotdat1 <- melt(plotdat1,id="Date",value.name = "CumRet")
g1 <- ggplot(plotdat1,aes(x=Date,y=CumRet,colour=variable))+geom_line()
g1 <- g1 + ggtitle("Cumulative return")+
  theme(legend.position="top",axis.title.x=element_blank(),
        axis.ticks=element_blank(),axis.text.x=element_blank())
plotdat2 <- data.frame(Price=coredata(VixDat$VXO[-1]),Date=index(VixDat[-1]))
g2 <- ggplot(plotdat2,aes(x=Date,y=VXO))+geom_line()
g <- grid.arrange(g1,g2,nrow=2,heights=c(50,25))

