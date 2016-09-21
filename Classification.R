load("DataWork/VixDat.RData")
classiffication <- ifelse(VixDat[,"PUTRet"]<0,0,1)
VixDat<-cbind(VixDat,classiffication)
#sum(is.na(VixDat[,"VXO"]))
DelRows<-which(is.na(VixDat[,"VXO"]))
VixDat<-VixDat[-DelRows,]
#names(VixDat): see the name of the rows

probit<-glm(formula = PUTRet.1 ~VXO+SPX, family =binomial(link="probit"),data = VixDat)
summary(probit)

a<-mean(VixDat[-1,"PUTRet.1"])

Prediction <- ifelse(probit$fitted.values<a,0,1)
sum(is.na(VixDat[,"VXO"]))
#sum(is.na(VixDat[,"SPX"]))
VixDat<-cbind.data.frame(VixDat,Prediction=c("Na",Prediction))
