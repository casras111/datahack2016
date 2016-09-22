Alexa<-function(){
load("DataWork/VixDat.RData")
classiffication <- ifelse(VixDat[,"PUTRet"]<0,0,1)
VixDat<-cbind(VixDat,classiffication)
#sum(is.na(VixDat[,"VXO"]))
DelRows<-which(is.na(VixDat[,"VXO"]))
VixDat<-VixDat[-DelRows,]
#names(VixDat): see the name of the rows

Top.Cut.Off<-0
for (cutoff in seq(0,1,0.01)){
probit<-glm(formula = PUTRet.1 ~VXO+SPX, family =binomial(link="probit"),data = VixDat)
#summary(probit)
#a<-mean(VixDat[-1,"PUTRet.1"])

Prediction <- ifelse(probit$fitted.values<cutoff,0,1)
#sum(is.na(VixDat[,"VXO"]))
#sum(is.na(VixDat[,"SPX"]))
Top.Cut.Off<-c(Top.Cut.Off,mean(1-abs(Prediction-VixDat[-1,"PUTRet.1"])))
}

plot(seq(0,1,0.01),Top.Cut.Off[-1])
BestCutof<-c(seq(0,1,0.01))[which(Top.Cut.Off[-1]==max(Top.Cut.Off))]

probit<-glm(formula = PUTRet.1 ~VXO+SPX, family =binomial(link="probit"),data = VixDat)
#summary(probit)
#a<-mean(VixDat[-1,"PUTRet.1"])

Prediction <- ifelse(probit$fitted.values<BestCutof,0,1)
#sum(is.na(VixDat[,"VXO"]))
#sum(is.na(VixDat[,"SPX"]))
Top.Cut.Off<-c(Top.Cut.Off,mean(1-abs(Prediction-VixDat[-1,"PUTRet.1"])))
Date<-index(VixDat)

Prediction<-as.numeric(c("NA",Prediction))
Prediction<-xts(Prediction,Date)
VixDat<-merge.xts(VixDat,Prediction)


mean(1-abs(VixDat[-1,"Prediction"]-VixDat[-1,"PUTRet.1"]))
save(Prediction,file = "Classify.RData")
}
