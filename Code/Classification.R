Alexa<-function(Day2class){
#Day2class<- "2013-11-27"
load("DataWork/VixDat.RData")

Day2class<-paste0("1988-06-01/",Day2class) #start Date:enDate
VixDat<-VixDat[Day2class]
Last<-VixDat[nrow(VixDat),]#t(data.frame(as.numeric(VixDat[nrow(VixDat),]))) #number of rows take the last one
colnames(Last)<-names(VixDat)
rownames(Last)<-1:nrow(Last)

classiffication <-as.numeric( ifelse(VixDat[,"PUTRet"]<0,0,1))
classiffication<-classiffication[-1] #we dont want the first line
Date<-index(VixDat)[-nrow(VixDat)]
classiffication<-xts(classiffication,Date)
VixDat<-VixDat[-nrow(VixDat),] #we want to gess the last line so we delete this line
Date<-index(VixDat)
#new column:
VixDat$classiffication <-classiffication
#classiffication<-xts(classiffication,Date)
#VixDat<-merge.xts(VixDat,classiffication)
#sum(is.na(VixDat[,"VXO"]))
DelRows<-which(is.na(VixDat[,"VXO"]))
VixDat<-VixDat[-DelRows,]
#names(VixDat): see the name of the rows

Top.Cut.Off<-0
for (cutoff in seq(0,1,0.01)){
probit<-glm(formula = classiffication ~VXO+SPX, family =binomial(link="probit"),data = VixDat)
#summary(probit)
#a<-mean(VixDat[-1,"classiffication"])

Prediction <- ifelse(probit$fitted.values<cutoff,0,1)
#sum(is.na(VixDat[,"VXO"]))
#sum(is.na(VixDat[,"SPX"]))
Top.Cut.Off<-c(Top.Cut.Off,mean(1-abs(Prediction-VixDat[,"classiffication"])))
}

plot(seq(0,1,0.01),Top.Cut.Off[-1])
BestCutof<-c(seq(0,1,0.01))[which(Top.Cut.Off[-1]==max(Top.Cut.Off))]

probit<-glm(formula = classiffication ~VXO+SPX, family =binomial(link="probit"),data = VixDat)
#summary(probit)
#a<-mean(VixDat[-1,"classiffication"])
PredicTest<-predict(probit, Last[,c("VXO","SPX")], type="response") 

BuySell<-ifelse(PredicTest<BestCutof,0,1)




#Prediction <- ifelse(probit$fitted.values<BestCutof,0,1)
#sum(is.na(VixDat[,"VXO"]))
#sum(is.na(VixDat[,"SPX"]))
#Top.Cut.Off<-c(Top.Cut.Off,mean(1-abs(Prediction-VixDat[-1,"classiffication"])))


#Prediction<-as.numeric(c("NA",Prediction))
#Prediction<-xts(Prediction,Date)
#VixDat<-merge.xts(VixDat,Prediction)


#mean(1-abs(VixDat[-1,"Prediction"]-VixDat[-1,"classiffication"]))
save(BuySell,file = "DataWork/Classify.RData")

return(BuySell)
}
