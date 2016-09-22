Alexa<-function(Day2class = "2016-06-30"){
#Day2class<- "2016-06-30"
load("DataWork/WeekDat.Rdata")

Day2class<-paste0("1988-06-01/",Day2class) #start Date:enDate
WeekDat<-WeekDat[Day2class]
Last<-WeekDat[nrow(WeekDat),]#t(data.frame(as.numeric(WeekDat[nrow(WeekDat),]))) #number of rows take the last one
colnames(Last)<-names(WeekDat)
rownames(Last)<-1:nrow(Last)

# 
# > names(WeekDat)
# [1] "PUTWkRet"  "BFLYWkRet" "SPXWkRet"  "pred1"     "pred2"     "pred3"     "pred4"     "pred5"    
# [9] "pred6"     "pred7"     "pred8"     "pred9"     "pred10"    "pred11"    "pred12"    "pred13"   
# [17] "pred14"    "pred15"   


classiffication <-as.numeric( ifelse(WeekDat[,"PUTWkRet"]<0,0,1))
classiffication<-classiffication[-1] #we dont want the first line
Date<-index(WeekDat)[-nrow(WeekDat)]
classiffication<-xts(classiffication,Date)
WeekDat<-WeekDat[-nrow(WeekDat),] #we want to gess the last line so we delete this line
Date<-index(WeekDat)
#new column:
WeekDat$classiffication <-classiffication
#classiffication<-xts(classiffication,Date)
#WeekDat<-merge.xts(WeekDat,classiffication)
#sum(is.na(WeekDat[,"SPXWkRet"]))
DelRows<-which(is.na(WeekDat[,"SPXWkRet"]))
if(DelRows>0)WeekDat<-WeekDat[-DelRows,]
#names(WeekDat): see the name of the rows

Top.Cut.Off<-0
for (cutoff in seq(0,1,0.01)){
probit<-glm(formula = classiffication ~ PUTWkRet + BFLYWkRet + SPXWkRet + pred1 + pred2 + pred3 + pred4 + pred5 + pred6 + pred7 + pred8 + pred9 + pred10 + pred11 + pred12 + pred13 + pred14 + pred15, family =binomial(link="probit"),data = WeekDat)
#summary(probit)
#a<-mean(WeekDat[-1,"classiffication"])

Prediction <- ifelse(probit$fitted.values<cutoff,0,1)
#sum(is.na(WeekDat[,"SPXWkRet"]))
#sum(is.na(WeekDat[,"SPX"]))
Top.Cut.Off<-c(Top.Cut.Off,mean(1-abs(Prediction-WeekDat[,"classiffication"])))
}

plot(seq(0,1,0.01),Top.Cut.Off[-1])
BestCutof<-c(seq(0,1,0.01))[which(Top.Cut.Off[-1]==max(Top.Cut.Off))]

probit<-glm(formula = classiffication ~PUTWkRet + BFLYWkRet + SPXWkRet + pred1 + pred2 + pred3 + pred4 + pred5 + pred6 + pred7 + pred8 + pred9 + pred10 + pred11 + pred12 + pred13 + pred14 + pred15, family =binomial(link="probit"),data = WeekDat)
SaveAndSend<-probit$fitted.values
WeeK<-ifelse(SaveAndSend<BestCutof,0,1)
WeeK<-xts(WeeK,index(WeekDat)[1:1461])
WeekDat$WeeK<-WeeK
save(WeeK,file = "Week.RData")

#summary(probit)
#a<-mean(WeekDat[-1,"classiffication"])
PredicTest<-predict(probit, Last[,c('PUTWkRet','BFLYWkRet','SPXWkRet','pred1','pred2','pred3','pred4','pred5','pred6','pred7','pred8','pred9','pred10','pred11','pred12','pred13','pred14','pred15')], type="response") 

BuySell<-ifelse(PredicTest<BestCutof,0,1)




#Prediction <- ifelse(probit$fitted.values<BestCutof,0,1)
#sum(is.na(WeekDat[,"SPXWkRet"]))
#sum(is.na(WeekDat[,"SPX"]))
#Top.Cut.Off<-c(Top.Cut.Off,mean(1-abs(Prediction-WeekDat[-1,"classiffication"])))


#Prediction<-as.numeric(c("NA",Prediction))
#Prediction<-xts(Prediction,Date)
#WeekDat<-merge.xts(WeekDat,Prediction)


#mean(1-abs(WeekDat[-1,"Prediction"]-WeekDat[-1,"classiffication"]))
save(BuySell,file = "DataWork/Classify.RData")

return(BuySell)
}
