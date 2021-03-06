#Classifier with regression

library(quantmod)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(caret)
library(MASS)

load("DataWork/VixDat.Rdata")
load("DataWork/WeekDat.Rdata")

WeekDat$PUTWkRet <- lag(WeekDat$PUTWkRet,-1) #dependent value
WeekDat <- WeekDat[-NROW(WeekDat),]          #remove last row with NA dependent

trainIndx <- createDataPartition(WeekDat$PUTWkRet,times=1,p=0.7,list=FALSE)

WeekDatTrain <- WeekDat[trainIndx]
WeekDatTest  <- WeekDat[-trainIndx]

objControl <- trainControl(method='repeatedcv',number=5,repeats=10)
prednames <- colnames(WeekDat)[grep("pred",colnames(WeekDat))]
f <- paste(prednames,collapse ="+")
#f <- "pred1+pred3+pred4+pred5+pred6+pred7+pred9+
#      pred12+pred13+pred15+pred16+pred17"
reg1 <- train(as.formula(paste0("PUTWkRet~",f)),method="rlm",
              data=WeekDatTrain,trControl=objControl,maxit=30)
#reg1 <- rlm(as.formula(paste0("PUTWkRet~",f)),data=WeekDatTrain,
#            maxit=30,method="MM")
summary(reg1)

#check accuracy on train data
pred.train <- predict(reg1,newdata=WeekDatTrain)
pred.train <- ifelse(pred.train>0,1,-1)
#pred.train <- ifelse(pred.train<(-3),-1,ifelse(pred.train>0.1,1,0))
y <- ifelse(WeekDatTrain$PUTWkRet>0,1,-1)
(contingency <- prop.table(table(pred=pred.train,train=y)))
sum(diag(contingency))
profit.train <- WeekDatTrain$PUTWkRet*pred.train
cumprofit.train <- cumprod(1+profit.train/100)

plotxts <- merge(cumprod(1+WeekDatTrain$PUTWkRet/100),cumprofit.train)
plotdat1 <- data.frame(coredata(plotxts[,1]),coredata(plotxts[,2]))
names(plotdat1) <- c("PUT","StratPUT")
plotdat1$Date <- index(WeekDatTrain)
plotdat1 <- melt(plotdat1,id="Date",value.name = "CumRet")
g1 <- ggplot(plotdat1,aes(x=Date,y=CumRet,colour=variable))+geom_line()
g1 <- g1+ggtitle("CumReturn compare for PUT strategy train data")
print(g1)

#check accuracy on test data
p1 <- predict(reg1,newdata = WeekDatTest)
pred.test <- ifelse(p1>0,1,-1)
#pred.test <- ifelse(p1<(-3),-1,ifelse(p1>0.1,1,0))
y <- ifelse(WeekDatTest$PUTWkRet>0,1,-1)
(contingency <- prop.table(table(pred=pred.test,train=y)))
sum(diag(contingency))
profit.test <- WeekDatTest$PUTWkRet*pred.test
cumprofit.test <- cumprod(1+profit.test/100)

plotxts <- merge(cumprod(1+WeekDatTest$PUTWkRet/100),cumprofit.test)
plotdat1 <- data.frame(coredata(plotxts[,1]),coredata(plotxts[,2]))
names(plotdat1) <- c("PUT","StratPUT")
plotdat1$Date <- index(WeekDatTest)
plotdat1 <- melt(plotdat1,id="Date",value.name = "CumRet")
g1 <- ggplot(plotdat1,aes(x=Date,y=CumRet,colour=variable))+geom_line()
g1 <- g1+ggtitle("CumReturn compare for PUT strategy test data")
print(g1)

p3 <- predict(reg1,newdata = WeekDat)
WeekDat$Prediction <- ifelse(p3>0,1,-1)
#WeekDat$Prediction <- ifelse(p3<(-3),-1,ifelse(p3>0.1,1,0))
WeekDatPred <- WeekDat

save(WeekDatPred,file="DataWork/WeekDatPred.Rdata")
