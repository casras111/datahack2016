#Classifier with svm

library(quantmod)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(caret)
library(kernlab)      #svm for caret

load("DataWork/VixDat.Rdata")
load("DataWork/WeekDat.Rdata")

WeekDat$PUTWkRet <- lag(WeekDat$PUTWkRet,-1) #dependent value
WeekDat <- WeekDat[-NROW(WeekDat),]          #remove last row with NA dependent
WeekDat$Direction <- ifelse(WeekDat$PUTWkRet<(-5),-1,1)

trainIndx <- createDataPartition(WeekDat$Direction,times=1,p=0.7,list=FALSE)

WeekDatTrain <- WeekDat[trainIndx]
WeekDatTest  <- WeekDat[-trainIndx]

objControl <- trainControl(method='repeatedcv',number=5,repeats=10)
prednames <- colnames(WeekDat)[grep("pred",colnames(WeekDat))]
f <- paste(prednames,collapse ="+")

WeekDatdf <- as.data.frame(coredata(WeekDatTrain))
WeekDatdf$Direction <- as.factor(WeekDatdf$Direction)
svm_traingrid <- expand.grid(sigma=seq(0.01,0.07,0.01),C=seq(3,5,0.5))
svmfit1 <- train(as.formula(paste0("Direction~",f)),
                            data=WeekDatdf,trControl=objControl,
                            method="svmRadial")
                 #tuneGrid=svm_traingrid)
print(plot(svmfit1))
svmfit1

#check accuracy on train data
pred.train <- predict(svmfit1,newdata=WeekDatdf)
y <- ifelse(WeekDatTrain$PUTWkRet<(-5),-1,1)
(contingency <- prop.table(table(pred=pred.train,train=y)))
sum(diag(contingency))
profit.train <- WeekDatTrain$PUTWkRet*as.numeric(as.character(pred.train))
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
WeekDatdf <- as.data.frame(coredata(WeekDatTest))
WeekDatdf$Direction <- as.factor(WeekDatdf$Direction)
pred.test <- predict(svmfit1,newdata = WeekDatdf)
y <- ifelse(WeekDatTest$PUTWkRet<(-5),-1,1)
(contingency <- prop.table(table(pred=pred.test,train=y)))
sum(diag(contingency))
profit.test <- WeekDatTest$PUTWkRet*as.numeric(as.character(pred.test))
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
WeekDat$Prediction <- ifelse(p3<(-5),-1,1)
y <- ifelse(WeekDat$PUTWkRet<(-5),-1,1)
WeekDatPred <- WeekDat

save(WeekDatPred,file="DataWork/WeekDatPred.Rdata")