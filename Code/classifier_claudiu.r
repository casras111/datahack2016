#Classifier with regression

library(quantmod)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(caret)

load("DataWork/VixDat.Rdata")
load("DataWork/WeekDat.Rdata")

WeekDat$PUTWkRet <- lag(WeekDat$PUTWkRet,-1) #dependent value
WeekDat <- WeekDat[-NROW(WeekDat),]          #remove last row with NA dependent

trainIndx <- createDataPartition(WeekDat$PUTWkRet,times=1,p=0.7,list=FALSE)

WeekDatTrain <- WeekDat[trainIndx]
WeekDatTest  <- WeekDat[-trainIndx]

prednames <- colnames(WeekDat)[grep("pred",colnames(WeekDat))]
f <- paste(prednames,collapse ="+")
reg1 <- lm(as.formula(paste0("PUTWkRet~",f)),data=WeekDatTrain)
summary(reg1)

p1 <- predict(reg1,newdata = WeekDatTest)

p3 <- predict(reg1,newdata = WeekDat)
WeekDat$Prediction <- ifelse(p3>0,1,-1)

WeekDatPred <- WeekDat

save(WeekDatPred,file="DataWork/WeekDatPred.Rdata")
