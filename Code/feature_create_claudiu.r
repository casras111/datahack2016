#Create new features from source data

library(quantmod)

load("DataWork/VixDat.Rdata")

if (!("PUTRet" %in% colnames(VixDat))) {
  VixDat$PUTRet <- ROC(VixDat$PUT,type="discrete",na.pad=F)*100 #daily pct return
}
if (!("BFLYRet" %in% colnames(VixDat))) {
  VixDat$BFLYRet <- ROC(VixDat$BFLY,type="discrete",na.pad=F)*100 #daily pct return
}
if (!("SPTRRet" %in% colnames(VixDat))) {
  VixDat$SPTRRet <- ROC(VixDat$SPTR,type="discrete",na.pad=F)*100 #daily pct return
}
if (!("SPXRet" %in% colnames(VixDat))) {
  VixDat$SPXRet <- ROC(VixDat$SPX,type="discrete",na.pad=F)*100 #daily pct return
}
if (!("VXORet" %in% colnames(VixDat))) {
  VixDat$VXORet <- ROC(VixDat$VXO,type="discrete",na.pad=F)*100 #daily pct return
}

save(VixDat,file="DataWork/VixDat.Rdata")

PutWeekly  <- periodReturn(VixDat$PUT, period='weekly',type='arithmetic')
BflyWeekly <- periodReturn(VixDat$BFLY,period='weekly',type='arithmetic')
SPXWeekly  <- periodReturn(VixDat$SPX, period='weekly',type='arithmetic')
WeekDat <- merge(PutWeekly,BflyWeekly,SPXWeekly)
WeekDat <- WeekDat*100                        #scale to percentage
names(WeekDat) <- c("PUTWkRet","BFLYWkRet","SPXWkRet")

n <- 15 #number of predictors
oldn <- NCOL(WeekDat)
prenames <- paste0("pred",1:n)
for (i in 1:n) {
  WeekDat <- cbind(WeekDat,rep(NA,NROW(WeekDat)))
  names(WeekDat)[NCOL(WeekDat)] <- prenames[i]
}
for (i in 2:nrow(WeekDat)) {
  wksVXO <- VixDat[paste0(index(WeekDat$PUTWkRet[i-1]),"/",index(WeekDat$PUTWkRet[i])),"VXO"]
  wksVXO <- t(coredata(last(wksVXO,'5 days'))) #remove first day of earlier week
  WeekDat[i,(oldn+1):(oldn+length(wksVXO))] <- wksVXO
  wksSPX <- VixDat[paste0(index(WeekDat$PUTWkRet[i-1]),"/",index(WeekDat$PUTWkRet[i])),"SPXRet"]
  wksSPX <- t(coredata(last(wksSPX,'5 days')))
  WeekDat[i,(oldn+6):(oldn+5+length(wksSPX))] <- wksSPX
  wksPUT <- VixDat[paste0(index(WeekDat$PUTWkRet[i-1]),"/",index(WeekDat$PUTWkRet[i])),"PUTRet"]
  wksPUT <- t(coredata(last(wksPUT,'5 days')))
  WeekDat[i,(oldn+11):(oldn+10+length(wksPUT))] <- wksPUT
}
WeekDat <- WeekDat[-1]

WeekDat$pred16 <- ifelse(WeekDat$pred1<10,1,ifelse(WeekDat$pred1>25,3,2))
VixDat$pred17 <- MACD(VixDat$VXO)$macd
VixDat$pred18 <- MACD(VixDat$SPX)$macd
VixDat$pred19 <- MACD(VixDat$PUT)$macd
WeekDat <- merge(WeekDat,VixDat$pred17,join='left')
WeekDat <- merge(WeekDat,VixDat$pred18,join='left')
WeekDat <- merge(WeekDat,VixDat$pred19,join='left')

n <- 19 #number of predictors
prenames <- paste0("pred",1:n)

for (i in 1:19) {
  naindx <- which(is.na(WeekDat[,prenames[i]]))
  if (length(naindx)>0) {WeekDat <- WeekDat[-naindx,]}
}


save(WeekDat,file="DataWork/WeekDat.Rdata")
