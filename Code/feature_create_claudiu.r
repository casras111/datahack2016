#Create new features from source data

library(quantmod)

load("DataWork/VixDat.Rdata")

if (!("PUTRet" %in% colnames(VixDat))) {
  VixDat$PUTRet <- ROC(VixDat$PUT,type="discrete",na.pad=F)*100 #daily pct return
}

save(VixDat,file="DataWork/VixDat.Rdata")