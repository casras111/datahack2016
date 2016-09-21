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
