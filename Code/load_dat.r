#load files and clean up
#Indexes: SPTR - SP500 Total return, SPX - SP500, VIX, VXO
#Strategies indexes: BXM - covered call, BXY - OTM BXM, BXMD - OTM 30 delta BXM
#                    PUT - sell put, PPUT - index+buy put
#                    BFLY - butterfly buy, CNDR - condor buy
#                    CLL - collar, index + sell call+buy put, CLLZ - zero cost CLL
#                    CMBO - index+sell strangle

library(xlsx)
library(quantmod)

spath  <- file.path("DataRaw","dailypricehistory.xls")
VixDat <- read.xlsx(file=spath,sheetIndex=1,startRow = 4,endRow = 7694,stringsAsFactors=F,
                    colClasses = c("Date",rep("numeric",19)))
VixDat <- VixDat[,1:15] #drop junk last 5 columns
indxs <- c("BXM","SPTR","SPX","PUT","CLL","BXY","VIX","VXO","BXMD",
           "BFLY","CLLZ","CMBO","CNDR","PPUT")
names(VixDat) <- c("Date",indxs)
VixDat <- VixDat[(which(is.na(VixDat$SPTR)==FALSE)[1]):nrow(VixDat),] #cut after last N/A

#fix missing values in VXO
avg_impute <-  function(x,naindx) {
  mean(x[naindx-1],x[naindx+1])
}
naindexes <- which(is.na(VixDat$VXO))
for (i in naindexes) {VixDat[i,"VXO"] <- avg_impute(VixDat$VXO,i)} 

#fix missing values in VIX
naindexes <- which(is.na(VixDat$VIX))
naindexes <- naindexes[naindexes>401]  #VIX only available from 1990
for (i in naindexes) {VixDat[i,"VIX"] <- avg_impute(VixDat$VIX,i)} 

VixDat <- xts(VixDat[,-1],VixDat$Date)

anyNA(VixDat[,!(indxs %in% c("VIX"))]) #check it's FALSE, no missing NA

save(VixDat,file="DataWork/VixDat.Rdata")
