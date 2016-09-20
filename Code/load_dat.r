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
names(VixDat) <- c("Date","BXM","SPTR","SPX","PUT","CLL","BXY","VIX","VXO","BXMD",
                   "BFLY","CLLZ","CMBO","CNDR","PPUT")
VixDat <- VixDat[(which(is.na(VixDat$SPTR)==FALSE)[1]):nrow(VixDat),] #cut after last N/A
VixDat <- xts(VixDat[,-1],VixDat$Date)
save(VixDat,file="DataWork/VixDat.Rdata")
