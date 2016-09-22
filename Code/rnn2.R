library(rnn)

setwd("C:/Users/Mor/Documents/datahack/datahack2016")

#y <- (VixDat$PUT,index(VixDat))
#y <- VixDat$PUT
#print(y)
#x <- VixDat[,'VXO']
#x$SPTR <- VixDat$SPTR 

#print(dim(x$SPTR))
#print(dim(x$VXO))

#print(dim(X))
#print(x)
#trainr(y,x)
y <- VixDat[1:1000,'PUT']
x <- VixDat[1:1000,'SPTR']
z <- array(VixDat[,'SPTR'] ,dim=c(1,1000,4))
t <-     matrix(y, ncol=1000)

print(z)
#x <- array(x,dim=(1))
#print(y)
model <- trainr(X = z,
                Y = y,
                learningrate = 0.01,
                numepochs = 50,
                hidden_dim = 10,seq_to_seq_unsync = TRUE)