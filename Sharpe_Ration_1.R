library(PerformanceAnalytics)
library(lubridate)

#Read data
berkshire=read.csv("M:/OMSA/MGT6203/berkshire.csv",header=TRUE)
berk=berkshire[0:350,]

#arithemetic avergae
br_mean=mean(berk$BrkRet)*100
br_mean

#standard deviation
br_sd=sd(berk$BrkRet)*100
br_sd

#Market Comparison 
mkt_mean=mean(berk$MKT)*100
mkt_mean
(br_mean-mkt_mean)/mkt_mean


#Sharpe Ratio
berk$Date=as.Date(berk$Date,format="%m/%d/%Y")
berk2 = berk[,-1]
rownames(berk2) = berk[,1]
head(berk2)

#berk sr
brsr=SharpeRatio(berk2[,1,drop=FALSE],berk2[,3,drop=FALSE],FUN="StdDev")
brsr

#market sr
mktsr=SharpeRatio(berk2[,2,drop=FALSE],berk2[,3,drop=FALSE],FUN="StdDev")
mktsr
