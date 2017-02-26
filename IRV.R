#clear everything
rm(list = ls())

#run package 
library(vars)
library(tseries)
library(forecast)
library(graphics)
library(fanplot)


#read the data 
data <- read.csv("/Users/computer/Desktop/R/HW5DATA.csv")
date <- ts(c(data[,1]),start=c(1959,1), frequency=4)
gdp <- ts(c(data[,2]),start=c(1958,1), frequency=4)
ffr <- ts(c(data[,3]),start=c(1958,1), frequency=4)
cpi <- ts(c(data[,4]),start=c(1958,1), frequency=4)
wuxia <- ts(c(data[,5]),start=c(1958,1), frequency=4)

#Q2

adf.test(gdp, alternative = "stationary")
adf.test(ffr, alternative = "stationary")
adf.test(cpi, alternative = "stationary")
adf.test(wuxia, alternative = "stationary")


dgdp <- round((gdp-lag(gdp,-4)),5)
dffr <- round((ffr-lag(ffr,-4)),5)
dcpi <- round((cpi-lag(cpi,-4)),5)
dwuxia <- round((wuxia-lag(wuxia,-4)),5)


adf.test(dgdp, alternative = "stationary")
adf.test(dffr, alternative = "stationary")
adf.test(dcpi, alternative = "stationary")
adf.test(dwuxia, alternative = "stationary")

#all stationary except CPI, take the second difference to make it stationary.

ddcpi <- round((dcpi-lag(dcpi,-4)),5)
adf.test(ddcpi, alternative = "stationary")

#all data are stationary.

grouped <- ts.intersect(gdp,cpi,ffr,wuxia)
ddgrouped <- ts.intersect(dgdp,ddcpi,dffr,dwuxia)
regdata <- ts.intersect(dgdp,ddcpi,dffr)
regknown <- window(regdata, end=c(2007,4), frequency=4)
regknown80 <- window(regdata, start=c(1980,1),end=c(2007,4), frequency=4)
regknown90 <- window(regdata, start=c(1990,1),end=c(2007,4), frequency=4)
pdwuxia <- window(dwuxia, start=c(1960,1),end=c(2015,3), frequency=4)
regunknown <- window(regdata, start=c(2008,1), frequency=4)
plot(ddgrouped, main="Transformed Series - from 1960 to 2015",lwd=2)
plot(regknown, main="Transformed Series - from 1960 to 2007")

#Q3

colnames(regknown) <- c("dgdp","ddcpi", "dffr")
checklag <- VARselect(regknown, lag.max = 75, type = "const", season = NULL)
sellectedlag <- as.numeric(checklag$selection[1])
vary <- VAR(regknown, p = sellectedlag, lag.max = NULL, ic="AIC")
summary(vary)
predicty <- predict(vary, n.ahead = 31, ci = 0.95)

plot(regdata[,1],type="l",ylim=range(c(regdata[,1],predicty$fcst$dgdp[,2],predicty$fcst$dgdp[,3])),ylab="Value", lwd=3)
par(new=T)
plot(predicty,names="dgdp",xaxt="n", lwd=2)

plot(regdata[,3],type="l",ylim=range(c(regdata[,3],predicty$fcst$dffr[,2],predicty$fcst$dffr[,3])),ylab="Value", lwd=3)
par(new=T)
plot(predicty,names="dffr",xaxt="n", lwd=2)

plot(pdwuxia,type="l",ylim=range(c(pdwuxia,predicty$fcst$dffr[,2],predicty$fcst$dffr[,3])),ylab="Value", lwd=3)
par(new=T)
plot(predicty,names="dffr",xaxt="n", lwd=2,main="Forecast of series dffr (using Wu-Xia as realization)")

plot(ddcpi,type="l",ylim=range(c(ddcpi,predicty$fcst$ddcpi[,2],predicty$fcst$ddcpi[,3])),ylab="Value", lwd=3)
par(new=T)
plot(predicty,names="ddcpi",xaxt="n", lwd=2)



rmsedgdp <- sqrt(mean(summary(vary)$varresult$dgdp$residuals^2))
rmsedgdp
rmdsedgdp <- sqrt(median(summary(vary)$varresult$dgdp$residuals^2))
rmdsedgdp

rmseddcpi <- sqrt(mean(summary(vary)$varresult$ddcpi$residuals^2))
rmseddcpi
rmdseddcpi <- sqrt(median(summary(vary)$varresult$ddcpi$residuals^2))
rmdseddcpi

rmsedffr <- sqrt(mean(summary(vary)$varresult$dffr$residuals^2))
rmsedffr
rmdsedffr <- sqrt(median(summary(vary)$varresult$dffr$residuals^2))
rmdsedffr


#Q4

amat <- rbind(c(NA, 0, 0), c(NA, NA, 0), c(NA, NA, NA))
amat

svary <- SVAR(vary, max.iter = 1000, estmethod = "scoring", Amat = amat, Bmat = NULL)
summary(svary)

# Impulse Response Functions
irf1 <- irf(svary, impulse = "dffr", response = c("dgdp"),runs = 100, n.ahead = 20) 
plot(irf1)
irf2 <- irf(svary, impulse = "dffr", response = c("ddcpi"),runs = 100, n.ahead = 20)
plot(irf2)
irf3 <- irf(svary, impulse = "dffr", response = c("dffr"),runs = 100, n.ahead = 20)
plot(irf3)

#To find the most prominent shock
plot(irf(svary, impulse = "ddcpi", response = c("dgdp"),runs = 100, n.ahead = 20))
plot(irf(svary, impulse = "dgdp", response = c("dgdp"),runs = 100, n.ahead = 20))

# Forecast error decomposition
fevd(svary,n.ahead = 20)
plot(fevd(svary,n.ahead = 20))

#Q5
#Data from 1980 to 2007

colnames(regknown80) <- c("dgdp","ddcpi", "dffr")
checklag80 <- VARselect(regknown80, lag.max = 75, type = "const", season = NULL)
sellectedlag80 <- as.numeric(checklag80$selection[1])
vary80 <- VAR(regknown80, p = sellectedlag80, lag.max = NULL, ic="AIC")
summary(vary80)

amat80 <- rbind(c(NA, 0, 0), c(NA, NA, 0), c(NA, NA, NA))
amat80

svary80 <- SVAR(vary80, max.iter = 1000, estmethod = "scoring", Amat = amat, Bmat = NULL)
summary(svary80)

irf180 <- irf(svary80, impulse = "dffr", response = c("dgdp"),runs = 100, n.ahead = 20) 
plot(irf180)

irf280 <- irf(svary80, impulse = "dffr", response = c("ddcpi"),runs = 100, n.ahead = 20)
plot(irf280)

irf380 <- irf(svary80, impulse = "dffr", response = c("dffr"),runs = 100, n.ahead = 20)
plot(irf380)


#Data from 1990 to 2007

colnames(regknown90) <- c("dgdp","ddcpi", "dffr")
checklag90 <- VARselect(regknown90, lag.max = 70, type = "const", season = NULL)
sellectedlag90 <- as.numeric(checklag90$selection[1])
vary90 <- VAR(regknown90, p = sellectedlag90, lag.max = NULL, ic="AIC")
summary(vary90)

amat90 <- rbind(c(NA, 0, 0), c(NA, NA, 0), c(NA, NA, NA))
amat90

svary90 <- SVAR(vary90, max.iter = 1000, estmethod = "scoring", Amat = amat, Bmat = NULL)
summary(svary90)

irf190 <- irf(svary90, impulse = "dffr", response = c("dgdp"),runs = 100, n.ahead = 20) 
plot(irf190)

irf290 <- irf(svary90, impulse = "dffr", response = c("ddcpi"),runs = 100, n.ahead = 20)
plot(irf290)

irf390 <- irf(svary90, impulse = "dffr", response = c("dffr"),runs = 100, n.ahead = 20)
plot(irf390)


#Q6
#There will be no change on the forecast of outpout and prices since Wu-Xia covers only the 
#period from 2009 onward which is out of the sample used to build the model

regdatawu <- ts.intersect(dgdp,ddcpi,dwuxia)
colnames(regdatawu) <- c("dgdp","ddcpi","dwuxia")
checklagwu <- VARselect(regdatawu, lag.max = 75, type = "const", season = NULL)
sellectedlagwu <- as.numeric(checklagwu$selection[1])
varywu <- VAR(regdatawu, p = sellectedlagwu, lag.max = NULL, ic="AIC")
summary(varywu)
predictywu <- predict(varywu, n.ahead = 31, ci = 0.95)
fanchart(predictywu)