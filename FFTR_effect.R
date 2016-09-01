#Clear everything & run required libraries
rm(list = ls())
library(zoo)
library(xts)
library(Quandl)
#Import data from FRED, rename variables, clean data & create loop list
ratedata <-
  Quandl(c("FRED/DFEDTAR","FRED/DTB3","FRED/DTB6","FRED/DGS1","FRED/DGS3","FRED/DGS5","FRED/DGS7","FRED/DGS10"),
         type="zoo",collapse="daily")
colnames(ratedata) <- c("TAR","3 Months","6 Months","1 Year","3 Years","5 Years","7 Years","10 Years")
ratel0 <- diff((ratedata),lag = 1L)
ratel1 <- ratel0[!(ratel0$TAR==0),]
changes <- ratel1[!(ratel1$"3 Months"=="NA"),]
typelist <- names(changes)[2:8]
#Run regressions (Full Period)
FInterceptV <- sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = changes))$coefficients[1,1]*100,1)})
FInterceptT <- sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = changes))$coefficients[1,3],1)})
FResponseV <- sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = changes))$coefficients[2,1]*100,1)})
FResponseT <- sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = changes))$coefficients[2,3],1)})
FRsquared <- sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = changes))$adj.r.squared*100,1)})
#Set interval & run regressions (Different Intervals)
ep <- as.zoo(c(0,67,84,113,131,NROW(changes)))
InterceptV <- period.apply(changes, ep, FUN=function(y){ sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = y))$coefficients[1,1]*100,1)})})
InterceptT <- period.apply(changes, ep, FUN=function(y){ sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = y))$coefficients[1,3],1)})})
ResponseV <- period.apply(changes, ep, FUN=function(y){ sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = y))$coefficients[2,1]*100,1)})})
ResponseT <- period.apply(changes, ep, FUN=function(y){ sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = y))$coefficients[2,3],1)})})
Rsquared <- period.apply(changes, ep, FUN=function(y){ sapply(typelist, function(x){
  round(summary(lm(substitute(i ~ TAR, list(i = as.name(x))), data = y))$adj.r.squared*100,1)})})
#Print results
FInterceptV
FInterceptT
FResponseV
FResponseT
FRsquared
InterceptV
InterceptT
ResponseV
ResponseT
Rsquared