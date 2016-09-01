#clear everything
rm(list=ls())

#run the needed libraries
library(zoo)
library(dynlm)
library(e1071)
library(timeDate)
library(forecast)
library(timeSeries)
library(fBasics)
library(plotrix)
library(tseries)
library(nortest)


#Q1- Importing the data
#read data
data = read.csv("/Users/computer/Desktop/R/HW2/HW2.csv", header= TRUE)

#Q1- set variables
Date = data[,1]
Employment = data[,2]
Uncertainty = data[,3]
lUncertainty = tail(Uncertainty, 647)
dates = seq(as.Date("02/01/1960", format = "%m/%d/%Y"),
             by = "months", length = length(lUncertainty))

#Q2- Calculate the annualized growth of employment and plot it against uncertainty
dEmployment = diff(Employment)/Employment[-length(Employment)]
Growth0 = dEmployment+1
Growth1 = Growth0^12
Growth = Growth1-1
plot(dates, Growth, type="l", col="red", ylab ="Growth")
par(new=TRUE)
plot(dates,lUncertainty, type="l", col="blue",main = "All Employees: Total Nonfarm Payrolls", xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Uncertainty",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("Growth","Uncertainty"))

#Q3- Calculate general statistics of Growth and Uncertainty and plot the histogram 

#Summary Statistics of Employment Growth
Gmean = mean(Growth)
Gvar = var(Growth)
Gskew = skewness(Growth)
Gkurt = kurtosis(Growth)
Gmean
Gvar
Gskew
Gkurt
hist(Growth, main = "Distribution of Employment Growth", xlab ="Employment Growth")

#Summary Statistics of Uncertainty
Umean = mean(lUncertainty)
Uvar = var(lUncertainty)
Uskew = skewness(lUncertainty)
Ukurt = kurtosis(lUncertainty)
Umean
Uvar
Uskew
Ukurt
hist(lUncertainty, main = "Distribution of Uncertainty", xlab ="Uncertainty")


#Q4- Proform test on the data

#aa H0: M = 0, Ha: M =/= 0
Mis0.lUncertainty = t.test(lUncertainty, alternative = "two.sided", mu = 0, conf.level = 0.95)
print(Mis0.lUncertainty)

#bb H0: M = 0, Ha: M greater than 0
Mis1lUncertainty = t.test(lUncertainty, alternative = "greater", mu = 0, conf.level = 0.95)
print(Mis1lUncertainty)

##cc H0: Skewness = 0, Ha: Skewness =/= 0
sk.test = (Uskew/(sqrt(6/length(lUncertainty))))

##dd H0: Kurt = 0, Ha: Kurt =/= 0
ku.test = ((Ukurt-3)/(sqrt(24/length(lUncertainty))))


#Q5- Estimate an AR(4) for Employment Growth
Gar = arima(Growth, order = c(4,0,0))
Gar
coeftest(Gar)

#Q6- Estimate an optimal AR model for Employment Growth
Garbic = auto.arima(Growth, max.p = 12, max.d = 0, max.q = 0, seasonal = FALSE, ic = 'bic')
Garbic
coeftest(Garbic)
summary(Garbic)

#Q7- Estimate an autoregressive distributed lag model of employment Growth
adl = Arima(Growth, c(4,0,0), xreg = lUncertainty) 
adl
coeftest(adl)

#Q8- Construct the one-step-ahead conditional forecast
#Forcasting Growth one step ahead using AR4 model
arfGrowth = forecast(Gar, h=1, level = c(95), fan = FALSE)
arfGrowth
summary(arfGrowth)
t.test(fitted(arfGrowth), alternative = "two.sided", mu = 0, conf.level = 0.95)

#Forcasting Growth one step ahead using ADL model
Uarbic = auto.arima(lUncertainty, max.p = 12, max.d = 0, max.q = 0, seasonal = FALSE, ic = 'bic')
elUncertainty = predict(Uarbic, n.ahead = 1)
adlfGrowth = forecast.Arima(adl, level = c(95), fan = FALSE, xreg = elUncertainty$pred)
adlfGrowth
summary(adlfGrowth)
t.test(fitted(adlfGrowth), alternative = "two.sided", mu = 0, conf.level = 0.95)


#Q9- Repeat the forecasting for the last 10 years and construct the RMSFE associated with each of the forecasts
#Set Variables
LastGrowth = tail(Growth, 120)
LastlUncertainty = tail(lUncertainty, 120)

#Last 10 years for Growth
LGarbic = arima(LastGrowth, c(4,0,0))
LarfGrowth = forecast(LGarbic, h=1, level = c(95), fan = FALSE)
summary(LarfGrowth)

#Last 10 Years ADL
LUarbic = auto.arima(LastlUncertainty, max.p = 12, max.d = 0, max.q = 0, seasonal = FALSE, ic = 'bic')
LeUncertainty = predict(LUarbic, n.ahead = 1)
Ladl = auto.arima(LastGrowth, max.p = 4, max.q = 0, max.d = 0, start.p = 4, xreg = LastlUncertainty) 
LadlfGrowth = forecast.Arima(Ladl, level = c(95), fan = FALSE, xreg = LeUncertainty$pred)
summary(LadlfGrowth)
