#clear everything
rm(list = ls())

#run package 
library(car,lib.loc="/Users/computer/Desktop/R/Libraries")
library(zoo,lib.loc="/Users/computer/Desktop/R/Libraries")
library(sandwich,lib.loc="/Users/computer/Desktop/R/Libraries")
library(lmtest,lib.loc="/Users/computer/Desktop/R/Libraries")
library(strucchange,lib.loc="/Users/computer/Desktop/R/Libraries")
library(tseries,lib.loc="/Users/computer/Desktop/R/Libraries")
library(forecast,lib.loc="/Users/computer/Desktop/R/Libraries")



#read the data 
data <- read.csv("/Users/computer/Desktop/R/payems.csv")
data2 <- read.csv("/Users/computer/Desktop/R/unemp.csv")
unemp <- ts(c(data2[,2]),start=c(1948,1), frequency=4)
dgdp <- ts(c(data2[,3]),start=c(1948,1), frequency=4)
pay <- ts(c(data[,2]),start=c(1939,1), frequency=12)
date <- ts(c(data[,1]),start=c(1939,1), frequency=12)
dunemp <- diff(unemp)
regdata = ts.intersect(dgdp,dunemp)

# Problem 1
#Q1
pay.model <- lm(pay ~ date)
pay.fit <- fitted(pay.model)
paydates <- seq(as.Date("1939/1/1"), as.Date("2015/10/1"),by ="month")
plot(paydates,pay.fit, type = "l", ylab="Trend", xlab="Date", lty=2, main="Payroll Employment Series")
par(new=T)
plot(pay.model$residuals, type = "l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("Gap",side=4,line=3)
legend("bottomright",col=c("black","blue"),inset=c(-0.15,-0.00),lty=c(2,1),legend=c("Trend","Gap"),bty = "n",ncol=3)

#test error correlation
acf(na.omit(pay.model$residuals),main="Gap correlation")
#the graph show clearly that data are correlated

#Q2
adf.test(na.omit(pay.model$residuals),alternative = "stationary")
adf.test(pay,alternative = "stationary")
#since the test on the residuals of the linear trend and full data result the same value we can 
#conclude that employee data is not stationary because of a stochastic trend (no determinestic trend).

#Q3
#data is non-stationary take diff to make it stationary
diffpay <- diff(pay)
adf.test(diffpay,alternative = "stationary")
#data is stationary now


# Problem 2
 
#Q1
#full data reg
uongctotalreg = lm(formula=dunemp~dgdp, data = as.data.frame(regdata))

#homo S.E.
uongco <- rollapply(zoo(regdata),
          width=60,
          FUN = function(regdata) 
          { 
            t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
          return(summary(t)$coefficients[2,1])
          },
          by.column=FALSE, align="right") 

uongse1 <- rollapply(zoo(regdata),
                width=60,
                FUN = function(regdata) 
                { 
                  t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
                  summary(t)$coefficients[2,1]-1.96*summary(t)$coefficients[2,2]
                },
                by.column=FALSE, align="right") 

uongse2 <- rollapply(zoo(regdata),
                 width=60,
                 FUN = function(regdata) 
                 { 
                   t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
                   summary(t)$coefficients[2,1]+1.96*summary(t)$coefficients[2,2]
                 },
                 by.column=FALSE, align="right") 


plot(uongco,type='n', ylim=c(-0.6,0.0), ylab="Coefficient", xlab="Year", main="Unemployment on GDP - Homoscedastic S.E.")
lines(uongco, col="black")
lines(uongse1, col="red", lty=2)
lines(uongse2, col="red", lty=2)
par(new=T)
abline(h=uongctotalreg$coefficients[2], col="brown", lty=4,lwd=2)


#hetro S.E.
uongcoh <- rollapply(zoo(regdata),
                width=60,
                FUN = function(regdata) 
                { 
                  t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
                  tt <- vcovHC(t);
                  ttt <- coeftest(t,vcov. = tt);
                  return(ttt[2,1])
                },
                by.column=FALSE, align="right") 

uongse1h <- rollapply(zoo(regdata),
                 width=60,
                 FUN = function(regdata) 
                 { 
                   t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
                   tt <- vcovHC(t);
                   ttt <- coeftest(t,vcov. = tt);
                   return(ttt[2,1]+1.96*ttt[2,2])
                 },
                 by.column=FALSE, align="right") 

uongse2h <- rollapply(zoo(regdata),
                 width=60,
                 FUN = function(regdata) 
                 { 
                   t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
                   tt <- vcovHC(t);
                   ttt <- coeftest(t,vcov. = tt);
                   return(ttt[2,1]-1.96*ttt[2,2])
                 },
                 by.column=FALSE, align="right") 


plot(uongcoh,type='n', ylim=c(-0.6,0.0), ylab="Coefficient", xlab="Year", main="Unemployment on GDP - Heteroskedastic S.E.")
lines(uongcoh, col="black")
lines(uongse1h, col="red", lty=2)
lines(uongse2h, col="red", lty=2)
abline(h=uongctotalreg$coefficients[2], col="brown", lty=4,lwd=2)

#we need to test coefficents to see if the relationship stable statistically (for example compare coefficients with 5 years interval)

uongcost <- rollapply(zoo(regdata),
                width=60,
                FUN = function(regdata) 
                { 
                  t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
                  return(summary(t)$coefficients[2,1])
                },
                by.column=FALSE, align="right") 

uongsest <- rollapply(zoo(regdata),
                   width=60,
                   FUN = function(regdata) 
                   { 
                     t = lm(formula=dunemp~dgdp, data = as.data.frame(regdata), na.rm=T); 
                     return(summary(t)$coefficients[2,2])
                   },
                   by.column=FALSE, align="right") 

st.tlist=vector("list",191)
for(i in 1:191){
  st.ttest <- (as.numeric(uongcost[i,])-as.numeric(uongcost[i+20,]))/as.numeric(uongsest[i])
  st.tlist[[i]]=st.ttest
}


plot(as.numeric(st.tlist), type="l",ylab="T-statistics",xaxt = "n", main="Coefficients Stability Test")
par(new=T)
abline(h=1.96, col="red")
par(new=T)
abline(h=-1.96, col="red")

#The relation is not stable over time since the coefficients became statically different sometimes
#Economecally Okun's law in not stable, because due to some economical events and 
#changes on regulations the relationship between Unemployment and GDP growth change in stringht in 
#favor of other parameters that determine GDP gowth and Unemployment rate which affect both by different 
#percentage and different periods depending on the relation between them.

#Q2
#full data reg
gonuctotalreg = lm(formula=dgdp~dunemp, data = as.data.frame(regdata))

#homo S.E.
gonuco <- rollapply(zoo(regdata),
                width=60,
                FUN = function(regdata) 
                { 
                  t = lm(formula=dgdp~dunemp, data = as.data.frame(regdata), na.rm=T); 
                  summary(t)$coefficients[2,1]
                },
                by.column=FALSE, align="right") 

gonuse1 <- rollapply(zoo(regdata),
                 width=60,
                 FUN = function(regdata) 
                 { 
                   t = lm(formula=dgdp~dunemp, data = as.data.frame(regdata), na.rm=T); 
                   summary(t)$coefficients[2,1]-1.96*summary(t)$coefficients[2,2]
                 },
                 by.column=FALSE, align="right") 

gonuse2 <- rollapply(zoo(regdata),
                 width=60,
                 FUN = function(regdata) 
                 { 
                   t = lm(formula=dgdp~dunemp, data = as.data.frame(regdata), na.rm=T); 
                   summary(t)$coefficients[2,1]+1.96*summary(t)$coefficients[2,2]
                 },
                 by.column=FALSE, align="right") 


plot(gonuco,type='n', ylim=c(-3,-0.1), ylab="Coefficient", xlab="Year", main="GDP on Unemployment - Homoscedastic S.E.")
lines(gonuco, col="black")
lines(gonuse1, col="red", lty=2)
lines(gonuse2, col="red", lty=2)
abline(h=gonuctotalreg$coefficients[2], col="brown", lty=4,lwd=2)

#hetro S.E. 
gonucoh <- rollapply(zoo(regdata),
                 width=60,
                 FUN = function(regdata) 
                 { 
                   t = lm(formula=dgdp~dunemp, data = as.data.frame(regdata), na.rm=T); 
                   tt <- vcovHC(t);
                   ttt <- coeftest(t,vcov. = tt);
                   return(ttt[2,1])
                 },
                 by.column=FALSE, align="right") 

gonuse1h <- rollapply(zoo(regdata),
                  width=60,
                  FUN = function(regdata) 
                  { 
                    t = lm(formula=dgdp~dunemp, data = as.data.frame(regdata), na.rm=T); 
                    tt <- vcovHC(t);
                    ttt <- coeftest(t,vcov. = tt);
                    return(ttt[2,1]+1.96*ttt[2,2])
                  },
                  by.column=FALSE, align="right") 

gonuse2h <- rollapply(zoo(regdata),
                  width=60,
                  FUN = function(regdata) 
                  { 
                    t = lm(formula=dgdp~dunemp, data = as.data.frame(regdata), na.rm=T); 
                    tt <- vcovHC(t);
                    ttt <- coeftest(t,vcov. = tt);
                    return(ttt[2,1]-1.96*ttt[2,2])
                  },
                  by.column=FALSE, align="right") 


plot(gonucoh,type='n', ylim=c(-3,-0.1), ylab="Coefficient", xlab="Year", main="GDP on Unemployment - Heteroskedastic S.E.")
lines(gonucoh, col="black")
lines(gonuse1h, col="red", lty=2)
lines(gonuse2h, col="red", lty=2)
abline(h=gonuctotalreg$coefficients[2], col="brown", lty=4,lwd=2)

#The regressions are different from eachothers because different factors affect both Unemployment and GDP growth 
#so the coefficient will be different because even if GDP growth affect Unemployment by sertain percentage 
#Unemployment does not affect GDP growth by the same percentage duo to other economecal factors that determain both 
#Unemployment rate and GDP growth.


#Q3 QLR

#for first regression
uongcoQLRtotal = lm(formula=dunemp~dgdp, data = as.data.frame(regdata))
ugQLR <- Fstats(uongcoQLRtotal, from = 0.15, to = 0.85, data = regdata)
plot(ugQLR, main="QLR Test - Unemployment on GDP")
#there might be a break at 1982
#for second regression
gonucoQLRtotal = lm(formula=dgdp~dunemp, data = as.data.frame(regdata))
guQLR <- Fstats(gonucoQLRtotal, from = 0.15, to = 0.85, data = regdata)
plot(guQLR,main="QLR Test - GDP on Unemployment")
#there might be a break at 2005