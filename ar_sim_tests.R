#Clear
rm(list = ls())

#Q1 to Q4
y=cumsum(rnorm(100)) 
x=cumsum(rnorm(100))
reg1= lm(y ~ x)
R2=summary(reg1)$r.squared
Rs=summary(reg1)$residuals
B1=summary(reg1)$coefficients[2,1]
SE=sqrt((1/100)*(((1/98)*sum(Rs^2))/var(x)))
Ttest=(B1-0)/SE
R2
Ttest

#Q5
R2100list= vector("list", 1000)
Ttest100list= vector("list", 1000)
for(i in 1:1000){
  y1=cumsum(rnorm(100))
  x1=cumsum(rnorm(100))
  reg2= lm(y1 ~ x1)
  R21=summary(reg2)$r.squared
  R2100list[[i]]= R21
  Rs1=summary(reg2)$residuals
  B11=summary(reg2)$coefficients[2,1]
  SE1=sqrt((1/100)*(((1/98)*sum(Rs1^2))/var(x1)))
  Ttest1=(B11-0)/SE1
  Ttest100list[[i]]= Ttest1
}
hist(as.numeric(R2100list), density=0, breaks=50, main="R2 (100 i.i.d.)", xlab="R2 value")
hist(as.numeric(Ttest100list), density=0, breaks=50, main="T-test (100 i.i.d.)", xlab="T value")
quantile(as.numeric(R2100list), c(.05,.5,.95))
quantile(as.numeric(Ttest100list), c(.05,.5,.95))
Reject_Null_Probability=sum(Ttest100list < -1.96, Ttest100list > 1.96)/1000
Reject_Null_Probability

#Q6
#Repeat Q5 with T = 50
R250list= vector("list", 1000)
Ttest50list= vector("list", 1000)
for(i in 1:1000){
  y5=cumsum(rnorm(50))
  x5=cumsum(rnorm(50))
  reg3= lm(y5 ~ x5)
  R25=summary(reg3)$r.squared
  R250list[[i]]= R25
  Rs5=summary(reg3)$residuals
  B15=summary(reg3)$coefficients[2,1]
  SE5=sqrt((1/50)*(((1/48)*sum(Rs5^2))/var(x5)))
  Ttest5=(B15-0)/SE5
  Ttest50list[[i]]= Ttest5
}
hist(as.numeric(R250list), density=0, breaks=50, main="R2 (50 i.i.d.)", xlab="R2 value")
hist(as.numeric(Ttest50list), density=0, breaks=50, main="T-test (50 i.i.d.)", xlab="T value")
quantile(as.numeric(R250list), c(.05,.5,.95))
quantile(as.numeric(Ttest50list), c(.05,.5,.95))
Reject_Null_Probability=sum(Ttest50list < -1.96, Ttest50list > 1.96)/1000
Reject_Null_Probability

#Repeat Q5 with T = 800
R2800list= vector("list", 1000)
Ttest800list= vector("list", 1000)
for(i in 1:1000){
  y8=cumsum(rnorm(800))
  x8=cumsum(rnorm(800))
  reg4= lm(y8 ~ x8)
  R28=summary(reg4)$r.squared
  R2800list[[i]]= R28
  Rs8=summary(reg4)$residuals
  B18=summary(reg4)$coefficients[2,1]
  SE8=sqrt((1/800)*(((1/798)*sum(Rs8^2))/var(x8)))
  Ttest8=(B18-0)/SE8
  Ttest800list[[i]]= Ttest8
}
hist(as.numeric(R2800list), density=0, breaks=50, main="R2 (800 i.i.d.)", xlab="R2 value")
hist(as.numeric(Ttest800list), density=0, breaks=50, main="T-test (800 i.i.d.)", xlab="T value")
quantile(as.numeric(R2800list), c(.05,.5,.95))
quantile(as.numeric(Ttest800list), c(.05,.5,.95))
Reject_Null_Probability=sum(Ttest800list < -1.96, Ttest800list > 1.96)/1000
Reject_Null_Probability

#Q7
ys=arima.sim(n=100,list(ar=0.5),innov = rnorm(100))
xs=arima.sim(n=100,list(ar=0.75),innov = rnorm(100))
reg8= lm(ys ~ xs)
R2s=summary(reg8)$r.squared
Rss=summary(reg8)$residuals
B1s=summary(reg8)$coefficients[2,1]
SEs=sqrt((1/100)*(((1/98)*sum(Rss^2))/var(xs)))
Ttests=(B1s-0)/SEs
R2s
Ttests

#Repeat Q5 for the new AR
R2100listS= vector("list", 1000)
Ttest100listS= vector("list", 1000)
for(i in 1:1000){
  ys1=arima.sim(n=100,list(ar=0.5),innov = rnorm(100))
  xs1=arima.sim(n=100,list(ar=0.75),innov = rnorm(100))
  reg5= lm(ys1 ~ xs1)
  R2s1=summary(reg5)$r.squared
  R2100listS[[i]]= R2s1
  Rss1=summary(reg5)$residuals
  B1s1=summary(reg5)$coefficients[2,1]
  SEs1=sqrt((1/100)*(((1/98)*sum(Rss1^2))/var(xs1)))
  Ttests1=(B1s1-0)/SEs1
  Ttest100listS[[i]]= Ttests1
}
hist(as.numeric(R2100listS), density=0, breaks=50, main="R2 (100 i.i.d., AR)", xlab="R2 value")
hist(as.numeric(Ttest100listS), density=0, breaks=50, main="T-test (100 i.i.d., AR)", xlab="T value")
quantile(as.numeric(R2100listS), c(.05,.5,.95))
quantile(as.numeric(Ttest100listS), c(.05,.5,.95))
Reject_Null_Probability=sum(Ttest100listS < -1.96, Ttest100listS > 1.96)/1000
Reject_Null_Probability

#Repeat Q6 for the new AR
#Repeat with T = 50
R250listS= vector("list", 1000)
Ttest50listS= vector("list", 1000)
for(i in 1:1000){
  ys5=arima.sim(n=50,list(ar=0.5),innov = rnorm(50))
  xs5=arima.sim(n=50,list(ar=0.75),innov = rnorm(50))
  reg6= lm(ys5 ~ xs5)
  R2s5=summary(reg6)$r.squared
  R250listS[[i]]= R2s5
  Rss5=summary(reg6)$residuals
  B1s5=summary(reg6)$coefficients[2,1]
  SEs5=sqrt((1/50)*(((1/48)*sum(Rss5^2))/var(xs5)))
  Ttests5=(B1s5-0)/SEs5
  Ttest50listS[[i]]= Ttests5
}
hist(as.numeric(R250listS), density=0, breaks=50, main="R2 (50 i.i.d., AR)", xlab="R2 value")
hist(as.numeric(Ttest50listS), density=0, breaks=50, main="T-test (50 i.i.d., AR)", xlab="T value")
quantile(as.numeric(R250listS), c(.05,.5,.95))
quantile(as.numeric(Ttest50listS), c(.05,.5,.95))
Reject_Null_Probability=sum(Ttest50listS < -1.96, Ttest50listS > 1.96)/1000
Reject_Null_Probability

#Repeat with T = 800
R2800listS= vector("list", 1000)
Ttest800listS= vector("list", 1000)
for(i in 1:1000){
  ys8=arima.sim(n=800,list(ar=0.5),innov = rnorm(800))
  xs8=arima.sim(n=800,list(ar=0.75),innov = rnorm(800))
  reg7= lm(ys8 ~ xs8)
  R2s8=summary(reg7)$r.squared
  R2800listS[[i]]= R2s8
  Rss8=summary(reg7)$residuals
  B1s8=summary(reg7)$coefficients[2,1]
  SEs8=sqrt((1/800)*(((1/798)*sum(Rss8^2))/var(xs8)))
  Ttests8=(B1s8-0)/SEs8
  Ttest800listS[[i]]= Ttests8
}
hist(as.numeric(R2800listS), density=0, breaks=50, main="R2 (800 i.i.d., AR)", xlab="R2 value")
hist(as.numeric(Ttest800listS), density=0, breaks=50, main="T-test (800 i.i.d., AR)", xlab="T value")
quantile(as.numeric(R2800listS), c(.05,.5,.95))
quantile(as.numeric(Ttest800listS), c(.05,.5,.95))
Reject_Null_Probability=sum(Ttest800listS < -1.96, Ttest800listS > 1.96)/1000
Reject_Null_Probability