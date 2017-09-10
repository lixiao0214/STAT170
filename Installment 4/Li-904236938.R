#### Xiao Li
#### 904236938


# Import Data
dat <- read.csv("~/Desktop/STATS 170/Project/Li-904236938.csv")
names(dat)[5] <- "AD"
names(dat)[4] <- "N27"
names(dat)[2] <- "CUR"
names(dat)[3] <- "CLFPR"
project <- dat[505:(nrow(dat)-12),]
outofsample <- dat[820:831,]

# out-of-sample data for AD
ADoutofsample <- ts(outofsample$AD,st = c(2016,4), fr = 12)
N27outofsample <- ts(outofsample$N27,st = c(2016,4), fr = 12)
CURoutofsample <- ts(outofsample$CUR,st = c(2016,4), fr = 12)
CLFPRoutofsample <- ts(outofsample$CLFPR,st = c(2016,4), fr = 12)

# Plot 1
AD <- ts(project$AD, st = c(1990,1), fr = 12)
N27 <- ts(project$N27, st = c(1990,1), fr = 12)
CUR <- ts(project$CUR, st = c(1990,1), fr = 12)
CLFPR <- ts(project$CLFPR, st = c(1990,1), fr = 12)

# summary statistics
mean(AD)
mean(N27)
mean(CUR)
mean(CLFPR)
sd(AD)
sd(N27)
sd(CUR)
sd(CLFPR)

par(mfrow=c(2,2))
plot(AD, main = "AD (average duration of unemployment in US)",cex.main=0.9)
plot(N27, main = "N27 (Number of Civilians Unemployed for 27 Weeks and Over in US)",cex.main=0.9)
plot(CUR, main = "CUR (Civilian Unemployment Rate in US)",cex.main=0.9)
plot(CLFPR, main = "CLFPR (Civilian Labor Force Participation Rate in US)",cex.main=0.9)

# log transformation 
lnAD <- ts(log(project$AD), st = c(1990,1), fr = 12)
lnN27 <- ts(log(project$N27), st = c(1990,1), fr = 12)
lnCUR <- ts(log(project$CUR), st = c(1990,1), fr = 12)
lnCLFPR <- ts(log(project$CLFPR), st = c(1990,1), fr = 12)
par(mfrow=c(1,1))
plot(lnAD, main = "log(AD) (average duration of unemployment in US)",cex.main=0.9)

mean(lnAD)
mean(lnN27)
mean(lnCUR)
mean(lnCLFPR)
sd(lnAD)
sd(lnN27)
sd(lnCUR)
sd(lnCLFPR)

# data Y*
Y_t <- lnAD

# Plot 2
AD.decom1 <- decompose(lnAD,type = "additive")
plot(AD.decom1)

# Plot 3
boxplot(lnAD~cycle(lnAD),xlab="Months", ylab="log(AD)",main="Seasonal Boxplot for log(AD)")

# Plot 4
AD.decom2 <- decompose(lnAD,type = "multiplicative")
plot(AD.decom2)

# Plot 5
AD.Seasonal2 <- AD.decom2$seasonal
plot(AD.Seasonal2)

# Plot 6
par(mfrow=c(1,2))
random1 <- AD.decom1$random
acf(na.omit(random1), main = "ACF (Additive)",cex.main=0.7)

# Plot 7 
random2 <- AD.decom2$random
acf(na.omit(random2), main = "ACF (Multiplicative)",cex.main = 0.7)
par(mfrow=c(1,1))

##### Section 4
##### ARIMA Modeling

# a
par(mfrow=c(1,2))
acf(Y_t,lag=25, main = "Series Y*")
Y_t1 <- diff(Y_t,lag=1,differences =1)
acf(Y_t1,lag=25,main = "Series (1-B)Y*")
par(mfrow=c(1,1))
Y_t2 <- diff(Y_t1, lag = 12,differences = 1)
acf(Y_t2,lag=25,main = "Series (1-B^12)(1-B)Y*")

# c
pacf(Y_t2,lag=25,main = "Series (1-B^12)(1-B)Y*")

# d
par(mfrow=c(1,2))
acf(Y_t2,lag=25,main = "ACF of Series Y**")
pacf(Y_t2,lag=25,main = "PACF of Series Y**")
par(mfrow=c(1,1))
model1 <- arima(Y_t,order = c(1,1,1),seasonal = list(order = c(1,1,1),12))
model2 <- arima(Y_t,order = c(1,1,0),seasonal = list(order = c(1,1,1),12))
model3 <- arima(Y_t,order = c(0,1,1),seasonal = list(order = c(1,1,1),12))
par(mfrow=c(1,3))
acf(model1$residuals, main = "ARIMA(1,1,1)(1,1,1)12")
acf(model2$residuals, main = "ARIMA(1,1,0)(1,1,1)12")
acf(model3$residuals, main = "ARIMA(0,1,1)(1,1,1)12")
par(mfrow=c(1,1))
AIC(model1)
AIC(model2)
AIC(model3)
model2$coef

# e
# Ljung-box test
Box.test(model2$residuals, lag = 6, type = "Ljung-Box")

# Normality
par(mfrow=c(1,2))
hist(model2$residuals, main = "Histogram of residuals",xlab = "residuals of ARIMA(1,1,0)(1,1,1)12")

# ACF of residuals
acf(model2$residuals, main = "ACF of residuals")
par(mfrow=c(1,1))

# t-test
model2
t_ar1 <- -0.1927/0.0579
t_sar1 <- 0.2535/0.0695
t_sma1 <- -0.9487/0.0665

# g
Y.pred <- ts(predict(model2, n.ahead = 12,se.fit = TRUE))
cils1 <- ts((Y.pred$pred - 1.96 * Y.pred$se), start = c(2016,4),frequency = 12)
cius1 <- ts((Y.pred$pred + 1.96 * Y.pred$se), start = c(2016,4),frequency = 12)
ts.plot(cbind(exp(Y_t),ADoutofsample,exp(Y.pred$pred), exp(cils1), exp(cius1)), 
        lty = c(1, 1, 2, 3, 3),  col=c("black","black","red","blue","blue"), 
        main = "Actual, Fitted and Forecasted Value for Average Duration of Unemployment")
legend("topleft",col = c("black","red","blue"),lty = c(1,2,2),legend = c("Actual","Forecast","CI"))

Y.pred <- ts(predict(model2, n.ahead = 12,se.fit = TRUE))
cils1 <- ts((Y.pred$pred - 1.96 * Y.pred$se), start = c(2016,4),frequency = 12)
cius1 <- ts((Y.pred$pred + 1.96 * Y.pred$se), start = c(2016,4),frequency = 12)
ts.plot(cbind(ADoutofsample,exp(Y.pred$pred), exp(cils1), exp(cius1)), 
        lty = c(1, 2, 3, 3),  col=c("black","red","blue","blue"),ylab="weeks",
        main = "Forecasted Value for Average Duration of Unemployment")
legend("topleft",col = c("blue","red","black"),lty = c(3,2,1),legend = c("CI","Pred","Actual"))

# h
exp(Y.pred$pred)
r <- c()
for (i in 1:12){
  r[i] <- (ADoutofsample[i] - exp(Y.pred$pred)[i])^2
}
rmse <- sqrt(sum(r)/12)
rmse


#### Section 5
#### Vector Autoregression


# N27
x1 <- lnN27
acf(x1)
x1_1 <- diff(x1, lag=1,differences = 1)
acf(x1_1)
x1_2 <- diff(x1_1,lag = 6,differences = 1)
acf(x1_2)

# CUR
x2 <- lnCUR
acf(x2)
x2_1 <- diff(x2,lag = 1,differences = 1)
acf(x2_1)
x2_2 <- diff(x2_1,lag = 12,differences = 1)
acf(x2_2)

# CLFPR
x3 <- lnCLFPR
acf(x3)
x3_1 <- diff(x3,lag = 1,differences = 1)
acf(x3_1)
x3_2 <- diff(x3_1,lag = 12,differences = 1)
acf(x3_2)

# Plot of ACF of all independent variables 
par(mfrow=c(1,3))

acf(x1_2,main="(1-B^6)(1-B)x1")
acf(x2_2,main="(1-B^12)(1-B)x2")
acf(x3_2,main="(1-B^12)(1-B)x3")
par(mfrow=c(1,1))

# ccf
par(mfrow=c(1,3))
ccf(Y_t2,x1_2,main="Y** & x1**")
ccf(Y_t2,x2_2,main="Y** & x2**")
ccf(Y_t2,x3_2,main="Y** & x3**")
par(mfrow=c(1,1))


# test unit root
library(tseries)
library(vars)
adf.test(Y_t)
adf.test(x1)   
adf.test(x2)
adf.test(x3)

# cointegration test
po.test(cbind(Y_t, x1))
po.test(cbind(Y_t, x2))
po.test(cbind(Y_t, x3))

#### VAR Model
d <- cbind(Y_t,x1,x2,x3)
AR.d <- ar(d, method="burg", dmean=T, intercept=F)
AR.d$order
var1 <- VAR(d,p=6)
coef(var1)
acf(resid(var1))

#### Impulse Response Function
library(vars)
var2 <- VAR(d, p = 6, type = "const")
irf1=irf(var2, impulse = "Y_t", response = c("Y_t","x1", "x2","x3"), boot =FALSE,n.ahead=150)
plot(irf1)

irf2=irf(var2, impulse = "x1", response = c("Y_t","x1", "x2","x3"), boot =FALSE,n.ahead=150)
plot(irf2)

irf3=irf(var2, impulse = "x2", response = c("Y_t","x1", "x2","x3"), boot =FALSE,n.ahead=150)
plot(irf3)

irf4=irf(var2, impulse = "x3", response = c("Y_t","x1", "x2","x3"), boot =FALSE,n.ahead=150)
plot(irf4)

# forecast
VAR.pred2 <- predict(var2,n.ahead = 12)
Y.pred2 <- ts(VAR.pred$fcst$Y_t[,1],st=c(2016,4),fr=12) 
cils2 <- ts(VAR.pred$fcst$Y_t[,2], start = c(2016,4),frequency = 12)
cius2 <- ts(VAR.pred$fcst$Y_t[,3], start = c(2016,4),frequency = 12)

ts.plot(cbind(exp(Y_t),ADoutofsample,exp(Y.pred2), exp(cils2), exp(cius2)), 
        lty = c(1, 1, 2, 3, 3),  col=c("black","black","red","blue","blue"), 
        main = "Actual, Fitted and Forecasted Value for Average Duration of Unemployment")
legend("topleft",col = c("black","red","blue"),lty = c(1,2,2),legend = c("Actual","Forecast","CI"))

ts.plot(cbind(ADoutofsample,exp(Y.pred2), exp(cils2), exp(cius2)), 
        lty = c(1, 2, 3, 3),  col=c("black","red","blue","blue"),ylab="weeks",
        main = "Forecasted Value for Average Duration of Unemployment")
legend("bottomleft",col = c("blue","red","black"),lty = c(3,2,1),legend = c("CI","Pred","Actual"))

exp(Y.pred2)
r <- c()
for (i in 1:12){
  r[i] <- (ADoutofsample[i] - exp(Y.pred2)[i])^2
}
rmse <- sqrt(sum(r)/12)
rmse


#### Time Series Regression
# Simple linear regression
model1 <- lm(Y_t ~x1+x2+x3+x3^2)
summary(model1)
par(mfrow=c(1,2))
plot(ts(model1$residuals),type="l",main="residual plot")
abline(h=0)
acf(ts(model1$residuals), main="ACF of random terms")
par(mfrow=c(1,1))

m <- cycle(Y_t)
times = time(Y_t)
T1 = 1:315
T2 = T1^2
T3 = T1^3
T4 = T1^4
model2 <- lm(Y_t ~T1+T2+T3+T4+T5+factor(m)+x1+x2+x3+x3^2)
summary(model2)
par(mfrow=c(1,3))
plot(ts(model2$residuals),type="l",main="residual plot")
abline(h=0)
acf(ts(model2$residuals), main="ACF of random terms")
pacf(ts(model2$residuals),main="PACF of random terms")
par(mfrow=c(1,1))

# fit AR(1) model
model3 <- arima(ts(model2$residuals),order=c(1,0,0),include.mean = F)
ar <- coef(model3)
acf(model3$residuals,main="ACF of random terms")
library(nlme)
modelgls <- gls(Y_t~T1+T2+T3+T4+T5+factor(m),correlation = corARMA(coef(model3),p=1))
summary(modelgls)

T11 = 316:327
T21 = T11^2
T31 = T11^3
T41 = T11^4
T51 = T11^5
new.dat = data.frame(m=factor(c(4:12,1,2,3)),T1=T11,T2=T21,T3=T31,T4=T41,T5=T51,
                     x1=N27outofsample,x2=CURoutofsample,x3=CLFPRoutofsample)
predicted = predict(modelgls,new.dat,se.fit=T)
hat.x.ts=ts(predicted,start=c(2016,4),fr=12)

ts.plot(exp(Y_t),exp(hat.x.ts),lty=1:2, col=c("black","red"),ylab="Weeks",xlab="Time", 
        main = "Actual, Fitted and Forecast Value")
lines(ADoutofsample,lty=1,col="black")
legend("topleft",legend= c("Actual","Forecast"),lty = c(1,2),col=c("black","red"))

ts.plot(ADoutofsample,exp(hat.x.ts),lty=1:2,col=c("black","red"), ylab="weeks",
        main = "Forecast Plot")
legend("topright",legend= c("Actual","Forecast"),lty = c(1,2),col=c("black","red"))

exp(hat.x.ts)
r <- c()
for (i in 1:12){
  r[i] <- (ADoutofsample[i] - exp(hat.x.ts)[i])^2
}
rmse <- sqrt(sum(r)/12)
rmse