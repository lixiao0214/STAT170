#### Xiao Li
#### 904236938


# Import Data
dat <- read.csv("~/Desktop/STATS 170/Project/Installment 2/Li-904236938-installment2.csv")
names(dat)[5] <- "AD"
names(dat)[4] <- "N27"
names(dat)[2] <- "CUR"
names(dat)[3] <- "CLFPR"
project <- dat[505:(nrow(dat)-12),]
outofsample <- dat[820:831,]

# out-of-sample data for AD
ADoutofsample <- ts(outofsample$AD,st = c(2016,4), fr = 12)

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
random1 <- lnAD.decom1$random
acf(na.omit(random1), main = "ACF (Additive)",cex.main=0.7)

# Plot 7 
random2 <- lnAD.decom2$random
acf(na.omit(random2), main = "ACF (Multiplicative)",cex.main = 0.7)


##### Section 4
##### ARIMA Modeling

# a
acf(Y_t,lag=25, main = "Series Y*")
Y_t1 <- diff(Y_t,lag=1,differences =1)
acf(Y_t1,lag=25,main = "Series (1-B)Y*")
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
AIC(model1)
AIC(model2)
AIC(model3)


# e
# Ljung-box test
Box.test(model2$residuals, lag = 6, type = "Ljung-Box")

# Normality
hist(model2$residuals, main = "Histogram of residuals of ARIMA(1,1,0)(1,1,1)12")

# ACF of residuals
acf(model2$residuals, main = "ACF of residuals of ARIMA(1,1,0)(1,1,1)12")

# t-test
model2
t_ar1 <- -0.1927/0.0579
t_sar1 <- 0.2535/0.0695
t_sma1 <- -0.9487/0.0665

# g
Y.pred <- ts(predict(model2, n.ahead = 12,se.fit = TRUE))
cils1 <- ts((Y.pred$pred - 1.96 * Y.pred$se), start = c(2016,4),frequency = 12)
cius1 <- ts((Y.pred$pred + 1.96 * Y.pred$se), start = c(2016,4),frequency = 12)
ts.plot(cbind(ADoutofsample,exp(Y.pred$pred), exp(cils1), exp(cius1)), 
        lty = c(1, 2, 3, 3),  col=c("black","red","blue","blue"), 
        main = "Forecasted Value for Average Duration of Unemployment")

# h
r <- c()
for (i in 1:12){
  r[i] <- (ADoutofsample[i] - exp(Y.pred$pred)[i])^2
}
rmse <- sqrt(sum(r)/12)
rmse
