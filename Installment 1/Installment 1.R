#### Xiao Li
#### 904236938


# Import Data
dat <- read.csv("~/Desktop/STATS 170/Project/Installment 1/Li-904236938.csv")
project <- dat[505:831,]
names(project)[5] <- "AD"
names(project)[4] <- "N27"
names(project)[2] <- "CUR"
names(project)[3] <- "CLFPR"

# Plot 1
AD <- ts(project$AD, st = c(1990,1), fr = 12)
N27 <- ts(project$N27, st = c(1990,1), fr = 12)
CUR <- ts(project$CUR, st = c(1990,1), fr = 12)
CLFPR <- ts(project$CLFPR, st = c(1990,1), fr = 12)
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
