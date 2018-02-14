# https://www.otexts.org/fpp/2/1

# Load library of packages and data sets for Hyndman's book
library(fpp)

# Plot Airline Time Series
plot(melsyd[,"Economy.Class"], 
     main="Economy class passengers: Melbourne-Sydney",
     xlab="Year",ylab="Thousands")

# Plots of drug sales time series
plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")

seasonplot(a10,ylab="$ million", xlab="Year", 
           main="Seasonal plot: antidiabetic drug sales",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=19)

monthplot(a10,ylab="$ million",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: antidiabetic drug sales")
axis(1,at=1:12,labels=month.abb,cex=0.8)

# Scatterplots of Car Mileage Data
plot(jitter(fuel[,5]), jitter(fuel[,8]), xlab="City mpg", ylab="Carbon footprint")

pairs(fuel[,-c(1:2,4,7)], pch=19)

# Summary Statistics
fuel2 <- fuel[fuel$Litres<2,]
summary(fuel2[,"Carbon"])
sd(fuel2[,"Carbon"])

# Autocorrelation of the beer production time series
beer2 <- window(ausbeer, start=1992, end=2006-.1)
lag.plot(beer2, lags=9, do.lines=FALSE)
# correlogram
Acf(beer2)

# Random Time Series (White Noise)
set.seed(30)
x <- ts(rnorm(50))
plot(x, main="White noise")
Acf(x)

# 2.3 Some simple forecasting methods

# Average Method
# meanf(y, h) 
# y contains the time series
# h is the forecast horizon

# Naive Method
# naive(y, h)
# rwf(y, h) # Alternative

# Seasonal Naive Method
# snaive(y, h)

# Drift Method
# rwf(y, h, drift=TRUE)

# Examples
beer2 <- window(ausbeer,start=1992,end=2006-.1)
beerfit1 <- meanf(beer2, h=11)
beerfit2 <- naive(beer2, h=11)
beerfit3 <- snaive(beer2, h=11)

plot(beerfit1, plot.conf=FALSE, 
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
legend("topright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))

dj2 <- window(dj,end=250)
plot(dj2,main="Dow Jones Index (daily ending 15 Jul 94)",
     ylab="",xlab="Day",xlim=c(2,290))
lines(meanf(dj2,h=42)$mean,col=4)
lines(rwf(dj2,h=42)$mean,col=2)
lines(rwf(dj2,drift=TRUE,h=42)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))

# 2.4 Transformations and Adjustments
# BOX-COX & Logarithmic
plot(log(elec), ylab="Transformed electricity demand",
     xlab="Year", main="Transformed monthly electricity demand")
title(main="Log",line=-1)
# The BoxCox.lambda() function will choose a value of lambda for you.
lambda <- BoxCox.lambda(elec) # = 0.27
plot(BoxCox(elec,lambda))
lambda

# Calendar Adjustment
monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14)
monthdays[26 + (4*12)*(0:2)] <- 29
par(mfrow=c(2,1))
plot(milk, main="Monthly milk production per cow",
     ylab="Pounds",xlab="Years")
plot(milk/monthdays, main="Average milk production per cow per day", 
     ylab="Pounds", xlab="Years")

# 2.5 Assessing Forecast Accuracy
# Examples for seasonal time series (beer sales)
beer2 <- window(ausbeer,start=1992,end=2006-.1)

beerfit1 <- meanf(beer2,h=11)
beerfit2 <- rwf(beer2,h=11)
beerfit3 <- snaive(beer2,h=11)

par(mfrow=c(1,1))
plot(beerfit1, plot.conf=FALSE,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
lines(ausbeer)
legend("topright", lty=1, col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))

beer3 <- window(ausbeer, start=2006)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

# Example for non-seasonal time series (Dow Jones)
dj2 <- window(dj, end=250)
plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)",
     ylab="", xlab="Day", xlim=c(2,290))
lines(meanf(dj2,h=42)$mean, col=4)
lines(rwf(dj2,h=42)$mean, col=2)
lines(rwf(dj2,drift=TRUE,h=42)$mean, col=3)
legend("topleft", lty=1, col=c(4,2,3),
       legend=c("Mean method","Naive method","Drift method"))
lines(dj)

dj3 <- window(dj, start=251)
accuracy(meanf(dj2,h=42), dj3)
accuracy(rwf(dj2,h=42), dj3)
accuracy(rwf(dj2,drift=TRUE,h=42), dj3)

# 2.6 Residual Diagnostics
# Example - Dow Jones
dj2 <- window(dj, end=250)
plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)", 
     ylab="", xlab="Day")
res <- residuals(naive(dj2))
plot(res, main="Residuals from naive method", 
     ylab="", xlab="Day")
Acf(res, main="ACF of residuals")
hist(res, nclass="FD", main="Histogram of residuals")
# Portmanteau tests for autocorrelation
# lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
Box.test(res,lag=10, fitdf=0, type="Lj")

# 2.8 Exercises
# data(package="fma")
dole2 <- window(dole, start=1956, end=1993-.5)
plot(dole2, main="Monthly Total of Persons on Unemployed Benefits in Australia")
plot(usdeaths, main="Total Accidental Deaths in US")
