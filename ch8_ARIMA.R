# https://www.otexts.org/fpp/8
# ARIMA models

# Load library of packages and data sets for Hyndman's book
library(fpp)

plot(a10, xlab="Year",
     ylab="Annual change in monthly log A10 sales")
plot(log(a10), xlab="Year",
     ylab="Annual change in monthly log A10 sales")
plot(diff(log(a10),12), xlab="Year",
     ylab="Annual change in monthly log A10 sales")

# Unit Root Testtest
# adf.test(x, alternative = "stationary")
# kpss.test(x)

# Number of Differences Required for Stationarity
# ns <- nsdiffs(x)
# if(ns > 0) {
#   xstar <- diff(x,lag=frequency(x),differences=ns)
# } else {
#   xstar <- x
# }
# nd <- ndiffs(xstar)
# if(nd > 0) {
#   xstar <- diff(xstar,differences=nd)
# }

fit <- auto.arima(usconsumption[,1],seasonal=FALSE)
plot(forecast(fit,h=10),include=80)
par(mfrow=c(1,2))
Acf(usconsumption[,1],main="")
Pacf(usconsumption[,1],main="")
par(mfrow=c(1,1))
fit <- Arima(usconsumption[,1], order=c(0,0,3))

# Example 8.2 Seasonally Adjusted Equipment Orders
eeadj <- seasadj(stl(elecequip, s.window="periodic"))
plot(eeadj)
tsdisplay(diff(eeadj),main="")
fit <- Arima(eeadj, order=c(3,1,1))
summary(fit)
Acf(residuals(fit))
Box.test(residuals(fit), lag=24, fitdf=4, type="Ljung")
plot(forecast(fit))

# Example 8.3 European Retail
plot(euretail, ylab="Retail index", xlab="Year")
tsdisplay(diff(euretail,4))
tsdisplay(diff(diff(euretail,4)))
fit <- Arima(euretail, order=c(0,1,1), seasonal=c(0,1,1))
tsdisplay(residuals(fit))
fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
res <- residuals(fit3)
tsdisplay(res)
Box.test(res, lag=16, fitdf=4, type="Ljung")
plot(forecast(fit3, h=12))
# Alternatively
auto.arima(euretail)

# Example 8.4 Cortecosteroid drug sales in Australia
lh02 <- log(h02)
par(mfrow=c(2,1))
plot(h02, ylab="H02 sales (million scripts)", xlab="Year")
plot(lh02, ylab="Log H02 sales", xlab="Year")
par(mfrow=c(1,1))
tsdisplay(diff(lh02,12), 
          main="Seasonally differenced H02 scripts", xlab="Year")
fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)
summary(fit)
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=6, type="Ljung")
fit <- auto.arima(h02)
summary(fit)
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=8, type="Ljung")
fit <- auto.arima(h02, lambda=0, d=0, D=1, max.order=9,
                  stepwise=FALSE, approximation=FALSE)
summary(fit)
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=8, type="Ljung")

getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}

getrmse(h02,h=24,order=c(3,0,0),seasonal=c(2,1,0),lambda=0)
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(2,1,0),lambda=0)
getrmse(h02,h=24,order=c(3,0,2),seasonal=c(2,1,0),lambda=0)
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,0),lambda=0)
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,1),lambda=0)
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(0,1,2),lambda=0)
getrmse(h02,h=24,order=c(3,0,1),seasonal=c(1,1,1),lambda=0)
getrmse(h02,h=24,order=c(4,0,3),seasonal=c(0,1,1),lambda=0)
getrmse(h02,h=24,order=c(3,0,3),seasonal=c(0,1,1),lambda=0)
getrmse(h02,h=24,order=c(4,0,2),seasonal=c(0,1,1),lambda=0)
getrmse(h02,h=24,order=c(3,0,2),seasonal=c(0,1,1),lambda=0)
getrmse(h02,h=24,order=c(2,1,3),seasonal=c(0,1,1),lambda=0)
getrmse(h02,h=24,order=c(2,1,4),seasonal=c(0,1,1),lambda=0)
getrmse(h02,h=24,order=c(2,1,5),seasonal=c(0,1,1),lambda=0)

fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)
plot(forecast(fit), ylab="H02 sales (million scripts)", xlab="Year")
