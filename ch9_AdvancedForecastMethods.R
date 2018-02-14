# https://www.otexts.org/fpp/9
# Advanced forecasting methods

# Load library of packages and data sets for Hyndman's book
library(fpp)

# Example 9.1 US Personal Consumption and Income
plot(usconsumption, xlab="Year",
     main="Quarterly changes in US consumption and personal income")
fit <- Arima(usconsumption[,1], xreg=usconsumption[,2],
             order=c(2,0,0))
tsdisplay(residuals.Arima(fit, type = "regression"), main="ARIMA errors")
# refit with ARIMA(1,0,2)
(fit2 <- Arima(usconsumption[,1], xreg=usconsumption[,2],
               order=c(1,0,2)))
# Summary of ARIMA Regression Model
summary(fit2)
# Ljung-Box test
Box.test(residuals(fit2),fitdf=5,lag=10,type="Ljung")
# plotting the forecast
fcast <- forecast(fit2,xreg=rep(mean(usconsumption[,2]),8), h=8)
plot(fcast,
     main="Forecasts from regression with ARIMA(1,0,2) errors")
# Using the auto.Arima code to select the best ARIMA formulation
fit <- auto.arima(usconsumption[,1], xreg=usconsumption[,2])
summary(fit)

# Example 9.2 International visitors to Australia
# Deterministic Trend
auto.arima(austa,d=0,xreg=1:length(austa))
# Stochastic Trend
auto.arima(austa,d=1)
# Plotting the two forecasts together
fit1 <- Arima(austa, order=c(0,1,0), include.drift=TRUE)
fit2 <- Arima(austa, order=c(2,0,0), include.drift=TRUE)
par(mfrow=c(2,1))
plot(forecast(fit2), main="Forecasts from linear trend + AR(2) error",
     ylim=c(1,8))
plot(forecast(fit1), ylim=c(1,8))
par(mfrow=c(1,1))

# Example 9.3 TV advertising and insurance quotations
plot(insurance, main="Insurance advertising and quotations", xlab="Year")

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
colnames(Advert) <- paste("AdLag",0:3,sep="")

# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit1
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
fit2
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
fit3
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)
fit4

# Best model fitted to all data (based on AICc), (fit2)
# Refit using all data
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
fit
# Forecasts
fc8 <- forecast(fit, xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))), h=20)
plot(fc8, main="Forecast quotes with advertising set to 8", ylab="Quotes")

# Example 9.4 A VAR model for forecasting US consumption
library(vars)
VARselect(usconsumption, lag.max=8, type="const")$selection
var <- VAR(usconsumption, p=3, type="const")
serial.test(var, lags.pt=10, type="PT.asymptotic")
summary(var)
fcst <- forecast(var)
plot(fcst, xlab="Year")

# Example 9.5 Credit scoring neural network
library(caret)
creditlog  <- data.frame(score=credit$score,
                         log.savings=log(credit$savings+1),
                         log.income=log(credit$income+1),
                         log.address=log(credit$time.address+1),
                         log.employed=log(credit$time.employed+1),
                         fte=credit$fte, single=credit$single)
fit  <- avNNet(score ~ log.savings + log.income + log.address +
                 log.employed, data=creditlog, repeats=25, size=3, decay=0.1,
               linout=TRUE)
fit
summary(fit)

# Example 9.6: Sunspots
fit <- nnetar(sunspotarea)
plot(forecast(fit,h=20))
fit <- nnetar(sunspotarea,lambda=0)
plot(forecast(fit,h=20))

# Hiearchical Time Series
# library(hts)
# y <- hts(bts, nodes=list(2, c(3,2)))

# Example 9.7 Australian tourism hierarchy
library(hts)
y <- hts(vn, nodes=list(4,c(2,2,2,2)))
allf <- forecast(y, h=8)
plot(allf)
