# http://otexts.org/fpp2/sec-2-methods.html
# Chapter 3 The forecaster’s toolbox

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# 3.1 Some simple forecasting methods
# Average method
# meanf(y,h)
# y contains the time series
# h is the forecast horizon

# Naïve method
# naive(y, h)
# rwf(y, h) # Alternative

# Seasonal naïve method
# snaive(y, h)

# Drift method
# rwf(y, h, drift=TRUE)

# Examples
# Set training data from 1992-2007
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
  forecast::autolayer(meanf(beer2, h=11)$mean, series="Mean") +
  forecast::autolayer(naive(beer2, h=11)$mean, series="Naïve") +
  forecast::autolayer(snaive(beer2, h=11)$mean, series="Seasonal naïve") +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

# Set training data to first 250 days
dj2 <- window(dj,end=250)
# Plot some forecasts
autoplot(dj2) +
  forecast::autolayer(meanf(dj2, h=42)$mean, series="Mean") +
  forecast::autolayer(rwf(dj2, h=42)$mean, series="Naïve") +
  forecast::autolayer(rwf(dj2, drift=TRUE, h=42)$mean, series="Drift") +
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)") +
  xlab("Day") + ylab("") +
  guides(colour=guide_legend(title="Forecast"))

# 3.2 Transformations and adjustments
# Calendar adjustments
dframe <- cbind(Monthly = milk, DailyAverage=milk/monthdays(milk)) 
autoplot(dframe, facet=TRUE) + xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")
# Mathematical transformations
(lambda <- BoxCox.lambda(elec))
autoplot(BoxCox(elec,lambda))
# Bias adjustments
fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80, biasadj=TRUE)
autoplot(eggs) +
  forecast::autolayer(fc, series="Simple back transformation") +
  forecast::autolayer(fc2$mean, series="Bias adjusted") +
  guides(colour=guide_legend(title="Forecast"))

# 3.3 Residual diagnostics
# Example: Forecasting the Dow-Jones Index
dj2 <- window(dj, end=250)
autoplot(dj2) + xlab("Day") + ylab("") +
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)")
res <- residuals(naive(dj2))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")
gghistogram(res) + ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")
# Portmanteau tests for autocorrelation
# lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
Box.test(res,lag=10, fitdf=0, type="Lj")
checkresiduals(naive(dj2))

# 3.4 Evaluating forecast accuracy
# Functions to subset a time series
window(ausbeer, start=1995)
# extracts all data from 1995 onwards.
subset(ausbeer, start=length(ausbeer)-4*5)
# extracts the last 5 years of observations from ausbeer.
subset(ausbeer, quarter = 1)
# extracts the first quarters for all years.
# the last 5 years of ausbeer can also be obtained using:
tail(ausbeer, 4*5)
# Forecast errors
# Beer Example
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
  forecast::autolayer(beerfit1$mean, series="Mean") +
  forecast::autolayer(beerfit2$mean, series="Naïve") +
  forecast::autolayer(beerfit3$mean, series="Seasonal naïve") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))
beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)
# Dow Jones Example
dj2 <- window(dj, end=250)
djfc1 <- meanf(dj2, h=42)
djfc2 <- rwf(dj2, h=42)
djfc3 <- rwf(dj2, drift=TRUE, h=42)
autoplot(dj) +
  forecast::autolayer(djfc1$mean, series="Mean") +
  forecast::autolayer(djfc2$mean, series="Naïve") +
  forecast::autolayer(djfc3$mean, series="Drift") +
  xlab("Day") + ylab("") +
  ggtitle("Dow Jones Index (daily ending 15 Jul 94)") +
  guides(colour=guide_legend(title="Forecast"))
dj3 <- window(dj, start=251)
accuracy(djfc1, dj3)
accuracy(djfc2, dj3)
accuracy(djfc3, dj3)

# Time series cross-validation
e <- tsCV(dj, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(dj, drift=TRUE))^2, na.rm=TRUE))
# Using the pipe operator %>%
dj %>% tsCV(forecastfunction=rwf, drift=TRUE, h=1) -> e
e^2 %>% mean(na.rm=TRUE) %>% sqrt()
dj %>% rwf(drift=TRUE) %>% residuals() -> res
res^2 %>% mean(na.rm=TRUE) %>% sqrt()
# Example: using tsCV()
autoplot(goog)
e <- tsCV(goog, forecastfunction=naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

# 3.5 Prediction Intervals
# Table 3.1: Multipliers to be used for prediction intervals.
# Percentage	Multiplier
# 50	0.67
# 55	0.76
# 60	0.84
# 65	0.93
# 70	1.04
# 75	1.15
# 80	1.28
# 85	1.44
# 90	1.64
# 95	1.96
# 96	2.05
# 97	2.17
# 98	2.33
# 99	2.58
naive(dj2)
autoplot(naive(dj2))
naive(dj2, bootstrap=TRUE)

# 3.6 The forecast package in R
# The following list shows all the functions that produce forecast objects.
# meanf()
# naive(), snaive()
# rwf()
# croston()
# stlf()
# ses()
# holt(), hw()
# splinef()
# thetaf()
# forecast()
# forecast() function
forecast(ausbeer)
