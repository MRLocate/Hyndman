# http://otexts.org/fpp2/expsmooth.html
# Chapter 7 Exponential smoothing

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# Simple Exponential Smoothing
oildata <- window(oil, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")
# Estimate parameters
fc <- ses(oildata, h=5)
# Accuracy of one-step-ahead training errors over period 1--12
round(accuracy(fc),2)
summary(fc)
autoplot(fc) +
  forecast::autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")
# Holt’s linear trend method
air <- window(ausair, start=1990)
autoplot(air) +
  ggtitle("Air passengers in Australia") +
  xlab("Year") + ylab("millions of passengers")
fc <- holt(air, h=5)
autoplot(fc)
# Damped trend methods
fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  forecast::autolayer(fc$mean, series="Holt's method") +
  forecast::autolayer(fc2$mean, series="Damped Holt's method") +
  ggtitle("Forecasts from Holt's method") +
  xlab("Year") + ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))
autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")
e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)
fc <- holt(livestock)
fc[["model"]]
autoplot(fc) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")
# Holt-Winters’ seasonal method
aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  forecast::autolayer(fit1$mean, series="HW additive forecasts") +
  forecast::autolayer(fit2$mean, series="HW multiplicative forecasts") +
  xlab("Year") + ylab("International visitor night in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))
summary(fit1)
summary(fit2)
fit3 <- hw(aust, damped=TRUE, seasonal="multiplicative")
summary(fit3)
autoplot(aust) +
  forecast::autolayer(fit1$mean, series="HW additive forecasts") +
  forecast::autolayer(fit2$mean, series="HW multiplicative forecasts") +
  forecast::autolayer(fit3$mean, series="HW damped mult forecasts") +
  xlab("Year") + ylab("International visitor night in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))
fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
         damped = TRUE, seasonal="multiplicative", h=35)
autoplot(hyndsight) +
  forecast::autolayer(fc$mean, series="HW multi damped")+
  guides(colour=guide_legend(title="Daily forecasts"))
# Estimation and model selection
aust <- window(austourists, start=2005)
fit <- ets(aust)
summary(fit)
autoplot(fit)
cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit, type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")
# Forecasting with ETS models
# forecast(object, h=ifelse(object$m>1, 2*object$m, 10),
#          level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE, npaths=5000,
#          PI=TRUE, lambda=object$lambda, biasadj=NULL, ...)
fit %>% forecast(h=8) %>%
  autoplot() +
  ylab("International visitor night in Australia (millions)")
