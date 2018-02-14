# http://otexts.org/fpp2/arima.html
# Chapter 8 ARIMA models

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# Differencing to create stationarity
plot(dj)
plot(diff(dj))
Box.test(diff(dj), lag=10,type = "Ljung-Box")
# Seasonal Differences
cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")
# Double Differences
cbind("Billion kWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" = diff(log(usmelec),12),
      "Doubly\n differenced logs" = diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly US net electricity generation")
# Augmented Dickey Fuller
library(tseries)
adf.test(dj, alternative = "stationary")
adf.test(diff(dj), alternative = "stationary")
# KPSS Test
library(tseries)
kpss.test(dj)
kpss.test(diff(dj))
# ARIMA
autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change")
fit <- auto.arima(uschange[,"Consumption"], seasonal=FALSE)
summary(fit)
fit %>% forecast(h=10) %>% autoplot(include=80)
ggAcf(uschange[,"Consumption"],main="")
ggPacf(uschange[,"Consumption"],main="")
(fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0)))
(fit3 <- auto.arima(uschange[,"Consumption"], seasonal=FALSE,
                    stepwise=FALSE, approximation=FALSE))
# Example: Seasonally adjusted electrical equipment orders
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj 
autoplot(eeadj)
eeadj %>% diff() %>% ggtsdisplay(main="")
fit <- Arima(eeadj, order=c(3,1,1))
summary(fit)
checkresiduals(fit)
autoplot(forecast(fit))
# Seasonal ARIMA models
autoplot(euretail) + ylab("Retail index") + xlab("Year")
euretail %>% diff(lag=4) %>% ggtsdisplay
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay
euretail %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals %>%
  ggtsdisplay
fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))
checkresiduals(fit3)
fit3 %>% forecast(h=12) %>% autoplot
auto.arima(euretail)
auto.arima(euretail, stepwise=FALSE, approximation=FALSE)
# Example: Cortecosteroid drug sales in Australia
lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales"=lh02) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("")
lh02 %>% diff(lag=12) %>%
  ggtsdisplay(xlab="Year", main="Seasonally differenced H02 scripts")
(fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0))
checkresiduals(fit, lag=36)
h02 %>%
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")
# ARIMA vs ETS
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}
air <- window(ausair, start=1999)
# Compute CV errors for ETS as e1
e1 <- tsCV(air, fets, h=1)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(air, farima, h=1)
# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
#> [1] 7.86
mean(e2^2, na.rm=TRUE)
#> [1] 9.62
air %>% ets() %>% forecast() %>% autoplot()
air %>% auto.arima() %>% forecast() %>% autoplot()
# Consider the qcement data beginning in 1988
cement <- window(qcement, start=1988)
# Use 20 years of the data as the training set
train <- window(cement, end=c(2007,4))
# Fit an ARIMA model to the training data
(fit.arima <- auto.arima(train))
#> Series: train 
#> ARIMA(2,0,0)(2,1,1)[4] with drift 
#> 
#> Coefficients:
#>         ar1    ar2   sar1    sar2    sma1  drift
#>       0.647  0.193  0.073  -0.240  -0.870  0.010
#> s.e.  0.116  0.118  0.160   0.139   0.162  0.003
#> 
#> sigma^2 estimated as 0.0116:  log likelihood=61.5
#> AIC=-109   AICc=-107   BIC=-92.6
checkresiduals(fit.arima)
# Fit an ETS model to the training data
(fit.ets <- ets(train))
#> ETS(M,N,M) 
#> 
#> Call:
#>  ets(y = train) 
#> 
#>   Smoothing parameters:
#>     alpha = 0.7341 
#>     gamma = 1e-04 
#> 
#>   Initial states:
#>     l = 1.6439 
#>     s=1.03 1.04 1.01 0.915
#> 
#>   sigma:  0.0559
#> 
#>    AIC   AICc    BIC 
#> -2.197 -0.641 14.477
checkresiduals(fit.ets)
# Generate forecasts and compare accuracy over the test set
fit.arima %>% forecast(h = 4*(2013-2007)+1) %>% accuracy(qcement)
#>                    ME  RMSE    MAE    MPE MAPE  MASE    ACF1 Theil's U
#> Training set -0.00585 0.101 0.0795 -0.655 4.35 0.543 -0.0132        NA
#> Test set     -0.15836 0.198 0.1675 -7.300 7.65 1.144  0.2885     0.723
fit.ets %>% forecast(h = 4*(2013-2007)+1) %>% accuracy(qcement)
#>                   ME  RMSE    MAE    MPE MAPE  MASE    ACF1 Theil's U
#> Training set  0.0141 0.102 0.0796  0.494 4.37 0.544 -0.0335        NA
#> Test set     -0.1350 0.184 0.1540 -6.251 6.99 1.052  0.5344     0.681
# Generate forecasts from an ARIMA model 
cement %>% auto.arima() %>% forecast(h=12) %>% autoplot()
# Generate forecasts from an ETS model 
cement %>% ets() %>% forecast(h=12) %>% autoplot()
