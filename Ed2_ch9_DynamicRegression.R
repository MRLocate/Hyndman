# http://otexts.org/fpp2/dynamic.html
# Chapter 9 Dynamic regression models

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# Example: US Personal Consumption and Income
autoplot(uschange[,1:2], facets=TRUE) +
  xlab("Year") + ylab("") + 
  ggtitle("Quarterly changes in US consumption and personal income")
(fit <- auto.arima(uschange[,"Consumption"], xreg=uschange[,"Income"]))
#> Series: uschange[, "Consumption"] 
#> Regression with ARIMA(1,0,2) errors 
#> 
#> Coefficients:
#>         ar1     ma1    ma2  intercept   xreg
#>       0.692  -0.576  0.198      0.599  0.203
#> s.e.  0.116   0.130  0.076      0.088  0.046
#> 
#> sigma^2 estimated as 0.322:  log likelihood=-157
#> AIC=326   AICc=326   BIC=345
cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)
checkresiduals(fit)
# Forecasting
fcast <- forecast(fit, xreg=rep(mean(uschange[,2]),8))
autoplot(fcast) + xlab("Year") +
  ylab("Percentage change")
# Example: Forecasting electricity demand
plot(elecdemand[,3],elecdemand[,1])
autoplot(elecdemand[,1])
autoplot(elecdemand[,3])
xreg <- cbind(MaxTemp = elecdemand[, "Temperature"], 
              MaxTempSq = elecdemand[, "Temperature"]^2, 
              Workday = elecdemand[, "WorkDay"])
fit <- auto.arima(elecdemand[, "Demand"], xreg = xreg)
checkresiduals(fit)
summary(fit)
#>  Ljung-Box test
#> 
#> data:  Residuals from Regression with ARIMA(2,1,2)(2,0,0)[7] errors
#> Q* = 30, df = 4, p-value = 1e-05
#> 
#> Model df: 10.   Total lags used: 14
autoplot(elecdemand[,'Demand'], series="Data") +
  forecast::autolayer(fitted(fit), series="Fitted") +
  ylab("") +
  ggtitle("Daily electricity demand (GW)") +
  guides(colour=guide_legend(title=" "))
fcast <- forecast(fit, 
                  xreg = cbind(rep(26,14), rep(26^2,14), 
                               c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast) + ylab("Electicity demand (GW)")
# Stochastic and deterministic trends
# Example: International visitors to Australia
autoplot(austa) + xlab("Year") +
  ylab("millions of people") +
  ggtitle("Total annual international visitors to Australia")
trend <- seq_along(austa)
(fit1 <- auto.arima(austa, d=0, xreg=trend))
#> Series: austa 
#> Regression with ARIMA(2,0,0) errors 
#> 
#> Coefficients:
#>        ar1     ar2  intercept   xreg
#>       1.11  -0.380      0.416  0.171
#> s.e.  0.16   0.158      0.190  0.009
#> 
#> sigma^2 estimated as 0.0298:  log likelihood=13.6
#> AIC=-17.2   AICc=-15.2   BIC=-9.28
(fit2 <- auto.arima(austa, d=1))
#> Series: austa 
#> ARIMA(0,1,1) with drift 
#> 
#> Coefficients:
#>         ma1  drift
#>       0.301  0.173
#> s.e.  0.165  0.039
#> 
#> sigma^2 estimated as 0.0338:  log likelihood=10.6
#> AIC=-15.2   AICc=-14.5   BIC=-10.6
fc1 <- forecast(fit1, xreg=data.frame(trend=length(austa)+1:10))
fc2 <- forecast(fit2, h=10)
autoplot(austa) +
  forecast::autolayer(fc2, series="Stochastic trend") +
  forecast::autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from deterministic and stochastic trend models") +
  xlab("Year") + ylab("Visitors to Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))
# 9.5 Dynamic harmonic regression
# Example: Australian eating out expenditure
cafe04 <- window(auscafe, start=2004)
plots <- list()
for (i in 1:6) {
  fit <- auto.arima(cafe04, xreg = fourier(cafe04, K = i), seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,xreg=fourier(cafe04, K=i, h=24))) +
    xlab(paste("K=",i,"   AICC=",round(fit$aicc,2)))+ylab("") + ylim(1.5,4.7) 
}
gridExtra::grid.arrange(plots[[1]],plots[[2]],plots[[3]],
                        plots[[4]],plots[[5]],plots[[6]], nrow=3)
# 9.6 Lagged predictors
# Example: TV advertising and insurance quotations
autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")
# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = lag(insurance[,"TV.advert"],-1),
  AdLag2 = lag(insurance[,"TV.advert"],-2),
  AdLag3 = lag(insurance[,"TV.advert"],-3))[1:NROW(insurance),]
# Choose optimal lag length for advertising based on AICc
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)
# Best model fitted to all data (based on AICc)
# Refit using all data
(fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0))
#> Series: insurance[, 1] 
#> Regression with ARIMA(3,0,0) errors 
#> 
#> Coefficients:
#>        ar1     ar2    ar3  intercept  AdLag0  AdLag1
#>       1.41  -0.932  0.359      2.039   1.256   0.162
#> s.e.  0.17   0.255  0.159      0.993   0.067   0.059
#> 
#> sigma^2 estimated as 0.217:  log likelihood=-23.9
#> AIC=61.8   AICc=65.3   BIC=73.6
fc8 <- forecast(fit, h=20,
                xreg=cbind(AdLag0=rep(8,20), AdLag1=c(Advert[40,1],rep(8,19))))
autoplot(fc8) + ylab("Quotes") +
  ggtitle("Forecast quotes with future advertising set to 8")
