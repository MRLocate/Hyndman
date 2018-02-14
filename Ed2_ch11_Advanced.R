# http://otexts.org/fpp2/advanced.html
# Chapter 11 Four Advanced Forecasting Topics

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# Complex Seasonality
# Call Center Volume
p1 <- autoplot(calls) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(breaks=seq(1,33,by=2))
p2 <- autoplot(window(calls, end=4)) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(minor_breaks = seq(1,4,by=0.2))
gridExtra::grid.arrange(p1,p2)
calls %>%
  mstl() %>%
  autoplot()
fit <- auto.arima(calls, seasonal=FALSE, lambda=0,
                  xreg=fourier(calls, K=c(10,10)))
fc <- forecast(fit, xreg=fourier(calls, K=c(10,10), h=2*169))
autoplot(fc, include=5*169) +
  ylab("Call volume") + xlab("Weeks")
# Electricity Demand
autoplot(elecdemand[,c("Demand","Temperature")], facet=TRUE) +
  scale_x_continuous(minor_breaks=NULL,
                     breaks=2014+cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))/365,
                     labels=month.abb) +
  xlab("Time") + ylab("")
elecdemand %>%
  as.data.frame %>%
  ggplot(aes(x=Temperature, y=Demand)) + geom_point() +
  xlab("Temperature (degrees Celsius)") +
  ylab("Demand (GW)")
cooling <- pmax(elecdemand[,"Temperature"], 18)
fit <- auto.arima(elecdemand[,"Demand"],
                  xreg = cbind(fourier(elecdemand, c(10,10,0)),
                               heating=elecdemand[,"Temperature"],
                               cooling=cooling))
temps <- subset(elecdemand[,"Temperature"], start=NROW(elecdemand)-2*48+1)
fc <- forecast(fit, xreg=cbind(fourier(temps, c(10,10,0)),
                               heating=temps, cooling=pmax(temps,18)))
autoplot(fc, include=14*48)
checkresiduals(fc)

library(vars)
VARselect(uschange[,1:2], lag.max=8, type="const")[["selection"]]
var <- VAR(uschange[,1:2], p=3, type="const")
summary(var)
serial.test(var, lags.pt=10, type="PT.asymptotic")
forecast(var) %>%
  autoplot() + xlab("Year")

# Nueral Networks
# fit <- nnetar(sunspotarea, lambda=0)
# autoplot(forecast(fit,h=30))
# sim <- ts(matrix(0, nrow=30L, ncol=9L), start=end(sunspotarea)[1L]+1L)
# for(i in seq(9))
#   sim[,i] <- simulate(fit, nsim=30L)
# autoplot(sunspotarea) + autolayer(sim)
# fcast <- forecast(fit, PI=TRUE, h=30)
# autoplot(fcast)
