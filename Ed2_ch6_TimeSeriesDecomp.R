# http://otexts.org/fpp2/ch-decomposition.html
# Chapter 6 Time series decomposition

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# Moving average smoothing
autoplot(elecsales) + xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia")
ma5 <- ma(elecsales, 5)
ma5
autoplot(elecsales, series="Data") +
  forecast::autolayer(ma(elecsales,5), series="5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))
beer2 <- window(ausbeer,start=1992)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)
ma4
ma2x4
autoplot(elecequip, series="Data") +
  forecast::autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))
elecequip %>% decompose(type="multiplicative") %>% 
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of electrical equipment index")
# 6.4 X11 Decomposition
library(seasonal)
elecequip %>% seas(x11="") %>%
  autoplot() +
  ggtitle("X11 decomposition of electrical equipment index")
fit = seas(elecequip)
autoplot(elecequip, series="Data") +
  forecast::autolayer(trendcycle(fit), series="Trend") +
  forecast::autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))
ggsubseriesplot(seasonal(fit)) + ylab("Seasonal")
# 6.5 SEATS decomposition
library(seasonal)
elecequip %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of electrical equipment index")
# 6.6 STL decomposition
elecequip %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot
# Forecasting with Decomposition
fit <- stl(elecequip, t.window=13, s.window="periodic", robust=TRUE)
fit %>% seasadj() %>% naive() %>% autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")
fit %>% forecast(method="naive") %>% autoplot() + ylab("New orders index")
fcast <- stlf(elecequip, method='naive')
autoplot(fcast)
