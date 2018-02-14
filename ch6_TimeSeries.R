# https://www.otexts.org/fpp/6
# Time Series Decomposition

# Load library of packages and data sets for Hyndman's book
library(fpp)

# trend, seasonal, and cyclic variations
par(mfrow=c(2,2))
plot(hsales,xlab="Year",ylab="Monthly housing sales (millions)")
plot(ustreas,xlab="Day",ylab="US treasury bill contracts")
plot(elec,xlab="Year",ylab="Australian monthly electricity production")
plot(diff(dj),xlab="Day",ylab="Daily change in Dow Jones index")
par(mfrow=c(1,1))

# Example 6.1 - Electrical Equipment Manufacturing
fit <- stl(elecequip, s.window=5)
plot(elecequip, col="gray",
     main="Electrical equipment manufacturing",
     ylab="New orders index", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")
plot(fit)
monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal")

# Seasonally Adjusted Plot
plot(elecequip, col="grey",
     main="Electrical equipment manufacturing",
     xlab="", ylab="New orders index")
lines(seasadj(fit),col="red",ylab="Seasonally adjusted")

# Moving Average (5-period)
ma(elecsales, order=5)
plot(elecsales, main="Residential electricity sales",
     ylab="GWh", xlab="Year")
lines(ma(elecsales,5),col="red")

beer2 <- window(ausbeer,start=1992)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)

plot(elecequip, ylab="New orders index", col="gray",
     main="Electrical equipment manufacturing (Euro area)")
lines(ma(elecequip, order=12), col="red")

# x is the time series
# fit <- decompose(x, type="multiplicative")
# plot(fit)

fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit)

fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="New orders index",
     main="Naive forecasts of seasonally adjusted data")

fcast <- forecast(fit, method="naive")
plot(fcast, ylab="New orders index")
