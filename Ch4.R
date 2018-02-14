# https://www.otexts.org/fpp/4
# Simple Regression

# Load library of packages and data sets for Hyndman's book
library(fpp)

# Example 4.1 Car Emisisons
plot(jitter(Carbon) ~ jitter(City),xlab="City (mpg)",
     ylab="Carbon footprint (tons per year)",data=fuel)
fit <- lm(Carbon ~ City, data=fuel)
abline(fit)
summary(fit)

res <- residuals(fit)
plot(jitter(res)~jitter(City), ylab="Residuals", xlab="City", data=fuel)
abline(0,0)

fitted(fit)[1]
fcast <- forecast(fit, newdata=data.frame(City=30))
plot(fcast, xlab="City (mpg)", ylab="Carbon footprint (tons per year)")
# The displayed graph uses jittering, while the code above does not.

confint(fit,level=0.95)

# 4.7 Non-linear Functional Forms
par(mfrow=c(1,2))
fit2 <- lm(log(Carbon) ~ log(City), data=fuel)
plot(jitter(Carbon) ~ jitter(City), xlab="City (mpg)",
     ylab="Carbon footprint (tonnes per year)", data=fuel)
lines(1:50, exp(fit2$coef[1]+fit2$coef[2]*log(1:50)))
plot(log(jitter(Carbon)) ~ log(jitter(City)), 
     xlab="log City mpg", ylab="log carbon footprint", data=fuel)
abline(fit2)
par(mfrow=c(1,1))

res <- residuals(fit2)
plot(jitter(res, amount=.005) ~ jitter(log(City)), 
     ylab="Residuals", xlab="log(City)", data=fuel)

# 4.8 Regression with time series
# Example 4.3 US Consumption
fit.ex3 <- tslm(consumption ~ income, data=usconsumption)
plot(usconsumption, ylab="% change in consumption and income",
     plot.type="single", col=1:2, xlab="Year")
legend("topright", legend=c("Consumption","Income"),
       lty=1, col=c(1,2), cex=.9)
plot(consumption ~ income, data=usconsumption, 
     ylab="% change in consumption", xlab="% change in income")
abline(fit.ex3)
summary(fit.ex3)

fcast <- forecast(fit.ex3, newdata=data.frame(income=c(-1,1)))
plot(fcast, ylab="% change in consumption", xlab="% change in income")

# Trend Analysis
fit.ex4 <- tslm(austa ~ trend)
f <- forecast(fit.ex4, h=5,level=c(80,95))
plot(f, ylab="International tourist arrivals to Australia (millions)",
     xlab="t")
lines(fitted(fit.ex4),col="blue")
summary(fit.ex4)

# Autocorrelation
par(mfrow=c(2,2))
res3 <- ts(resid(fit.ex3),s=1970.25,f=4)
plot.ts(res3,ylab="res (Consumption)")
abline(0,0)
Acf(res3)
res4 <- resid(fit.ex4)
plot(res4,ylab="res (Tourism)")
abline(0,0)
Acf(res4)
par(mfrow=c(1,1))
