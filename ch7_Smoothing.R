# https://www.otexts.org/fpp/7
# Exponential Smoothing

# Load library of packages and data sets for Hyndman's book
library(fpp)

oildata <- window(oil,start=1996,end=2007)
plot(oildata, ylab="Oil (millions of tonnes)",xlab="Year")

fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oildata, h=3)
plot(fit1, PI=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)

# Example 7.2 Air Passengers
air <- window(ausair,start=1990,end=2004)
fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5) 
fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5) 
# Results for first model:
fit1$model$state
fitted(fit1)
fit1$mean

fit3 <- holt(air, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=5) 
plot(fit2, type="o", ylab="Air passengers in Australia (millions)", xlab="Year", 
     fcol="white", PI=FALSE)
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft", lty=1, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))

# Example 7.3 Sheep in Asia
livestock2 <- window(livestock,start=1970,end=2000)
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2,exponential=TRUE)
fit4 <- holt(livestock2,damped=TRUE)
fit5 <- holt(livestock2,exponential=TRUE,damped=TRUE)
# Results for first model:
fit1$model
accuracy(fit1) # training set
accuracy(fit1,livestock) # test set

plot(livestock2)
plot(fit2$model$state)
plot(fit4$model$state)

plot(fit3, type="o", ylab="Livestock, sheep in Asia (millions)", 
     flwd=1, PI=FALSE)
lines(window(livestock,start=2001),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))

# Example 7.4 International Visitor's Australia
aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

plot(fit2,ylab="International visitor night in Australia (millions)",
     PI=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)
lines(fit1$mean, type="o", col="red")
lines(fit2$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))

states <- cbind(fit1$model$states[,1:3],fit2$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")
fit1$model$state[,1:3]
fitted(fit1)
fit1$mean

# Error, Trend, Seasonality (ETS)
# ets(y, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
#     gamma=NULL, phi=NULL, additive.only=FALSE, lambda=NULL,
#     lower=c(rep(0.0001,3), 0.8), upper=c(rep(0.9999,3),0.98),
#     opt.crit=c("lik","amse","mse","sigma","mae"), nmse=3,
#     bounds=c("both","usual","admissible"),
#     ic=c("aicc","aic","bic"), restrict=TRUE)

# forecast(object, h=ifelse(object$m>1, 2*object$m, 10),
#          level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE,
#          npaths=5000, PI=TRUE, lambda=object$lambda, ...)

# Example 7.1 Oil Forecast Revisited
oildata <- window(oil,start=1996,end=2007)
fit <- ets(oildata, model="ANN")
plot(forecast(fit, h=3), ylab="Oil (millions of tones)")
fit$par

# Example 7.4 International Visitor's Australia (back to)
vndata <- window(austourists, start=2005)
fit <- ets(vndata)
summary(fit)
plot(fit)
plot(forecast(fit,h=8),
     ylab="International visitor night in Australia (millions)")
