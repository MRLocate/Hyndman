# https://www.otexts.org/fpp/5
# Multiple Regression

# Load library of packages and data sets for Hyndman's book
library(fpp)

#The panel.hist function is defined in help(pairs).
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


# Credit Scoring Example
pairs(credit[,-(4:5)], diag.panel=panel.hist)

# log(x+1) transformation
creditlog <- data.frame(score=credit$score, 
                        log.savings=log(credit$savings+1), 
                        log.income=log(credit$income+1), 
                        log.address=log(credit$time.address+1),
                        log.employed=log(credit$time.employed+1), 
                        fte=credit$fte, single=credit$single)
pairs(creditlog[,1:5], diag.panel=panel.hist)

# multiple regression on score
fit <- step(lm(score ~ log.savings + log.income + log.address 
               + log.employed + single, data=creditlog))
summary(fit)

plot(fitted(fit), creditlog$score,
     ylab="Score", xlab="Predicted score")

# Australian quarterly beer production
beer2 <- window(ausbeer,start=1992,end=2006-.1)
fit <- tslm(beer2 ~ trend + season)
summary(fit)

plot(beer2, xlab="Year", ylab="", main="Quarterly Beer Production")
lines(fitted(fit), col=2)
legend("topright", lty=1, col=c(1,2), legend = c("Actual", "Predicted"))

plot(fitted(fit), beer2, xy.lines=FALSE, xy.labels=FALSE, 
     xlab="Predicted values", ylab="Actual values", 
     main="Quarterly Beer Production")
abline(0, 1, col="gray")

fcast <- forecast(fit)
plot(fcast, main="Forecasts of beer production using linear regression")

# Model selection criteria
CV(fit)

# Back to the Credit Score example
fit <- lm(score ~ log.savings + log.income + 
            log.address + log.employed, data=creditlog)
par(mfrow=c(2,2))
plot(creditlog$log.savings,residuals(fit),xlab="log(savings)")
plot(creditlog$log.income,residuals(fit),xlab="log(income)")
plot(creditlog$log.address,residuals(fit),xlab="log(address)")
plot(creditlog$log.employed,residuals(fit),xlab="log(employed)")
par(mfrow=c(1,1))

plot(fitted(fit), residuals(fit),
     xlab="Predicted scores", ylab="Residuals")

# Evaluating Beer Production Forecasts
fit <- tslm(beer2 ~ trend + season)
res <- residuals(fit)
par(mfrow=c(1,2))
plot(res, ylab="Residuals",xlab="Year")
Acf(res, main="ACF of residuals")
par(mfrow=c(1,1))

dwtest(fit, alt="two.sided")
# It is recommended that the two-sided test always be used
# to check for negative as well as positive autocorrelation
# Test for autocorrelations up to lag 5.
bgtest(fit,5)

hist(res, breaks="FD", xlab="Residuals", 
     main="Histogram of residuals", ylim=c(0,22))
x <- -50:50
lines(x, 560*dnorm(x,0,sd(res)),col=2)

# Peicewise linear regression for Auto Emissions
Cityp <- pmax(fuel$City-25,0)
fit2 <- lm(Carbon ~ City + Cityp, data=fuel)
x <- 15:50; z <- pmax(x-25,0)
fcast2 <- forecast(fit2, newdata=data.frame(City=x,Cityp=z))
plot(jitter(Carbon) ~ jitter(City), data=fuel)
lines(x, fcast2$mean,col="red")

# Cubic spline
fit3 <- lm(Carbon ~ City + I(City^2) + I(City^3) + I(Cityp^3), data=fuel)
fcast3 <- forecast(fit3,newdata=data.frame(City=x,Cityp=z))
plot(jitter(Carbon) ~ jitter(City), data=fuel)
lines(x, fcast3$mean,col="red")
