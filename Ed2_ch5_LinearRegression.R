# http://otexts.org/fpp2/ch-regression.html
# Chapter 5 Linear regression models

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# Example: US consumption expenditure
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")
uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
tslm(Consumption ~ Income, data=uschange)
summary(tslm(Consumption ~ Income, data=uschange))

# Example: US consumption expenditure (revisited)
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()
fit.consMR <- tslm(Consumption ~ Income + Production + Unemployment + Savings,
                   data=uschange)
summary(fit.consMR)
autoplot(uschange[,'Consumption'], series="Data") +
  forecast::autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percentage change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))
cbind(Data=uschange[,"Consumption"], Fitted=fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  xlab("Fitted (predicted values)") +
  ylab("Data (actual values)") +
  ggtitle("Percentage change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)

# Example: Australian quarterly beer production
beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + xlab("Year") + ylab("Megalitres")
fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)
autoplot(beer2, series="Data") +
  forecast::autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")
cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted, colour=as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)
# Fourier series
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)
autoplot(beer2, series="Data") +
  forecast::autolayer(fitted(fourier.beer), series="Fourier") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")

# Evaluating the regression model
checkresiduals(fit.beer)

df <- as.data.frame(uschange)
df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) + geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) + geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) + geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) + geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

fit <- tslm(elec ~ trend + season)
cbind(Fitted=fitted(fit), Residuals=residuals(fit)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

# Spurious Correlation
fit <- tslm(aussies ~ guinearice)
summary(fit)

# Cross Validation
CV(fit.consMR)

# Forecasting with regression
beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using linear regression")

# Scenario based forecasting
fit.consBest <- tslm(Consumption ~ Income + Savings + Unemployment, data = uschange)
h <- 4
newdata <-
  cbind(Income=c(1,1,1,1), Savings=c(0.5,0.5,0.5,0.5), Unemployment=c(0,0,0,0)) %>%
  as.data.frame()
fcast.up <- forecast(fit.consBest, newdata=newdata)
newdata <-
  cbind(Income=rep(-1,h), Savings=rep(-0.5,h), Unemployment=rep(0,h)) %>%
  as.data.frame()
fcast.down <- forecast(fit.consBest, newdata=newdata)

autoplot(uschange[,1]) + ylab("% change in US consumption") +
  forecast::autolayer(fcast.up, PI=TRUE, series="increase") +
  forecast::autolayer(fcast.down, PI=TRUE, series="decrease") +
  guides(colour=guide_legend(title="Scenario"))
autoplot(uschange[,"Consumption"]) +
  ylab("% change in US consumption") +
  forecast::autolayer(fcast.up, PI=TRUE, series="Average increase") +
  forecast::autolayer(fcast.down, PI=TRUE, series="Extreme increase") +
  guides(colour=guide_legend(title="Scenario"))

# Forecasting with a nonlinear trend
# Example: Boston marathon winning times
h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h=h)
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h=h)
t.break1 <- 1940
t.break2 <- 1980
t <- time(marathon)
t1 <- ts(pmax(0,t-t.break1), start=1897)
t2 <- ts(pmax(0,t-t.break2), start=1897)
fit.pw <- tslm(marathon ~ t + t1 + t2)
t.new <- t[length(t)]+seq(h)
t1.new <- t1[length(t1)]+seq(h)
t2.new <- t2[length(t2)]+seq(h)
newdata <- cbind(t=t.new,t1=t1.new,t2=t2.new)%>%as.data.frame()
fcasts.pw <- forecast(fit.pw,newdata = newdata)
autoplot(marathon) +
  forecast::autolayer(fitted(fit.lin), series = "Linear") +
  forecast::autolayer(fitted(fit.exp), series="Exponential") +
  forecast::autolayer(fitted(fit.pw), series = "Piecewise") +
  forecast::autolayer(fcasts.pw, series="Piecewise") +
  forecast::autolayer(fcasts.lin$mean, series = "Linear") +
  forecast::autolayer(fcasts.exp$mean, series="Exponential") +
  xlab("Year") +  ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour=guide_legend(title=" "))
checkresiduals(fit.pw)
