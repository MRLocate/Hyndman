# http://otexts.org/fpp2/hierarchical.html
# Chapter 10 Forecasting hierarchical or grouped time series

# Load library of packages and data sets for Hyndman's book
library(fpp2)

# forecast(object, h = ifelse(frequency(object$bts)>1, 2*frequency(object$bts),10),
# fmethod = c("ets", "arima", "rw"),
# method = c("comb", "bu", "mo", "tdgsa", "tdgsf", "tdfp"), 
# weights = c("wls", "ols", "mint", "nseries"), ...)

require(hts)

# Heirarchical Time Series
tourism.hts <- hts(visnights, characters = c(3, 5))
tourismL0 <- aggts(tourism.hts, levels = 0)
p1 <- autoplot(tourismL0) +
  xlab("Year") +
  ylab("Visitor nights ('000)")+
  ggtitle("Total")

tourismL1 <- aggts(tourism.hts, levels = 1)
p2 <- autoplot(tourismL1[,c(1,3,5)]) +
  xlab("Year") +
  ylab("Visitor nights ('000)")+
  scale_colour_discrete(guide = guide_legend(title = "State"))

p3 <- autoplot(tourismL1[,c(2,4,6)]) +
  xlab("Year") +
  ylab("Visitor nights ('000)")+
  scale_colour_discrete(guide = guide_legend(title = "State"))

lay=rbind(c(1,1),c(2,3))
gridExtra::grid.arrange(p1, p2,p3, layout_matrix=lay)

# Grouped Time Series
# Using the `characters` input
prison.gts <- gts(prison, characters = c(3,1,9))
#> Argument gnames is missing and the default labels are used.

fcsts = forecast(prison.gts, h = 8, method = "comb", weights = "wls", fmethod = "ets")

plot(fcsts,levels = 0, color_lab=TRUE)
title(main = "Total")
plot(fcsts,levels = 1, color_lab=TRUE)
title(main = "Grouped by state")
plot(fcsts,levels = 2, color_lab=TRUE)
title(main = "Grouped by leagal status")
plot(fcsts,levels = 3, color_lab=TRUE)
title(main = "Grouped by gender")

train <- window(prison.gts,end=c(2014,4))
test <- window(prison.gts,start=2015)

fcsts.opt = forecast(train, h = 8, method = "comb", weights = "wls", fmethod = "ets")
fcsts.bu = forecast(train, h = 8, method = "bu", fmethod = "ets")

tab <- matrix(NA,ncol=4,nrow=6)
rownames(tab) <- c("Total", "State", "Legal status", "Gender","Bottom", "All series")
colnames(tab) <- c("Bottom-up MAPE","Bottom-up MASE","Optimal MAPE","Optimal MASE")

tab[1,] <- c(accuracy.gts(fcsts.bu,test,levels = 0)[c("MAPE","MASE"),"Total"],
             accuracy.gts(fcsts.opt,test,levels = 0)[c("MAPE","MASE"),"Total"])

j=2
for(i in c(1:3,7)){
  tab[j,] <- c(mean(accuracy.gts(fcsts.bu,test,levels = i)["MAPE",]),
               mean(accuracy.gts(fcsts.bu,test,levels = i)["MASE",]),
               mean(accuracy.gts(fcsts.opt,test,levels = i)["MAPE",]),
               mean(accuracy.gts(fcsts.opt,test,levels = i)["MASE",]))
  j=j+1
}

tab[6,] <- c(mean(accuracy.gts(fcsts.bu,test)["MAPE",]),
             mean(accuracy.gts(fcsts.bu,test)["MASE",]),
             mean(accuracy.gts(fcsts.opt,test)["MAPE",]),
             mean(accuracy.gts(fcsts.opt,test)["MASE",]))

knitr::kable(tab, digits=2, booktabs=TRUE)
