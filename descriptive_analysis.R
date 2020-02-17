library(ggplot2)
library(expsmooth)
library(fpp)

### Load Data

hours = read.csv("project/dataset/hour.csv")
days = read.csv("project/dataset/day.csv")

head(hours)

# Converting datatype
days$dteday<-as.Date(days$dteday)
days$season<-as.factor(days$season)
days$weekday<-as.factor(days$weekday)
days$holiday<-as.factor(days$holiday)
sapply(days,class)

hours$dteday<-as.Date(dayhourss$dteday)
hours$season<-as.factor(hours$season)
hours$weekday<-as.factor(hours$weekday)
hours$holiday<-as.factor(hours$holiday)




### Descriptive Analysis

## Mean and median temperature by season
aggregate(days$temp~days$season, FUN= function(x) c(mean = mean(x), median = median(x)))


boxplot(days$temp~days$season)


cor(days[, c("temp", "atemp", "cnt")])


### Means per month
cbind(aggregate(days$temp~days$mnth + days$yr, FUN= mean),
  aggregate(days$atemp~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$hum~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$windspeed~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$cnt~days$mnth + days$yr, FUN= mean)[3])


## Correlation between temperature and bike rentals
cor(hours[c("temp", "casual", "registered")])


plot(days$dteday, days$cnt, type="l")

boxplot(days$cnt)


smoothed = ses(ts(days$cnt, start = min(days$dteday), end = max(days$dteday)))

plot(cbind(smoothed$fitted, days$cnt))

?hw
