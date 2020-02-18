library(ggplot2)
library(expsmooth)
library(fpp)
library(smooth)
library("TTR") ## Teacher's recommendation
library(imputeTS)

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


boxplot(days$temp~days$season, xlab="Seasons", ylab="Temperature")


cor(days[, c("temp", "atemp", "cnt", "casual", "registered" )])


### Means per month
cbind(aggregate(days$temp~days$mnth + days$yr, FUN= mean),
  aggregate(days$atemp~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$hum~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$windspeed~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$cnt~days$mnth + days$yr, FUN= mean)[3])


## Correlation between temperature and bike rentals
cor(hours[c("temp", "casual", "registered")])


plot(days$dteday, days$cnt, type="l")

?boxplot

smoothed = holt(ts(days$cnt, start = min(days$dteday), end = max(days$dteday)))


## Using library 'smooth'
es(ts(days$cnt, start = min(days$dteday), end = max(days$dteday)), h=18, holdout=TRUE, silent=FALSE)
# Using library TTR
smoothed = SMA(ts(days$cnt, start = min(days$dteday), end = max(days$dteday)), n=7)


p = ggplot() + 
  geom_line(data = days, aes(x = days$dteday, y = days$cnt), color = "lightblue") +
  geom_line(data = days, aes(x = days$dteday, y = smoothed), color = "red") +
  xlab('Dates') +
  ylab('Number of rentals')

print(p)


## Decomposing the time series
smoothed = SMA(ts(days$cnt, start = min(days$dteday), end = max(days$dteday)), n=7)

# Remove missing values
timeseries <- ts(smoothed[!is.na(smoothed)], frequency = 30)
plot(timeseries)

decomposed <- decompose(timeseries)
plot(decomposed)


## Remove the seasonal component
decomposed$x-decomposed$season
