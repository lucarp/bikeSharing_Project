setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(ggplot2)
library(expsmooth)
library(fpp)
library(smooth)
library("TTR") ## Teacher's recommendation
library(imputeTS)



#####################
### Load Data
#####################

#hours = read.csv("project/dataset/hour.csv")
#days = read.csv("project/dataset/day.csv")
hours = read.csv("./dataset/hour.csv")
days = read.csv("./dataset/day.csv")

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



#####################
### Descriptive Analysis
#####################

## Mean and median temperature by season
# ----- How do the temperatures change across the seasons? 
# ----- What are the mean and median temperatures?
aggregate(days$temp~days$season, FUN= function(x) c(mean = mean(x), median = median(x)))

boxplot(days$temp~days$season, xlab="Seasons", ylab="Temperature")


# ----- Is there a correlation between the temp/atemp/mean.temp.atemp and 
# ----- the total count of bike rentals?
cor(days[, c("temp", "atemp", "cnt", "casual", "registered" )])


## Means per month
# ----- What are the mean temperature, humidity, windspeed and total rentals per months?
cbind(aggregate(days$temp~days$mnth + days$yr, FUN= mean),
  aggregate(days$atemp~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$hum~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$windspeed~days$mnth + days$yr, FUN= mean)[3],
  aggregate(days$cnt~days$mnth + days$yr, FUN= mean)[3])


## Correlation between temperature and bike rentals
# ----- Is temperature associated with bike rentals (registered vs. casual)?
cor(hours[c("temp", "casual", "registered")])


# ----- Plot the cnt vs dteday and examine its patterns and irregularities
plot(days$dteday, days$cnt, type="l")


# ----- Smooth your time series and compare with the origina
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


#####################
### Decomposing the time series
#####################
cnt_ma = SMA(ts(days$cnt, start = min(days$dteday), end = max(days$dteday)), n=7)

## Remove missing values
# ----- transform cnt_ma into a time series with frequency 30 named count_ma
count_ma <- ts(cnt_ma[!is.na(cnt_ma)], frequency = 30)
plot(count_ma)

# ----- Use decompose() or stl() to examine and possibly remove components of the series
decomposed <- decompose(count_ma)
plot(decomposed)

# Removed seasonal component
deseasonal_cnt <- count_ma - decomposed$season
plot(count_ma - decomposed$season)


#Is the test stationary ?
adf.test(count_ma)

## The timeseries is not stationary, because there is a decreasing tendency in the plot
Acf(count_ma)
Pacf(count_ma)

## PS: a small alpha value means that the most recent values does not explain very well the variation in the mean.



## Remove the seasonal component
# ----- create a time series deseasonal_cnt by removing the seasonal component
deseasonal_cnt <- decomposed$x-decomposed$season



#####################
### Stationarity
#####################
# ----- Is the serie count_ma stationary? If not, how to make stationary 
# ----- (Use adf.test(), ACF, PACF plots )
res_test <- adf.test(count_ma)

layout(matrix(1:2, nrow=1))
Acf(count_ma)
Pacf(count_ma)


#####################
### Forecasting with ARIMA Models
#####################

## I.Fitting ARIMA model

# ----- Fit an ARIMA model to deseasonal_cnt 
# ----- (Examine the ACF and PACF plots, trends, residuals)

arima.result <- arima(deseasonal_cnt, order = c(2,0,0))
arima.forecast <- forecast(arima.result, h=25)
plot(arima.forecast)

# ----- What is your conclusion?

Acf(deseasonal_cnt)
Pacf(deseasonal_cnt)

## II. Fit an ARIMA with Auto-ARIMA

# ----- Use auto.arima() function to fit an ARIMA model of deseasonal_cnt
auto.arima.result <- auto.arima(deseasonal_cnt)
auto.arima.forecast <- forecast(auto.arima.result, h=25)
plot(auto.arima.forecast)

# -----  Check residuals, which should have no patterns and be normally distributed
plot(auto.arima.forecast$residuals)

## III.Evaluate and iterate

# -----  If there are visible patterns or bias, plot ACF/PACF. 
# -----  Are any additional order parameters needed?


# ----- Refit model if needed. Compare model errors and fit criteria such as AIC or BIC.


# ----- Calculate forecast using the chosen model


# ----- plot both the original and the forecasted time series


## IV.Forecasting

# ----- Split the data into training and test times series 
# ----- (test starting at observation 700, use function window)


# ----- fit an Arima model, manually and with Auto-Arima on the training part


# ----- forecast the next 25 observation and plot the original ts and the forecasted one.


# ----- What do you observe?
