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

layout(matrix(1:4, nrow=2))
plot(x=1:12, y=unlist(aggregate(days$temp~days$mnth, FUN= mean)[2]), type="b", lty=1, xlab="Months", ylab="Temp")
#plot(x=1:12, y=unlist(aggregate(days$atemp~days$mnth, FUN= mean)[2]), type="b", lty=1, xlab="Months", ylab="Atemp")
plot(x=1:12, y=unlist(aggregate(days$hum~days$mnth, FUN= mean)[2]), type="b", lty=1, xlab="Months", ylab="Humidity")
plot(x=1:12, y=unlist(aggregate(days$windspeed~days$mnth, FUN= mean)[2]), type="b", lty=1, xlab="Months", ylab="Windspeed")
plot(x=1:12, y=unlist(aggregate(days$cnt~days$mnth, FUN= mean)[2]), type="b", lty=1, xlab="Months", ylab="Total rentals")
dev.off()


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

stled <- stl(count_ma, 5)
plot(stled)

# Removed seasonal component
deseasonal_cnt <- count_ma - decomposed$season
plot(count_ma - decomposed$season)


## Remove the seasonal component
# ----- create a time series deseasonal_cnt by removing the seasonal component
deseasonal_cnt <- decomposed$x-decomposed$season


## PS: a small alpha value means that the most recent values does not explain very well the variation in the mean.



#####################
### Stationarity
#####################
# ----- Is the serie count_ma stationary? If not, how to make stationary 
# ----- (Use adf.test(), ACF, PACF plots )
res_test <- adf.test(count_ma) ## p-value = 0.9753 means the timeseries is non-stationary
res_test
#layout(matrix(1:2, nrow=1))
Acf(count_ma) ## The timeseries is not stationary, because there is a decreasing tendency in the plot
Pacf(count_ma)


#####################
### Forecasting with ARIMA Models
#####################

## I.Fitting ARIMA model

# ----- Fit an ARIMA model to deseasonal_cnt 
# ----- (Examine the ACF and PACF plots, trends, residuals)

arima.result <- arima(deseasonal_cnt)
arima.result
arima.forecast <- forecast(arima.result, h=25)
plot(arima.forecast)

tsdisplay(residuals(arima.result), lag.max=45, main='(0,0,0) Model Residuals') 

# ----- What is your conclusion?
## AIC is higher than with the parameters proposed by Auto.Arima

## II. Fit an ARIMA with Auto-ARIMA

# ----- Use auto.arima() function to fit an ARIMA model of deseasonal_cnt
auto.arima.result <- auto.arima(deseasonal_cnt, seasonal = FALSE)
auto.arima.result
auto.arima.forecast <- forecast(auto.arima.result, h=25)
plot(auto.arima.forecast)

# -----  Check residuals, which should have no patterns and be normally distributed
layout(matrix(c(1,2), 2, 2, byrow = TRUE))
plot(auto.arima.forecast$residuals) #no patterns
hist(auto.arima.forecast$residuals) #normally distributed

## III.Evaluate and iterate

# -----  If there are visible patterns or bias, plot ACF/PACF. 
# -----  Are any additional order parameters needed?

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
fit
tsdisplay(residuals(fit), lag.max=45, main='Model Residuals') 

# ----- Refit model if needed. Compare model errors and fit criteria such as AIC or BIC.

fit2 = arima(deseasonal_cnt, order=c(1,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

### Therefore, we have chosen the fit2, as it has the smaller AIC

best <- 1e6
best_param <- c()
for(a in 1:20){
  for(b in 1:20){
    for(c in 1:20){
      temp <- arima(deseasonal_cnt, order=c(a,b,c)) 
      if(temp$aic < best){
        best_param <- c(a, b, c)
        best <- temp$aic
        print(best_param)
      }
    }
  }
}
fit2 = arima(deseasonal_cnt, order=best_param)
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')


# ----- Calculate forecast using the chosen model

fcast <- forecast(fit2, h=25)

# ----- plot both the original and the forecasted time series
plot(fcast)


## IV.Forecasting

# ----- Split the data into training and test times series 
# ----- (test starting at observation 700, use function window)

hold <- window(ts(deseasonal_cnt), start=700)

# ----- fit an Arima model, manually and with Auto-Arima on the training part

# ----- forecast the next 25 observation and plot the original ts and the forecasted one.

#best_param <- c(1,1,7)
  
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=best_param)
fit_no_holdout
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

fit3 = auto.arima(ts(deseasonal_cnt[-c(700:725)]), seasonal=FALSE)
fit3
seas_fcast <- forecast(fit3, h=25)
plot(seas_fcast) 
lines(ts(deseasonal_cnt))

# ----- What do you observe?

### 
