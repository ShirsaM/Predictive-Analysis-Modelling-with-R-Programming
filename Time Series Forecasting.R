###### Time Series Forecasting Case Study ######
##### Airline passenger data analysis


#### Step-1 - Data collection & preparation ####
### Air passenger dataset is a built-in dataset so n sepearte collection required


#### Step-2 - to check non-stationarity ####
### We perform the Augmented Dickey - Fuller test (ADF) to check non-statonarity
### H0- null hypothesis: the series is not stationary (p-value>0.05)
### H1- alternate hypothesis: the series is stationary (p-value<=0.05)


library(tseries)
adf.test(as.numeric(AirPassengers),k=12)    #----> k is the lag order to calculate the test statistic
                                            #----> We want R to compare values with past 12 values to check for stationarity


### Another test for stationarity - Kwiatkowski-Phillips-Schmidt-Shin (KPSS)
### KPSS test can be used to test both level and trend stationarity.
### * Level stationarity indicates whether the time series is stationary about a level (may be non-zero) or not.
### * Trend stationarity means if the time series is stationary once, we remove the trend component.
### H0- null hypothesis: the series is stationary around a linear trend or mean (p-value>0.05)
### H1- alternate hypothesis: the series is not stationary (p-value<=0.05)

kpss.test(AirPassengers,null=c("Trend"))
kpss.test(AirPassengers,null=c("Level"))

### the tests show the series is Trend Stationary but not Level Stationary.
### We can also mention trend and level together or seperate



#### We can also confirm the non-stationarity of data by looking at the Auto Correlation Function (ACF) plot. 
### The slow decay along with the wavy pattern suggests that this data is non-stationary. 

acf(AirPassengers)



#### Step-3 - Time Series Decomposition ####
### time series data can be decomposed in two ways:
### * Additive seasonal decomposition, wherein the individual components can be added to get the time series data
### * Multiplicative seasonal decomposition, wherein the individual components can be multiplied to get the time series data

plot(decompose(AirPassengers))   #---------> by default it is taken additive
plot(decompose(AirPassengers,type="multiplicative")) 


#### Step-4 - Determine parameters for ARIMA ####

### To obtain 'd'
### * If the series shows positive correlations above the upper blue dotted line in the ACF plot till a high number of lags, then the data needs to be further differencing.
### * An optimally differenced series can be identified by the following observations:
###      The differenced data shows a very low positive or negative correlation (within the blue dotted lines in the ACF plot) at lag 1.
###      The remaining correlations at higher lags are close to 0, patternless or within the blue dotted line.
###      The series will have the smallest standard deviation.
## * Over-differencing a series might lead to erroneous parameter values. An over-differenced series will generally show a lag 1 correlation value to be -0.5 or less.


### to obtain 'p' , 'q'
### * AR(p): The lag after which all partial autocorrelation values (in the PACF plot) are within the confidence margins of proximity to 0 is considered the order of the AR model.
### * MA(q): The lag after which all autocorrelation values(in the ACF plot) are within the confidence margins of proximity to 0 is considered the order of the MA model.
### The data has both trend and seasonality components. Previously, you understood that applying a first order differencing with lag 1 removed the trend, while a first order differencing with lag 12 removed the seasonality. 
### The linear trend is removed by taking another first-order differencing with lag 1, i.e., (1-B)
### The seasonality is removed by taking a first-order differencing with lag 12, i.e., (1-B^12)log(Xt)

#### Further checks with ACF and PACF 
tap=log(as.numeric(AirPassengers))
tap12=diff(tap,lag=12)
tap1=diff(tap12,lag=1)
plot(tap1,type="l",main="Line plot for the differenced time series",ylab="Residual",xlab="Time")
kpss.test(tap12,null=c("Level","Trend"))

pacf(tap1,lag.max=500,main="PACF vs. lag for the differenced time series")
acf(tap1,lag.max=500,main="ACF vs. lag for the differenced time series")



#### Step-5 - ARIMA forecast ####

library(forecast)
best_mod=auto.arima(log(AirPassengers),trace=TRUE)
frcst=forecast(best_mod,h=60, level=c(95))
plot(frcst)

### Here, h = 60 denotes 60 values to forecast, and level = c(95) indicates that we want a 95% prediction interval around the forecast values. 



#### Step-6 - SARIMA Model Residuals ####
install.packages('ggfortify')
tsdisplay(residuals(best_mod), lag.max=120, main='SARIMA Model Residuals') 
library(ggfortify)
ggtsdiag(best_mod)   #---------> finding independence of these errors using the Ljung Box test



#### Step-7 -  Check for white noises ####
shapiro.test(best_mod$residuals)
hist(best_mod$residuals, main="Histogram of the residuals",xlab="Residuals")

kpss.test(best_mod$residuals, null=c("Level","Trend"))



