#-------------- Project 6 -------------------#

#Installing required packages

install.packages("forecast")
install.packages("tseries")
install.packages("dygraphs")
install.packages("TTR")
install.packages("xts")

library(forecast)
library(tseries)
library(dygraphs)
library(TTR)
library(xts)
library(TSstudio)
#Reading the Dataset

USGas<- forecast::gas

#------------ Performing EDA -------------#

class(USGas)

str(USGas)

summary(USGas)

# Checking if the dataset have null values.

sum(is.na(USGas))

# Checking Frequency.

frequency(USGas)

#Checking the periodicity of the Gas Data.

periodicity(USGas)

#Plotting the on Original Dataset.
gas_data <- window(USGas, start = c(1970,1), frquency =12)

dygraph(gas_data, main = "Gas Dataset")

#Plotting the seasonal Plot on Original Dataset.

ts_seasonal(gas_data, type = "all")

# Plotting the Heat Map on Original Dataset.

ts_heatmap(gas_data)

# Plotting the Surface Map on Original Dataset.
ts_surface(gas_data)

# Plotting the box Plot to check the Outliers.

boxplot(gas_data~cycle(gas_data), main = "Boxplot for Gas Dataset")


#Plotting the decompositionS of data.
gas.ts <- stl(gas_data, s.window = "periodic")
plot(gas.ts)

ts_lags(gas_data, lags = c(10,20,30,40,50,60,70,80,90,100))

#Checking the Stationarity of the data.

adf.test(gas_data,alternative = "stationary", k=12)

#As we see that the data have trend, we difference the data.

diff.gas.data<- diff(log(gas_data))

dygraph(diff.gas.data)


#Performing the ADF Test on differenced data 

adf.test(diff.gas.data,k=12)

#Plotting the decomposition of differenced data.

decom.ts<- (stl(diff.gas.data,s.window = "periodic"))

plot(decom.ts)

#Splitting the data into train and test.

train <- window(diff.gas.data, end =c(1993,12), frequency =12)
dygraph(train)


test <- window(diff.gas.data,start = c(1994,1), frequency=12)
dygraph(test)


# Performing ACF test to identify q.
acf(train, lag.max = 50)

# Performing PACF to identify p.
pacf(train, lag.max = 50)

# Performing Manual arima on calculated p and q.
gasArima <- arima(exp(train), order = c(2,0,3))
summary(gasArima)

# Perforing Ljung Test to check if the residuals are independent.

Box.test(exp(gasArima$residuals))

# Plotting Residuals
tsdisplay(residuals(gasArima), lag.max = 45, main = "Arima Residuals")


# Performing accuracy on Test data.
forecast_arima <- forecast(gasArima, h=20)

accuracy(forecast_arima, exp(test))

test_forecast(forecast.obj = forecast_arima,actual =exp(diff.gas.data),test = exp(test))

test_for

#Performing Auto Arima

gasAuto <- auto.arima(exp(train),seasonal = TRUE)
summary(gasAuto)


#Perfroming Ljung test

Box.test(gasAuto$residuals)

#Predicting the accuracy on test.

forecast_auto <- forecast(gasAuto, h=20)
accuracy(forecast_auto, exp(test))

plot(forecast_auto)

test_forecast(forecast.obj = forecast_auto,actual = exp(diff.gas.data),test = exp(test))

# Building the Model on the Original Dataset.

auto_original <- auto.arima(gas_data, seasonal = TRUE)
summary(auto_original)

#Perfroming Ljung test

Box.test(auto_original$residuals)

#Forecasting for next 12 Months

next_forecast <- forecast(auto_original, h=12)
next_forecast


plot(next_forecast)


