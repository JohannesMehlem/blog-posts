#Accompanying blog post: https://johannesmehlem.com/blog/exponential-smoothing-time-series-forecasting-r/

#Reading in CSV file with header from set working directory
data <- read.csv("organic-traffic.csv", header = T)
#Checking format of the data
head(data)
#Checking class of date column in data / confirming "factor" class
class(data[,1])
#Converting "factor" to "Date" according to cell formatting in CSV file
data[,1] <- as.Date(data[,1], format = "%m/%d/%y")
#Converting vector of "Organic Sessions" into time series object
Organic_Traffic <- ts(data[,2], start = c(2014,1), end = c(2018,6), frequency = 12)
#Avoiding scientific notation for y-axis values
options(scipen=999)
#Plotting our "Organic Traffic" data
plot(Organic_Traffic, main = "Organic Traffic", ylab = "Sessions", ylim = c(0, 700000))


#Decomposing data into seasonal, trend and irregular components using classical decomposition
fit_decompose <- decompose(Organic_Traffic, type = "multiplicative")
#Printing component data of decomposition 
fit_decompose
#Plotting component data of decomposition
plot(fit_decompose)


#Install and load forecast package if not installed
if(!require(forecast)) {
  install.packages("forecast", dependencies = T)
  library(forecast) }
#Fit simple exponential smoothing model to data and show summary
fit_ses <- ses(Organic_Traffic, h = 6)
summary(fit_ses)
#Plot the forecasted values
plot(fit_ses)


#Fit Holt exponential smoothing model to data and show summary
fit_holt <- holt(Organic_Traffic, h = 6)
summary(fit_holt)
#Plot the forecasted values
plot(fit_holt)


#Fit Holt-Winters exponential smoothing model to data and show summary
fit_hw <- hw(Organic_Traffic, h = 6, seasonal = "multiplicative")
summary(fit_hw)
#Plot the forecasted values
plot(fit_hw)


#Fit automated exponential smoothing model to data and show summary
fit_auto <- forecast(Organic_Traffic, h = 10)
summary(fit_auto)
#Plot the forecasted values
plot(forecast(fit_auto))


#Fit automated exponential smoothing model to training dataset and calculate forecast accuracy
train <- window(Organic_Traffic, end = c(2018, 3))
test <- window(Organic_Traffic, start = c(2018, 4))
fit_hw_train <- hw(train)
fit_auto_train <- forecast(train)
accuracy(forecast(fit_hw_train), test) ["Test set", "RMSE"]
accuracy(forecast(fit_auto_train), test) ["Test set", "RMSE"]


#Calcuate mean of residuals and plot histogram for residuals
fit_hw_res <- residuals(forecast(fit_hw_train))
fit_auto_res <- residuals(forecast(fit_auto_train))
mean(fit_hw_res)
mean(fit_auto_res)
hist(fit_hw_res)
