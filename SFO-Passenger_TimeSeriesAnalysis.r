# Turning off the warnings
options(warn=-1)

# Changing the default size of the plots we are going to draw
options(repr.plot.width=15, repr.plot.height=8)

# Importing required libraries
library(sfo)
library(dplyr)
library(plotly)
library(TSstudio)
library(forecast)
library(uroot)
library(tseries)
library(lmtest)
library(astsa)
library(TSA)

# loading the data
data("sfo_passengers")
str(sfo_passengers)


## Transforming the activity_period column in dataset into a Date format
df <- sfo_passengers %>% mutate(Date = as.Date(paste(substr(sfo_passengers$activity_period, 1,4), 
                                                     substr(sfo_passengers$activity_period, 5,6), 
                                                     "01", sep ="/"))) 

## Transforming the dataset into a time series format by grouping the passenger by the date variable
df <- df %>% group_by(Date) %>%  summarise(passengers = sum(passenger_count), .groups = "drop")
head(df) 
str(df)
df$passengers <- as.numeric(df$passengers)

## Time Series plot for the whole Data
plot(df, type = "l", xlab = "Date", ylab = "Number of Passengers", main = "Time Series Plot")
points(df, pch = 16, cex = 0.5)

## pre_Covid data
df_precovid <- subset(df, Date<'2020-01-01')

## Plotting the pre-Covid Data
plot(df_precovid, type = "l", xlab = "Date", ylab = "Number of Passengers", main = "Time Series Plot(Pre-Covid)")
points(df_precovid, pch = 16, cex = 0.5)

## Transforming passengers Time Series into Time Series object
passengers <- ts(df_precovid$passengers, start = c(2005, 7), frequency = 12)

## Checking for Seasonal and trend componenets
plot(decompose(passengers))

## using the boxplot to see if there is any seasonal effects
boxplot(passengers~cycle(passengers))

## ACF plot
Acf(passengers, main = "ACF Plot",lag.max = 50)

## Dickey-Fuller Test
adf.test(passengers, alternative ="stationary", k=12)

## Plotting the passengers_differenced data
passengers_differenced <- diff(passengers)
plot(passengers_differenced)

## ACF and PACF and Dickey-Fuller test for differenced data
par(mfrow = c(1,2))
Acf(passengers_differenced, main = "ACF Plot-Differenced",lag.max = 50)
Acf(passengers_differenced, type = "partial", main = "PACF Plot-Differenced",lag.max = 50)
adf.test(passengers_differenced, alternative ="stationary", k=12)

## Plotting the passengers_differenced data with lag12
passengers_monthly_differenced<- diff(passengers_differenced , lag = 12)
plot(passengers_monthly_differenced)

## ACF and PACF for differenced data with lag12
par(mfrow = c(1,2))
Acf(passengers_monthly_differenced, main = "ACF Plot-Differenced",lag.max = 50)
Acf(passengers_monthly_differenced, type = "partial", main = "PACF Plot-Differenced",lag.max = 50)
adf.test(passengers_monthly_differenced, alternative ="stationary", k=12)

## Dividing dataset into train and test sets
train <- df_precovid[1:150, ]
test <- df_precovid[151:174, ]

## Converting data to Time Series Object
train_ts <- ts(train$passengers, start = c(2005, 7), frequency = 12)
test_ts <- ts(test$passengers, start = c(2018, 1), frequency = 12)

## Fitting Models

### Holt-Winters additive model
fit_hw_additive <- hw(train_ts,seasonal="additive")

### Holt-Winters multiplicative model
fit_hw_multiplicative <- hw(train_ts,seasonal="multiplicative")

### ARIMA model
fit_ARIMA <- auto.arima(train_ts)

## Holt-Winters additive model with lag 20
checkresiduals(fit_hw_additive, lag = 20)

## Holt-Winters multiplicative model with lag 50
checkresiduals(fit_hw_multiplicative, lag = 50)

## ARIMA model with lag 10
checkresiduals(fit_ARIMA, lag = 10)

## Coefficient test for ARIMA model
library(lmtest)
coeftest(fit_ARIMA)

# conditional heteroscedasticity for ARIMA model
library(TSA)
McLeod.Li.test(fit_ARIMA)$p.value

## plotting forcasts for the next 24 month based on the models that we created
par(mfrow = c(3,1))
plot(forecast(fit_hw_additive,h=24))
plot(forecast(fit_hw_multiplicative,h=24))
plot(forecast(fit_ARIMA,h=24))

## Storing forcasted data in suitable variables
pred_hw_additive <- forecast(fit_hw_additive,h=24)
pred_hw_multiplicative <- forecast(fit_hw_multiplicative,h=24)
pred_ARIMA <- forecast(fit_ARIMA,h=24)

train_ts <- window(train_ts,start=2005)
autoplot(train_ts) +
  autolayer(test_ts, series="Actual data", PI=FALSE) +
  autolayer(pred_ARIMA, series="Arima", PI=FALSE)+
  xlab("Year") +
  ylab("Passengers") +
  ggtitle("Comparing the predictions with the actual data - SARIMA") +
  theme(plot.title = element_text(hjust =0.5))+
  guides(colour=guide_legend(title="Forecast"))

autoplot(train_ts) +
  autolayer(pred_hw_multiplicative, series="HW multiplicative forecasts", PI=FALSE) +
  autolayer(test_ts, series="Actual data", PI=FALSE) +
  xlab("Year") +
  ylab("Passengers") +
  ggtitle("Comparing the predictions with the actual data - HW multiplicative") +
  theme(plot.title = element_text(hjust =0.5))+
  guides(colour=guide_legend(title="Forecast"))

autoplot(train_ts) +
  autolayer(pred_hw_additive, series="HW additive forecasts", PI=FALSE ) +
  autolayer(test_ts, series="Actual data", PI=FALSE) +
  xlab("Year") +
  ylab("Passengers") +
  ggtitle("Comparing the predictions with the actual data - HW additive") +
  theme(plot.title = element_text(hjust =0.5))+
  guides(colour=guide_legend(title="Forecast"))

autoplot(train_ts) +
  autolayer(pred_hw_additive, series="HW additive forecasts", PI=FALSE ) +
  autolayer(pred_hw_multiplicative, series="HW multiplicative forecasts", PI=FALSE) +
  autolayer(test_ts, series="Actual data", PI=FALSE) +
  autolayer(pred_ARIMA, series="Arima", PI=FALSE)+
  xlab("Year") +
  ylab("Passengers") +
  ggtitle("Comparing the predictions with the actual data") +
  theme(plot.title = element_text(hjust =0.5))+
  guides(colour=guide_legend(title="Forecast"))

## Holt-Winters additive model SSE
e_hw_additive= pred_hw_additive$mean - test$passengers
SSE_hw_additive <- sum(e_hw_additive^2)

## Holt-Winters multiplicative model SSE
e_hw_multiplicative= pred_hw_multiplicative$mean - test$passengers
SSE_hw_multiplicative <- sum(e_hw_multiplicative^2)

## ARIMA model SSE
e_ARIMA= pred_ARIMA$mean - test$passengers
SSE_ARIMA <- sum(e_ARIMA^2)

print('Holt-Winters additive model SSE is:')
SSE_hw_additive

print('Holt-Winters multiplicative model SSE is:')
SSE_hw_multiplicative

print('ARIMA model SSE is:')
SSE_ARIMA

## Spilliting data to train and test subsets for one step forcast for 24 months
train <- df_precovid[1:150, ]
test <- df_precovid[151:174, ]

## Converting the train and test sets to Time-Series-Object
train_ts <- ts(train$passengers, start = c(2005, 7), frequency = 12)
test_ts <- ts(test$passengers, start = c(2018, 1), frequency = 12)

## Creating two empty vectors
p <- numeric(length(1:24))
e <- numeric(length(1:24))

## One Step Ahead Forcasting with HW additive model
for(i in 1:24){
  train <- subset(head(df_precovid,174-(25-i)))
  test <- subset(tail(df_precovid,25-i))
  y <- ts(train$passengers, start = c(2005, 7), frequency = 12)
  fit_hw_additive <- hw(y,seasonal="additive")
  pred_hw_additive <- forecast(fit_hw_additive,h=1)
  p[i] <- pred_hw_additive$mean
  e[i] <- p[i]-test$passengers[1]
}

## One Step Ahead Forcast SSE for HW additive model
SSE2_hw_additive <- sum(e^2)

## Comparing forcasted data points with actual data points
test <- subset(tail(df_precovid,24))
comparison_table_hw_additive <- data.frame(test$passengers, p)
colnames(comparison_table_hw_additive) <- c('Actual Data','Prediction')

comparison_table_hw_additive

## Creating two empty vectors
p <- numeric(length(1:24))
e <- numeric(length(1:24))

## One Step Ahead Forcasting with HW multiplicative model
for(i in 1:24){
  train <- subset(head(df_precovid,174-(25-i)))
  test <- subset(tail(df_precovid,25-i))
  y <- ts(train$passengers, start = c(2005, 7), frequency = 12)
  fit_hw_multiplicative <- hw(y,seasonal="multiplicative")
  pred_hw_multiplicative <- forecast(fit_hw_multiplicative,h=1)
  p[i] <- pred_hw_multiplicative$mean
  e[i] <- p[i]-test$passengers[1]
}

## One Step Ahead Forcast SSE for HW multiplicative model
SSE2_hw_multiplicative <- sum(e^2)

## Comparing forcasted data points with actual data points
test <- subset(tail(df_precovid,24))
comparison_table_hw_multiplicative <- data.frame(test$passengers, p)
colnames(comparison_table_hw_multiplicative) <- c('Actual Data','Prediction')

comparison_table_hw_multiplicative

## Creating two empty vectors
p <- numeric(length(1:24))
e <- numeric(length(1:24))

## One Step Ahead Forcasting with ARIMA model
for(i in 1:24){
  train <- subset(head(df_precovid,174-(25-i)))
  test <- subset(tail(df_precovid,25-i))
  y <- ts(train$passengers, start = c(2005, 7), frequency = 12)
  fit_ARIMA <- auto.arima(y)
  pred_ARIMA <- forecast(fit_ARIMA,h=1)
  p[i] <- pred_ARIMA$mean
  e[i] <- p[i]-test$passengers[1]
}

## One Step Ahead Forcast SSE for ARIMA model
SSE2_ARIMA <- sum(e^2)

## Comparing forcasted data points with actual data points
test <- subset(tail(df_precovid,24))
comparison_table_ARIMA <- data.frame(test$passengers, p)
colnames(comparison_table_ARIMA) <- c('Actual Data','Prediction')

comparison_table_ARIMA

print('Holt-Winters additive model SSE is:')
SSE2_hw_additive

print('Holt-Winters multiplicative model SSE is:')
SSE2_hw_multiplicative

print('ARIMA model SSE is:')
SSE2_ARIMA

## Storing pre-covid and after-covid data to suitable variables
df_precovid <- subset(df, Date<'2020-01-01')
df_aftercovid  <- subset(df, Date >= '2020-01-01')

## Converting pre-covid and after-covid data to Time Series Objects
df_precovid <- ts(df_precovid$passengers, start = c(2005, 7), frequency = 12)
df_aftercovid <- ts(df_aftercovid$passengers, start = c(2020, 1), frequency = 12)

## Fitting Holt-Winters Additive Model
fit_hw_additive <- hw(df_precovid,seasonal="additive")
pred_hw_additive <- forecast(fit_hw_additive,h=12)

## Comparing actual after-covid data with the number of predicted passengers in after-covid period based on the pre-covid data 
df_precovid <- window(df_precovid,start=2005)
autoplot(df_precovid) +
  autolayer(df_aftercovid, series="Actual data", PI=FALSE) +
  autolayer(pred_hw_additive, series="HW additive forecasts", PI=FALSE )+
  xlab("Year") +
  ylab("Passengers") +
  ggtitle("Comparing the predictions with the actual data for after covid - Additive Model") +
  theme(plot.title = element_text(hjust =0.5))+
  guides(colour=guide_legend(title="Forecast"))
