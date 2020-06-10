#Libraries
library(prophet)
library(lubridate)
library(ggplot2)
library(tidyverse)

#Bike sharing data
data <- read.csv(file.choose(), header = T)
data$dteday <- ymd(data$dteday)

#Plot
qplot(dteday, cnt, data = data,
      main = 'Bike Rentals in Washington DC')

#Data
ds <- data$dteday
y <- data$cnt
df <- data.frame(ds,y)
df$temp <- data$temp #this is regressor
df$hum <- data$hum #this is regressor

#Forcasting model
m <- prophet()
m <- add_country_holidays(m, country_name = 'US')
m <- add_regressor(m, 'temp')
m <- add_regressor(m, 'hum')
m <- fit.prophet(m, df)

#Prediction
future <- make_future_dataframe(m, periods = 10)
x <- data.frame(df$temp)
colnames(x) <- 'temp'
y <- data.frame(runif(10, 0.1, 0.3))
colnames(y) <- 'temp'
future$temp <- rbind(x,y)

x <- data.frame(df$hum)
colnames(x) <- 'hum'
y <- data.frame(runif(10, 0.4, 0.8))
colnames(y) <- 'hum'
future$hum <- rbind(x,y)

future <- as.matrix(future)
colnames(future) <- NULL
colnames(future) <- c('ds','temp','hum')
future <- data.frame(future)
future$ds <- ymd(future$ds)
future$temp <- as.numeric(as.character(future$temp))
future$hum <- as.numeric(as.character(future$hum))

forecast <- predict(m, future)

#Plot forecast
plot(m, forecast)
dyplot.prophet(m, forecast)

#Forecast components
prophet_plot_components(m, forecast)

#Model performance
pred <- forecast$yhat[1:731]
actual <- df[,2]
plot(actual,pred)
abline(lm(pred~actual), col = 'red')
summary(lm(pred~actual))
