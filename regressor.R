#Libraries
library(prophet)
library(lubridate)
library(ggplot2)
library(tidyverse)

#mengambi data dari drive komputer
data <- read.csv(file.choose(), header = T)
data$dteday <- ymd(data$dteday)

#melihat plot dasar hubungan jumlah penyewa dan waktu
qplot(dteday, cnt, data = data,
      main = 'Bike Rentals in Washington DC')

#preparasi
ds <- data$dteday
y <- data$cnt
df <- data.frame(ds,y)
df$temp <- data$temp #this is regressor
df$hum <- data$hum #this is regressor
df$windspeed <- data$windspeed #this is regressor


#Forcasting model
m <- prophet()
m <- add_country_holidays(m, country_name = 'US')
m <- add_regressor(m, 'temp')
m <- add_regressor(m, 'hum')
m <- add_regressor(m, 'windspeed')
m <- fit.prophet(m, df)

#Prediction model
future <- make_future_dataframe(m, periods = 10) #prediksi untuk 10 hari kedepan
x <- data.frame(df$temp)
colnames(x) <- 'temp'
y <- data.frame(runif(10, 0.1, 0.3)) #generate 10 random number with range 0.1 - 0.3
colnames(y) <- 'temp'
future$temp <- rbind(x,y)

x <- data.frame(df$hum)
colnames(x) <- 'hum'
y <- data.frame(runif(10, 0.4, 0.8))  #generate 10 random number with range 0.1 - 0.3
colnames(y) <- 'hum'
future$hum <- rbind(x,y)

x <- data.frame(df$windspeed)
colnames(x) <- 'windspeed'
y <- data.frame(runif(10, 0.1, 0.3))  #generate 10 random number with range 0.1 - 0.3
colnames(y) <- 'windspeed'
future$windspeed <- rbind(x,y)


future <- as.matrix(future)
colnames(future) <- NULL
colnames(future) <- c('ds','temp','hum','windspeed')
future <- data.frame(future)
future$ds <- ymd(future$ds)
future$temp <- as.numeric(as.character(future$temp))
future$hum <- as.numeric(as.character(future$hum))
future$windspeed <- as.numeric(as.character(future$windspeed))

forecast <- predict(m, future)

#Plot forecast
plot(m, forecast)
dyplot.prophet(m, forecast)

#Forecast components
prophet_plot_components(m, forecast)

#Model performance
prediction <- forecast$yhat[1:731]
actual <- df[,2]
plot(actual,prediction)
abline(lm(prediction~actual), col = 'red')
summary(lm(prediction~actual))
