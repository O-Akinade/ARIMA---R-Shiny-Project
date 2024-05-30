#library####
library(lubridate)
library(seasonal)
library(deseasonalize)
library(tidyverse)
library(fpp2)
library(xts)
library(forecast)
library(ggplot2)
library(tseries)
library(rio)


setwd("C:/Users/bosun/Desktop/HDip Data-Analysis/Semester 2/Final Project")

golddata = read.csv("Gold Data.csv")


#Exploratory Data Analysis####

golddata$date = as.Date(golddata$Date,"%Y-%m-%d")

ggplot(golddata, aes(date, Price)) + geom_line() + scale_x_date("Month") +
  ylab("Daily Gold Price") +
  xlab("")

ggplot(golddata, aes(date, Price)) + geom_point(color = "yellow") + 
  facet_wrap(~ Month) + scale_x_date("Month") +  ylab("Daily Gold Price") +
  xlab("")

tsobjest = ts(golddata[,c("Price")], start =c(1979, 1), end = c(2022,02), frequency = 7)


golddata$clean_count = tsclean(tsobjest)

ggplot() +
  geom_line(data = golddata, aes(x = date, y = clean_count)) + ylab("clean Count")

golddata$cnt_ma = ma(golddata$clean_count, order = 5)
golddata$cnt_ma22 = ma(golddata$clean_count, order = 22)

ggplot() +
  geom_line(data = golddata, aes(x = date, y = clean_count, colour = "Counts")) +
  geom_line(data = golddata, aes(x = date, y = cnt_ma, colour = "weekly Moving Average ")) +
  geom_line(data = golddata, aes(x = date, y = cnt_ma22, colour = "Monthly Moving Average ")) +
  ylab("Gold Price")


#Seasonal text using moving average

count_ma = ts(na.omit(golddata$cnt_ma), frequency = 22)

decomp = stl(count_ma, s.window  = "periodic")
deseasonal_cnt = seasadj(decomp) # for ARIMA later
plot(decomp)


#Stationary test (Augmented-Dickey Test )
adf.test(count_ma, alternative = "stationary")
#data:  count_ma
#Dickey-Fuller = -0.9971, Lag order = 22, p-value = 0.9394
#alternative hypothesis: stationary


#Autocorrelation and Choosing model order
#ACF plot display correlation between a series and 
Acf(count_ma,main = "")
#PACF plot display correlation between a series and 
Pacf(count_ma,main = "")


#difference 
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)


adf.test(count_d1, alternative = "stationary")
#data:  count_d1
#Dickey-Fuller = -21.988, Lag order = 22, p-value = 0.01
#alternative hypothesis: stationary


#Lag point difference
Acf(count_d1, main = "ACF for Differenced Series")
Pacf(count_d1, main = "ACF for Differenced Series")




# Fitting An ARIMA Model
auto.arima(deseasonal_cnt,seasonal = FALSE)

fit = auto.arima(deseasonal_cnt,seasonal = FALSE)

tsdisplay(residuals(fit), lag.max = 45, main = "Seasonal Residuals")





#HWES####

x = decompose(tsobjest)
plot(x)

window(x)
