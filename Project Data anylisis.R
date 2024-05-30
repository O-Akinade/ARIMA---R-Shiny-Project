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
library(quantmod)
library(timeSeries)
library(pdfetch)
library(readr)
library(dplyr)
library(scales)
library(TSPred)
 


#Data set set up,
# YAhoo stock code, Gold = "GC=F", Oil = "CL=F", copper = "HG=F", silver = SI=F




gold_data = na.locf(getSymbols(Symbols = "GC=F", 
                        auto.assign = FALSE,
                        from = "2007-01-02",
                        to = Sys.Date()),fromLast = TRUE)

gold_data$close = gold_data$`GC=F.Close`


oil_data = na.locf(getSymbols(Symbols = "CL=F", 
                        auto.assign = FALSE,
                        from = "2007-01-02",
                        to = Sys.Date()),fromLast = TRUE)

oil_data$close = oil_data$`CL=F.Close`


copper_data = na.locf(getSymbols(Symbols = "HG=F", 
                       auto.assign = FALSE,
                       from = "2007-01-02",
                       to = Sys.Date()),fromLast = TRUE)

copper_data$close = copper_data$`HG=F.Close`


silver_data = na.locf(getSymbols(Symbols = "SI=F", 
                          auto.assign = FALSE,
                          from = "2007-01-02",
                          to = Sys.Date()),fromLast = TRUE)

silver_data$close = silver_data$`SI=F.Close`


#ggplot Multiple plot

combined_data = data.frame(date = index(gold_data),
                           gold_data,
                           row.names = NULL) %>%
  select(date, close = close) %>%
  mutate(ticker = "GC=F" ) %>%
  bind_rows(., 
            data.frame(date = index(oil_data),
                       oil_data,
                       row.names = NULL) %>%
              select(date, close  = close ) %>%
              mutate(ticker = "CL=F")) %>%
  bind_rows(.,
            data.frame(date = index(copper_data),
                       copper_data,
                       row.names = NULL) %>%
              select(date, close  = close ) %>%
              mutate(ticker = "HG=F"))%>%
bind_rows(.,
          data.frame(date = index(silver_data),
                     silver_data,
                     row.names = NULL) %>%
            select(date, close  = close ) %>%
            mutate(ticker = "SI=F"))


combined_data

combingg = ggplot(data = combined_data,
                  aes(x = date, y = close, color = ticker)) +
  geom_line()

combingg


combined_data2 = data.frame(date = index(oil_data),
                       oil_data,
                       row.names = NULL) %>%
              select(date, close  = close ) %>%
              mutate(ticker = "CL=F") %>%
  bind_rows(.,
            data.frame(date = index(copper_data),
                       copper_data,
                       row.names = NULL) %>%
              select(date, close  = close ) %>%
              mutate(ticker = "HG=F"))%>%
  bind_rows(.,
            data.frame(date = index(silver_data),
                       silver_data,
                       row.names = NULL) %>%
              select(date, close  = close ) %>%
              mutate(ticker = "SI=F"))


combined_data2

combingg2 = ggplot(data = combined_data2,
                  aes(x = date, y = close, color = ticker)) +
  geom_line()

combingg2


#Arima MOduel building

# Arima is a forcasting augridem that takes into account the past value t predict the feuture value, 
# as there re information n the past values that an be indicative to the future value,
# yt = intercept + lags (AR) + errrors(MA)
# Arima three therms are P = order of the auto regerscive  term.
# D = order of diffrening required to make the time serise stationary, 
# q = the order ofthe MA term
# stationary time serice is rquried to fit an arima module, stationary referss to the fact that the price serise is mean reverting,
# which in most case is not the case as priceses dont mean revert usally and the one that are are very rare.
# otherwise it will be easy to pridit and profite of.
# but the returnes are more liklry to  mean revert as they are distuduted randomly around a zero mean, this is diffrensing.
# we need a stationary time serise in other to modlled  an arima model, the reason is due to the autoregressive term .
# beause it auto regresive means the model is a liner regression  that uses it own lags as predictore,
# liner regaresion works best when the features when the predictores are not dependent on each other,
# cause if they are dependent there is a risk of runing into a multicollinearity in the seanarion of a multiconinearity
# predictores variable are correlated, makeing the regression unstable and might not reach a result.
# it important that the predicticore for the variales to indepenant from each other and not be correlated.
# as so it is important that the tier serise is stationary or at least close to stationary as a perect statinarity might not be achiveable but .
# a middly stationary time serise to ba able to module with an arima module. 


# Stationary test using ADF test(Augmented Dickey Fuller)

#H1 The null hypothesis of the ADF test is that the time series is non-stationary
#Ho the Alternative hypothesis of the ADF is that the time series is stationary

#If the p-value is below the significant level (0.05) then we can rejected the null hypothesis and 
#infer that the time series is indeed stationary
# if the p-value is >0.05 the order of difference is needed.


#by computing the returns as they randomly disputed around zero,
#is is done by subtracting the previous value from the current value. 
#if a single difference is done one might not get a stationary time series and
# more than 1 differences will be needed in that case.which is getting the return of the return.
# the order of differing is the d in the aroma module.
# the order of difference need can be computed manual by running a ACF   


#order of differing determining by checking coefficients of correlation between a time series and it lagged values. 
#Simply stated: ACF explains how the present value of a given time series is correlated with the past
#Autocorrelation Function ACF (d)

#ndiffs function can be use to estimate the order of differing  



# P is order of Auto regressive term (AR) in the arima model, 
#which refers to the number of order or lag that will be use as a predictor

# Partical auto correlation plot (Pacf) function can be use to determine  this number
# Pacf plot represent the correlation between the time series and it lags



# the oreder of the moving average (MA)portion of the module is represented by q, 
# q refrees to the lag forecast error that should do into the module
# the acf plot can be use to determine the number for the MA term
#acf of the dirrened serise is used

gold_diff_acf_plot = acf(na.omit(gold_diff))

# looking at the plot the order of 0 or 9may suitable as it above the order of significant line .






#Gold Analysis

tsdisplay(gold_data$close)

gold_auto = auto.arima(gold_data$close, seasonal = FALSE)

tsdisplay(residuals(gold_auto), lag.max = 40, main = "(0,1,0) Model Residuals")

plot.ts(gold_data$close,type = "l", col = "blue")
lines(fitted(gfita), col = "orange")


#ARIMA Manual Modeling

# Stationary check Using(D)
# Augmented Dickey-Fuller Test

adf.test(gold_data$close)

#data:  gold_data$close
#Dickey-Fuller = -1.8468, Lag order = 15, p-value = 0.6432
#p-value of 0.653 is above the significant level of 0.05, 
#so the time series is non-stationary and  order of difference is needed 

#Autocorrelation Function ACF to determine order of difference (d)

gold_acf = acf(gold_data$close)

# the ACF(auto correlation plot) plot shows a strong auto correlation structure,
# with the first lag having the correlation coefficient of 1,
# this mean differences order of 1 of lag of 1,should get the time series close to stationary 

#auto difference 

ndiffs(gold_data$close, test = "adf")

#using the ndiffs to automatic get the order of difference showed that order of 1 will be ideal(D=1)

gold_diff = diff(gold_data$close, lag = 1)

gold_diff_plot= plot(gold_diff)

tsdisplay(gold_diff)


# Partial auto correlation plot (PACF) function to determine order of Auto regressive term (AR) for the ARIMA model

gold_Pacf=Pacf(gold_data$close)

gold_Pacf1=Pacf(gold_diff)

# Pacf plot shows that lag number 9 shows the strongest correlation, and wll be use in the moduel(P = 9).


# the ACF plot is be use to determine the number for the MA term
#acf of the dirrened series is used

gold_diff_acf_plot = acf(na.omit(gold_diff))

# looking at the plot the order of 0 or 9may suitable as it above the order of significant line .


#Gold close Manual Module

gold_model = forecast::Arima(gold_data$close, c(9,1,9))

plot(gold_data$close,type = "l")


gplot = plot.ts(gold_data$close,type = "l", col = "red")
lines(fitted(gold_model), col = "black")


#Gold Forecasting
par(mfrow = c(1,2))

gold_fcastA = forecast(gold_auto, h = 100)
plot (gold_fcastA)


gold_fcastM = forecast(gold_model, h = 100)
plot (gold_fcastM)

#Gold Mape Accuracy

accuracy(gold_fcastA)
100 - 0.7906908

#Mape value of 0.7906908 - 1 = 99.20931

accuracy(gold_fcastM)
100 - 0.7911577
#Mape value of 0.7906908 - 1 = 99.20884
