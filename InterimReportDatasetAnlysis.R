#libary####
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

#Data set#####
help(as.Date)

setwd("C:/Users/bosun/Desktop/HDip Data-Analysis/Semester 2/Final Project")

#Gold####

golddata = read.csv("Gold Data.csv")

golddata$date = as.Date(golddata$Date,"%Y-%m-%d")
golddata$date
mean(golddata$Price)


ggplot(golddata, aes(date,Price)) + geom_line() + scale_x_date("Month") + ylab ("Daily Gold Price") + xlab("")



str(golddata)




sautoplot(golddata)tart(golddata_ts)
end(golddata_ts)





copperdata = read.csv("Copper Data.csv")
str(copperdata)
class(copperdata)












oildata = read.csv("Oil Data.csv")
str(oildata)
class(oildata)

















#Data Analysis##### 



plot(mydata)

#from 1990 to 2022 (a)
 
#line
lp1a = plot(golddata$Date,golddata$Price,
            type = "l",
            main = "Line plot of Gold Historical Price From 1990 to 2022",
            xlab = "Years, 1990 to 2022",
            ylab = "U.S. Dollars per troy ounce",
            col = "orange")
lp1a

lp2a = plot(date,Copper,
            type = "l",
            main = "Line plot of Copper Historical Price From 1990 to 2022",
            xlab = "Years, 1990 to 2022",
            ylab = "U.S. Dollars per pound",
            col = "brown")
lp2a


lp3a = plot(date,Oil,
            type = "l",
            main = "Line plot of Oil Historical Price Price From 1990 to 2022",
            xlab = "Years, 1990 to 2022",
            ylab = "U.S. Dollars per barrel",
            col = "black")
lp3a


#box
bp1a = boxplot(Gold,
               main = "Boxplot of Gold Historical Price From 1990 to 2022",
               ylab = "U.S. Dollars per troy ounce",
               col = "orange")
bp1a


bp2a = boxplot(Copper,
               main = "Boxplot of Copper Historical Price From 1990 to 2022",
               ylab = "U.S. Dollars per pound",
               col = "brown")
bp2a

bp3a = boxplot(Oil,
               main = "Boxplot of Oil Historical Price Price From 1990 to 2022",
               ylab = "U.S. Dollars per barrel",
               col = "black")
bp3a




#from 1990 - 2000 (b)####

data19902000 = mydata[ 1:2532,]
data19902000


#line
lp1b = plot(date[ 1:2532],data19902000$Gold,
            type = "l",
            main = "Line plot of Gold Historical Price From 1990 - 2000",
            xlab = "Years, 1990 - 2000",
            ylab = "U.S. Dollars per troy ounce",
            col = "orange")
lp1b

lp2b = plot(date[ 1:2532],data19902000$Copper,
            type = "l",
            main = "Line plot of Copper Historical Price Price From 1990 - 2000",
            xlab = "Years, 1990 - 2000",
            ylab = "U.S. Dollars per pound",
            col = "brown")
lp2b


lp3b = plot(date[ 1:2532],data19902000$Oil,
            type = "l",
            main = "Line plot of Oil Historical Price Price From 1990 - 2000",
            xlab = "Years, 1990 - 2000",
            ylab = "U.S. Dollars per barrel",
            col = "black")
lp3b


#box
bp1b = boxplot(data19902000$Gold,
               main = "Boxplot of Gold Historical Price From 1990 - 2000",
               ylab = "U.S. Dollars per troy ounce",
               col = "orange")
bp1b


bp2b = boxplot(data19902000$Copper,
               main = "Boxplot of Copper Historical Price From 1990 - 2000",
               ylab = "U.S. Dollars per pound",
               col = "brown")
bp2b

bp3b = boxplot(data19902000$Oil,
               main = "Boxplot of Oil Historical Price Price From 1990 - 2000",
               ylab = "U.S. Dollars per barrel",
               col = "black")
bp3b



#from 2000 - 2010 (c)####
data20002010 = mydata[2532:5083,]
data20002010

#line
lp1c = plot(date[ 2532:5083],data20002010$Gold,
            type = "l",
            main = "Line plot of Gold Historical Price From 2000 - 2010",
            xlab = "Years, 2000 - 2010",
            ylab = "U.S. Dollars per troy ounce",
            col = "orange")
lp1c

lp2c = plot(date[ 2532:5083],data20002010$Copper,
            type = "l",
            main = "Line plot of Copper Historical Price Price From 2000 - 2010",
            xlab = "Years, 2000 - 2010",
            ylab = "U.S. Dollars per pound",
            col = "brown")
lp2c


lp3c = plot(date[ 2532:5083],data20002010$Oil,
            type = "l",
            main = "Line plot of Oil Historical Price Price From 2000 - 2010",
            xlab = "Years, 2000 - 2010",
            ylab = "U.S. Dollars per barrel",
            col = "black")
lp3c


#box
bp1c = boxplot(data20002010$Gold,
               main = "Boxplot of Gold Historical Price From 2000 - 2010",
               ylab = "U.S. Dollars per troy ounce",
               col = "orange")
bp1c


bp2c = boxplot(data20002010$Copper,
               main = "Boxplot of Copper Historical Price From 2000 - 2010",
               ylab = "U.S. Dollars per pound",
               col = "brown")
bp2c

bp3c = boxplot(data20002010$Oil,
               main = "Boxplot of Oil Historical Price Price From 2000 - 2010",
               ylab = "U.S. Dollars per barrel",
               col = "black")
bp3c



#from 2010 - 2020 (d)####
data20102020 = mydata[5083:7613,]
data20102020

#line#
lp1d = plot(date[ 5083:7613],data20102020$Gold,
            type = "l",
            main = "Line plot of Gold Historical Price From 2010 - 2020",
            xlab = "Years, 2010 - 2020",
            ylab = "U.S. Dollars per troy ounce",
            col = "orange")
lp1d

lp2d = plot(date[ 5083:7613],data20102020$Copper,
            type = "l",
            main = "Line plot of Copper Historical Price Price From 2010 - 2020",
            xlab = "Years, 2010 - 2020",
            ylab = "U.S. Dollars per pound",
            col = "brown")
lp2d


lp3d = plot(date[ 5083:7613],data20102020$Oil,
            type = "l",
            main = "Line plot of Oil Historical Price Price From 2010 - 2020",
            xlab = "Years, 2010 - 2020",
            ylab = "U.S. Dollars per barrel",
            col = "black")
lp3d


#box
bp1d = boxplot(data20102020$Gold,
               main = "Boxplot of Gold Historical Price From 2010 - 2020",
               ylab = "U.S. Dollars per troy ounce",
               col = "orange")
bp1d


bp2d = boxplot(data20102020$Copper,
               main = "Boxplot of Copper Historical Price From 2010 - 2020",
               ylab = "U.S. Dollars per pound",
               col = "brown")
bp2d

bp3d = boxplot(data20102020$Oil,
               main = "Boxplot of Oil Historical Price Price From 2010 - 2020",
               ylab = "U.S. Dollars per barrel",
               col = "black")
bp3d




#from 2020 - 2022 (e)####

data20202022 = mydata[7868:8145,]
data20202022

#line
lp1e = plot(date[ 7868:8145],data20202022$Gold,
            type = "l",
            main = "Line plot of Gold Historical Price From 2020 - 2022",
            xlab = "Years, 2020 - 2022",
            ylab = "U.S. Dollars per troy ounce",
            col = "orange")
lp1e

lp2e = plot(date[ 7868:8145],data20202022$Copper,
            type = "l",
            main = "Line plot of Copper Historical Price Price From 2020 - 2022",
            xlab = "Years, 2020 - 2022",
            ylab = "U.S. Dollars per pound",
            col = "brown")
lp2e


lp3e = plot(date[ 7868:8145],data20202022$Oil,
            type = "l",
            main = "Line plot of Oil Historical Price Price From 2020 - 2022",
            xlab = "Years, 2020 - 2022",
            ylab = "U.S. Dollars per barrel",
            col = "black")
lp3e


#box
bp1e = boxplot(data20202022$Gold,
               main = "Boxplot of Gold Historical Price From 2020 - 2022",
               ylab = "U.S. Dollars per troy ounce",
               col = "orange")
bp1e


bp2e = boxplot(data20202022$Copper,
               main = "Boxplot of Copper Historical Price From 2020 - 2022",
               ylab = "U.S. Dollars per pound",
               col = "brown")
bp2e

bp3e = boxplot(data20202022$Oil,
               main = "Boxplot of Oil Historical Price Price From 2020 - 2022",
               ylab = "U.S. Dollars per barrel",
               col = "black")
bp3e


gc = boxplot(data19902000$Gold,data20002010$Gold,data20102020$Gold, 
             col = 5:7,
             main = "Gold price compressing in decades",
             ylab = "U.S. Dollars per troy ounce",
             names=c("1990 to 2000", "2000 to 2010", "2010 to 2020"))
gc


cc = boxplot(data19902000$Copper,data20002010$Copper,data20102020$Copper, 
             col = 5:7,
             main = "Copper price compressing in decades",
             ylab = "U.S. Dollars per troy ounce",
             names=c("1990 to 2000", "2000 to 2010", "2010 to 2020"))
cc


oc = boxplot(data19902000$Oil,data20002010$Oil,data20102020$Oil, 
             col = 5:7,
             main = "Crude Oil price compressing in decades",
             ylab = "U.S. Dollars per troy ounce",
             names=c("1990 to 2000", "2000 to 2010", "2010 to 2020"))
oc


#Gold time series####
 
time_series = ts(values, start = 2015, frequency =12)


# decomposition
autoplot(decompose(mydata$Gold)) + ggtitle("Decomposition of the series") + theme(plot.title = element_text(size=8))



tsgold = xts(mydata$Gold,mydata$Date)

tscopper = xts(mydata$Copper, mydata$Date)






#time_series data##### 

class(mydata)

mydata$Date = as.Date(mydata$Date)

#gold
golddata_ts = ts(mydata[,2],start = c(1990,1,2), frequency = 1)
golddata_ts

class(golddata_ts)

is.ts(golddata_ts)

frequency(golddata_ts)

window(golddata_ts)

start(golddata_ts)

end(golddata_ts)

autoplot(golddata_ts)

ggseasonplot(golddata_ts, polar = T)

ggsubseriesplot(austres)

ggAcf(austres)

ggPacf(austres)



#copper
copperdata_ts = xts(mydata$Copper, mydata$Date)
copperdata_ts
class(copperdata_ts)

#oil
Oildata_ts = ts(mydata$Oil , mydata$Date)
Oildata_ts
class(Oildata_ts)




