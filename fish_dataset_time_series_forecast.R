library(tidyverse)
library(fpp)
library(forecast)
library(backtest)
library(quantmod)
library(lubridate)
library(dplyr)

Sale_of_fish <- read.csv("Fish dataset.csv",header=T)

#remove columns that are not interested
adj <- select(Sale_of_fish, -c("year","month","end.of.period","value..E.","unsold..Kg."))
#remove the space from the number and convert to numeric format
adj$sales <- as.numeric(gsub(' ','',adj$sales))
#convert to day, month and year format
adj$start <- dmy(adj$start)
#log(sales from the last period) - log(sales of the current period)
adj$logr <- log(lag(adj$sales)) - log(adj$sales)

#remove NA
narm <- function (x)
{
  x[is.na(x)] <- 1
  return(x)
}

adj$logr <- narm(adj$logr)

max_date <- max(adj$start)
min_date <- min(adj$start)

test_ts <- ts(adj$logr,end = c(year(max_date),month(max_date)),start = c(year(min_date),month(min_date)), frequency = 12)

plot(test_ts)
#raw data, seasonality, trend, residuals
plot(stl(test_ts,s.window ="periodic"))

acf = acf(test_ts, main = 'ACF plot', lag.max = 100)
pacf.logr = pacf(test_ts,main = 'PACF plot',lag.max = 100)

#Augmented Dickey-Fuller(ADF) Test
print(adf.test(test_ts))
#The obtained p-value < 0.05, therefore it is safe to assume that the dataset is stationary

m1 <- auto.arima(test_ts, seasonal = TRUE)
summary(m1)
accuracy(forecast(m1))

checkresiduals(m1) 