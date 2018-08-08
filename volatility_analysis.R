setwd("F:/Bitcoin-Quantitative-Analysis-BQA-")

# load relevant libraries
library(ggplot2); library(gridExtra)
library(tseries); library(zoo); library(forecast)
library(rugarch)

# print all CSV files int the working directory
drfiles <- list.files()
drfiles[grepl('csv', drfiles)]

# Load Bitcoin price series
prices <- read.csv('market-price.csv', header = F)
names(prices) <- c('Date', 'Market_Price')

# Alter the date variable
prices$Date <- as.Date(strptime(prices$Date,format = "%Y-%m-%d %H:%M:%S"))
prices$Month <- months(prices$Date)
prices$Year <- as.numeric(substring(prices$Date,1,4))

# create log returns variable
prices$Log_Returns <- append(0,
                             log(prices$Market_Price[2:nrow(prices)]) 
                             - log(prices$Market_Price[1:(nrow(prices) - 1)]))

# 0/0 errors ==> NaNs
# basically, the first 295 market prices
which(is.nan(log(prices$Market_Price[2:nrow(prices)]) 
             - log(prices$Market_Price[1:(nrow(prices) - 1)])))



# value / 0 ==> Inf
# because the 296th market price turns positive from 0
which(is.infinite(log(prices$Market_Price[2:nrow(prices)]) 
                  - log(prices$Market_Price[1:(nrow(prices) - 1)])))

# Discard rows upto 297, as the price series is 0 till here
prices <- prices[298:nrow(prices),]


# Create a variable for squared returns and absolute returns
prices$Absolute_Returns <- abs(prices$Log_Returns)
prices$Squared_Returns <- prices$Log_Returns^2

# Plot Bitcoin price series against time
p1 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Market_Price), col = 'blue') + 
  xlab('Date') +
  ylab('Market Price') +
  ggtitle('Bitcoin Price Series')

# Plot Bitcoin log-returns against time
p2 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Log_Returns), col = 'red') + 
  xlab('Date') +
  ylab('Log Returns') +
  ggtitle('Bitcoin Return Series')

# Plot Bitcoin Squared Returns against time
p3 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Squared_Returns), col = 'black') + 
  xlab('Date') +
  ylab('Squared Returns') +
  ggtitle('Squared Return Series')


# # Plot zoo object
# plot(zoo(x = prices$Squared_Returns, order.by = prices$Date),
#      xlab = 'Date',
#      ylab = 'Squared Returns')

grid.arrange(p1, p2, p3, nrow = 3)

# It's clear that the data is best taken 2014 onwards
prices <- prices[which(prices$Date == '2014-01-01'):nrow(prices),]

# Plot Bitcoin price series against time
p4 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Market_Price), col = 'blue') + 
  xlab('Date') +
  ylab('Market Price') +
  ggtitle('Bitcoin Price Series')

# Plot Bitcoin log-returns against time
p5 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Log_Returns), col = 'red') + 
  xlab('Date') +
  ylab('Log Returns') +
  ggtitle('Bitcoin Return Series')

# Plot Bitcoin Squared Returns against time
p6 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Squared_Returns), col = 'black') + 
  xlab('Date') +
  ylab('Squared Returns') +
  ggtitle('Squared Return Series')

grid.arrange(p4, p5, p6, nrow = 3)

# Conver the return series into a zoo object (rt)
rt <- zoo(x = prices$Log_Returns, order.by = prices$Date)

# Check whether returns can be modeled as ARIMA class of models
# Visual Inspection clearly shows time series models can't be fit here
par(mfrow = c(2,1)); acf(rt); pacf(rt); par(mfrow = c(1,1))

# Convert the squared return series into a zoo object (ht) 
et2 <- zoo(x = prices$Squared_Returns, order.by = prices$Date)


# Generate conditional variance series
set.seed(1010)
vt <- rnorm(n = nrow(prices), mean = 0, sd = 1)
vt2 <- zoo(x = vt^2,
          order.by = prices$Date)
ht <- et2/vt2
p7 <- ggplot() + 
  geom_line(aes(x = prices$Date, y = ht), col = 'blue') +
  xlab('Date') + 
  ylab('Conditional Variance') +
  ggtitle('Conditional Variance of Return Series')
p8 <- ggplot() + 
  geom_line(aes(x = prices$Date, y = vt), col = 'red') +
  xlab('Date') + 
  ylab('Standard Normal Random Variable') +
  ggtitle('Time Series of Normal Independently Distributed Random Variable')
p9 <- ggplot() + 
  geom_line(aes(x = prices$Date, y = et2), col = 'black') +
  xlab('Date') + 
  ylab('Squared Returns') +
  ggtitle('Squared Return Series')
grid.arrange(p7, p8, p9, nrow = 3)


# Plot autocorrelations and partial autocorrelations
# Clearly, the conditional variance series cannot be modeled as ARMA
par(mfrow = c(2,1))
acf(ht) # cuts off at lag 1
pacf(ht) # cuts off at lag 2
par(mfrow = c(1,1))

# GARCH(1,1) Model using the library rugarch
garch11bit <- ugarchspec(variance.model = list(model = "sGARCH", 
                                               garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(0, 0)))
garch11bitfit <- ugarchfit(spec = garch11bit, data = rt, solver = "hybrid")
garch11bitfit
names(garch11bitfit@fit)
names(garch11bitfit@model)

garch11bitfit@fit$condH # [1] 6.961193

# Estimated conditional variances
cond_var <- zoo(x = garch11bitfit@fit$var, order.by = prices$Date)

p10 <- ggplot() + 
  geom_line(aes(x = prices$Date, y = et2), col = 'blue') +
  xlab('Date') + 
  ylab('Squared Returns') +
  ggtitle('Squared Return Series')
p11 <- ggplot() + 
  geom_line(aes(x = prices$Date, y = cond_var), col = 'red') +
  xlab('Date') + 
  ylab('Conditional Variances') +
  ggtitle('Modeled Variance')
grid.arrange(p10, p11, nrow = 2)


























