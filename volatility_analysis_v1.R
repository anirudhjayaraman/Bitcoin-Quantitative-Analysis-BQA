setwd("F:/Bitcoin-Quantitative-Analysis-BQA-")

## Load relevant libraries ------------------------------------------------
library(ggplot2); library(gridExtra)
library(tseries); library(zoo); library(forecast)
library(rugarch)

## Load and Prepare the Data ----------------------------------------------

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

# Discard rows upto 297, as the price series is 0 till here
prices <- prices[298:nrow(prices),]


# Create a variable for squared returns and absolute returns
prices$Absolute_Returns <- abs(prices$Log_Returns)
prices$Squared_Returns <- prices$Log_Returns^2

# Data is best taken 2014 onwards
prices <- prices[which(prices$Date == '2014-01-01'):nrow(prices),]

## Analysis ---------------------------------------------------------------
# Plot Bitcoin price series against time
p4 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Market_Price), col = 'blue', size = 1) + 
  xlab('Date') +
  ylab('Market Price') +
  ggtitle('Bitcoin Price Series')

# Plot Bitcoin log-returns against time
p5 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Log_Returns), col = 'red', size = 1) + 
  xlab('Date') +
  ylab('Log Returns') +
  ggtitle('Bitcoin Return Series')

# Plot Bitcoin Squared Returns against time
p6 <- ggplot() +
  geom_line(aes(x = prices$Date, y = prices$Squared_Returns), col = 'black', size = 1) + 
  xlab('Date') +
  ylab('Squared Returns') +
  ggtitle('Squared Return Series')

grid.arrange(p4, p5, p6, nrow = 3)

# Convert the return series into a zoo object (rt)
rt <- zoo(x = prices$Log_Returns, order.by = prices$Date)

# Convert the squared return series into a zoo object (et2) 
et2 <- zoo(x = prices$Squared_Returns, order.by = prices$Date)

# GARCH(1,2) Model using the library rugarch ------------------------------
# Fit the Model
garch12bit <- ugarchspec(variance.model = list(model = "sGARCH", 
                                               garchOrder = c(1, 2)), 
                         mean.model = list(armaOrder = c(0, 0)))
garch12bitfit <- ugarchfit(spec = garch12bit, data = rt, solver = "hybrid")

# Info Criteria for GARCH(1,2)
ic_garch12 <- infocriteria(garch12bitfit) # (-2*likelihood(garch12bitfit))/length(rt)+2*(length(garch12bitfit@fit$coef))/length(rt)
estimates_garch12 <- garch12bitfit@fit$robust.matcoef

# Estimated conditional variances
cond_var_12 <- zoo(x = garch12bitfit@fit$var, order.by = prices$Date)

p11 <- ggplot() + 
  geom_line(aes(x = prices$Date, y = et2), col = 'blue', size = 1) +
  geom_line(aes(x = prices$Date, y = cond_var_12), col = 'red', size = 1) +
  xlab('Date') + 
  ylab('Modeled Variance') +
  ggtitle('Squared Return Series vs GARCH(1,2) modeled variance')
grid.arrange(p11, nrow = 1)

garch12bitfitroll <- ugarchroll(spec = garch12bit, data = rt, n.start = 100,
                                refit.every = 1, refit.window = 'moving',
                                solver = 'hybrid', calculate.VaR = TRUE, 
                                VaR.alpha = 0.01, keep.coef = TRUE,
                                solver.control = list(tol = 1e-7, delta = 1e-9), 
                                fit.control = list(scale = 1))
# report(object = garch12bitfitroll, type = 'VaR', VaR.alpha = 0.01,conf.level = 0.99)
# plot(garch12bitfit)
# Forecasting using GARCH(1,2) 
bitcoin_return_preds <- ugarchboot(fitORspec = garch12bitfit, 
                                   method = c("Partial", "Full")[2], 
                                   n.ahead = 252, 
                                   n.bootpred = 252)
show(bitcoin_return_preds)
# plot(bitcoin_return_preds)
# bitcoin_return_preds@fseries
# bitcoin_return_preds@fsigma
# bitcoin_return_preds@bcoef
# bitcoin_return_preds@model
# bitcoin_return_preds@model$modeldata$meanfit.x
# bitcoin_return_preds@model$modeldata$meanfit.s
# bitcoin_return_preds@forc










