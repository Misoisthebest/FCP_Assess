##################################################################################

# This file loads weekly sales data for an auto-sales company
# We will use this data to forecast the next several months of
# sales per week.

# Created by Noel

##################################################################################


# Clearing the startup screen and console 

cat("\014")
rm(list=ls())

# Loading in the packages needed

library(tidyverse)
library(tidyquant)
library(timetk)
library(readr)
library(forecast)
library(fpp2)
library(sweep)
library(ggplot2)
library(ggforce)

# Setting working directory and loading in the data

setwd("C:/Users/nghahn/Desktop/SKU_forecast/")
SKUs <- read.csv(file = 'Forecast_Sample_File.csv', header = TRUE)
head(SKUs)
SKUs$period <- as.Date(SKUs$period)#the column of dates was in a character format

unique_SKUs <- unique(SKUs$unique_skus_id)


# Creating .csv files for each of the unique SKUs, and also creating objects for
# each of the SKUs in the global environment
for(i in unique_SKUs){
  write_csv(filter(SKUs, unique_skus_id == i), path = paste0(i, ".csv"))
}

for(i in SKUs$unique_skus_id){
  assign(paste0("SKU_",as.character(i)), SKUs %>% filter(unique_skus_id == i), envir = .GlobalEnv)
}

##################################################################################
# From here, I am going to do a thorough time series analysis of an individual SKU
##################################################################################

# Declaring the first SKU as time series data
ts_SKU_1 <- ts(SKU_1$sales, start = c(2018,13), end = c(2021,12), frequency = 52)


##################################################################################
# Preliminary Analysis
##################################################################################

# Time plot
autoplot(ts_SKU_1) +
  ggtitle("SKU 1: Sales per Week") +
  ylab("Sales") +
  xlab("Year")
# There is a strong trend, so going to investigate transformations

# Taking the difference to remove the trend
ts_SKU_1_diff <- diff(ts_SKU_1)

# Time plot of differenced data
autoplot(ts_SKU_1_diff) +
  ggtitle("SKU 1: Change in Sales per Week") +
  ylab("Sales") +
  xlab("Year")

# Investigating seasonality
ggseasonplot(ts_SKU_1_diff, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Sales") +
  ggtitle("Seasonal plot: SKU_1")
ggsubseriesplot(ts_SKU_1_diff)
# From visual inspection, there does not appear to be clear seasonality.
# However, there are peaks and dips in sales at certain parts of the year.
# For example, it appears that towards the end of the 52-week period, there
# is a dip in sales in the three years

# Checking the plots of the Sales with the trend and seasonal patterns separated
# from the observed data
plot(decompose(ts_SKU_1))
plot(decompose(ts_SKU_1, type = "multiplicative"))

##################################################################################
# This SKU_1 shows a trend and some seasonality
# Will continue with forecast with various methods
##################################################################################

###################
# Forecasting
###################

# Using simple benchmarking methods

bfit <- snaive(ts_SKU_1_diff) # Residual SD = 36.8323
print(summary(bfit))
checkresiduals(bfit)

# Using the benchmarking methods on the data itself
mean_1 <- meanf(ts_SKU_1, h = 26)
naive_1 <- naive(ts_SKU_1, h = 26)
driftm_1 <- rwf(ts_SKU_1, h = 26, drift = T)
snaive_1 <- snaive(ts_SKU_1, h = 26)

# Plotting the data along with the benchmarking forecasting methods
plot(mean_1)
lines(naive_1$mean, col = 2, lty = 1)
lines(driftm_1$mean, col = 3, lty = 1)
lines(snaive_1$mean, col = 6, lty = 1)
legend("topleft", legend = c("Mean", "Naive", "Drift", "Seasonal Naive"), lty = 1, col = c(1,2,3,6))

# Checking the residuals for the seasonal naive method
resid_n_SKU_1 <- residuals(snaive_1)
checkresiduals(snaive_1)


# In this section, I use a training and test set to evaluate forecasting accuracy
# Designating the training set and test sets
SKU_1_train <- window(ts_SKU_1, start = c(2018,13), end = c(2020,52))
SKU_1_test <- window(ts_SKU_1, start = c(2021,1))

# Plot of the training set
autoplot(SKU_1_train) + ggtitle("SKU 1") + ylab("Sales") + xlab("Year")
ggseasonplot(SKU_1_train, year.labels = TRUE, year.labels.left = TRUE) + ylab("Sales") + ggtitle("Seasonal plot: SKU_1")

# Using benchmarking forecasting methods on the training set
mean_1_train <- meanf(SKU_1_train, h = 26)
naive_1_train <- naive(SKU_1_train, h = 26)
driftm_1_train <- rwf(SKU_1_train, h = 26, drift = T)
snaive_1_train <- snaive(SKU_1_train, h = 26)

# Plot of the training set along with the benchmarking forecasts and test set
plot(mean_1_train)
lines(naive_1_train$mean, col = 2, lty = 1, lwd = 3)
lines(driftm_1_train$mean, col = 3, lty = 1, lwd = 3)
lines(snaive_1_train$mean, col = 6, lty = 1, lwd = 3)
lines(SKU_1_test, col = 5, lty = 1, lwd = 3)
legend("topleft", legend = c("Mean", "Naive", "Drift", "Seasonal Naive"), lty = 1, col = c(1,2,3,6))

# Investigating the accuracy of each of the forecasting models
accuracy(mean_1_train,SKU_1_test) # Training MASE = 0.739, Test MASE = 1.345
accuracy(naive_1_train,SKU_1_test) # Training MASE = 0.441, Test MASE = 1.343
accuracy(driftm_1_train,SKU_1_test) # Training MASE = 0.442, Test MASE = 1.243
accuracy(snaive_1_train,SKU_1_test) # Training MASE = 1.00, Test MASE = 0.92
# It appears, from the lowers MASE, the seasonal naive forecasting method is
# the most accurate


###################
# Fit on ETS method
###################

ets_ts_SKU_1 <- ets(ts_SKU_1) # Residual SD = 22.684
print(summary(ets_ts_SKU_1))
checkresiduals(ets_ts_SKU_1)
# The smaller residual SD indicates a better fit
# The residuals look good.

###################
# Fit on ARIMA method
###################

arima_ts_SKU_1 <- auto.arima(ts_SKU_1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
# Residual SD = 22.172
print(summary(arima_ts_SKU_1))
checkresiduals(arima_ts_SKU_1)
# The residual SD is slightly smaller than the residual SD of the ETS model

###################
# Forecasting!
###################

# Because the ARIMA model fits the best due to the low residual SD,
# I am using it to forecast

fcast_SKU_1 <- forecast(arima_ts_SKU_1, h = 26)
autoplot(fcast_SKU_1)
print(summary(fcast_SKU_1))




##################################################################################
# Looping through all of the SKUs using the ets forecasting model
##################################################################################

# Here, each time series is nested by SKU
weekly_sales_by_SKU_nest <- SKUs %>%
  group_by(unique_skus_id) %>%
  nest()

# Adds a column (data.ts) then applies the tk_ts function to the data
weekly_sales_by_SKU_ts <- weekly_sales_by_SKU_nest %>%
  mutate(data.ts = map(.x = data,
                       .f = tk_ts,
                       select = -period,
                       start = c(2018,13),
                       freq = 52))
weekly_sales_by_SKU_ts

# Add a column (fit.ets), then applies the ets function to the data
weekly_sales_by_SKU_fit <- weekly_sales_by_SKU_ts %>%
  mutate(fit.ets = map(data.ts, ets))
weekly_sales_by_SKU_fit

# Getting the model parameters for each nested list
weekly_sales_by_SKU_fit %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = unique_skus_id, value = estimate)

# Viewing model accuracies
weekly_sales_by_SKU_fit %>%
  mutate(glance = map(fit.ets, sw_glance)) %>%
  unnest(glance)

# Creating decompositions
weekly_sales_by_SKU_fit %>%
  mutate(decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(decomp)

# Forecasting the model. We are doing a 26-week forecast
weekly_sales_by_SKU_fcast <- weekly_sales_by_SKU_fit %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 26))
weekly_sales_by_SKU_fcast

# Tidying up the data frame
weekly_sales_by_SKU_fcast_tidy <- weekly_sales_by_SKU_fcast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)
weekly_sales_by_SKU_fcast_tidy

# Plotting the data

fcast_weekly_SKU_sales <- weekly_sales_by_SKU_fcast_tidy %>%
  ggplot(aes(x = index, y = sales, color = key, group = unique_skus_id)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Sales by SKU",
       subtitle = "ETS Model Forecasts",
       x = "", y = "Sales") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap_paginate(~ unique_skus_id, scales = "free_y", nrow = 5, ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p

required_n_pages <- n_pages(p)

# I had to break the plot up into several pages so that they would be visible
for(i in 1:required_n_pages){
  weekly_sales_by_SKU_fcast_tidy %>%
    ggplot(aes(x = index, y = sales, color = key, group = unique_skus_id)) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
                fill = "#D5DBFF", color = NA, size = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
                fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    geom_line() +
    labs(title = "Sales by SKU",
         subtitle = "ETS Model Forecasts",
         x = "", y = "Sales") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_color_tq() +
    scale_fill_tq() +
    facet_wrap_paginate(~ unique_skus_id, scales = "free_y", nrow = 5, ncol = 3, page = i) +
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
  print(p)
}
