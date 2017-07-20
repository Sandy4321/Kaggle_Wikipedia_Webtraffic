library(tidyverse)
library(timekit)
library(forecast)
library(lubridate)
library(opera)
library(prophet)
library(hydroGOF)  # for calculating rmse
library(forecastxgb)
library(stringr)
library(smooth)

source("Code/func_forecasts.R")

# -- Read the data
df_data <- read_csv("Data/train_1.csv") 



# -- get one example
df_data <- df_data %>%
  # -- choose one example 
  filter(Page == "2NE1_zh.wikipedia.org_all-access_spider") %>%
  # -- Transform data to long format
  gather( key = Date,  value = y, -Page) %>%
  # -- Transform date to date datatype
  mutate(Date = as.Date(Date))

start_test <- date("2016-11-02")
end_test <- date("2016-12-31")
start_pred <- date("2017-01-01")
end_pred <- date("2017-03-01")
forecast_horizon <- as.integer(end_pred - start_pred)

last_day_train <- max(df_data$Date) - forecast_horizon - 1

df_train <- filter(df_data, Date <= last_day_train)
df_test <- filter(df_data, Date > last_day_train)

# -- plot the example
ggplot(df_train, aes(x = Date, y = y)) +
  geom_line()

ts_train <- df_train %>%
  select(-Page) %>%
  tk_ts(start = 201507)

forecast_results <- multiple_forecasts(df_train, df_test)

write_delim(forecast_results, 
            path = "Shiny_Kaggle_Webtraffic/Data/forecast_results.csv",
            delim = ";")



  
  
   
   
  


