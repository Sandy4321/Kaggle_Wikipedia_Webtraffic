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
library(microbenchmark)

source("Code/func_forecasts.R")
source("Code/func_evaluation.R")

set.seed(999)

# -- Read the data
df_data <- read_csv("Data/train_1.csv") 

example_articles <- df_data %>%
  select(Page) %>%
  head(3) %>%
  pull()

# -- get one example
df_data <- df_data %>%
  # -- choose one example 
  filter(Page %in% example_articles) %>%
  # -- Transform data to long format
  gather( key = Date,  value = y, -Page) %>%
  # -- Transform date to date datatype
  mutate(Date = as.Date(Date)) %>%
  mutate(y = if_else(y > 0, y, 1L))

start_test <- date("2016-11-02")
end_test <- date("2016-12-31")
start_pred <- date("2017-01-01")
end_pred <- date("2017-03-01")
forecast_horizon <- as.integer(end_pred - start_pred)

last_day_train <- max(df_data$Date) - forecast_horizon - 1

df_train <- filter(df_data, Date <= last_day_train)
df_test <- filter(df_data, Date > last_day_train)

train_test_for <- function() {

  df_forecast_results <- tibble()
  for (i in 1:length(example_articles)) {
    df_forecast_results <- multiple_forecasts(df_train, df_test, example_articles[i]) %>%
      bind_rows(., df_forecast_results)
  }
  return(df_forecast_results)
}

train_test_par <- function() {
library(doParallel)
cl = makeCluster(4)
registerDoParallel(cl)
df_forecast_results <- foreach(i = 1:length(example_articles),
        .combine = bind_rows,
        .export = c("multiple_forecasts",
                    "start_test",
                    "df_train",
                    "df_test",
                    "example_articles",
                    "create_forecast", 
                    "df_to_ts",
                    "forecast_prophet",
                    "forecast_ensemble"),
        .packages = c("tidyverse", "timekit", "forecast", "forecastxgb",
                      "smooth", "prophet", "stringr", "opera")) %dopar%
  multiple_forecasts(df_train
                     , df_test, example_articles[i])
stopCluster(cl)
return(df_forecast_results)
}

res_benchmark <- microbenchmark(
  for_forecasts = train_test_for(),
  par_forecasts = train_test_par()
, times = 1L
)

df_forecast_results


# -- calc error metric smape
mean(calc_sm(forecast_results$y, forecast_results$p_ets), na.rm = TRUE)

# -- Write CSV File of forecasts for plotting in shiny
write_delim(forecast_results, 
            path = "Shiny_Kaggle_Webtraffic/Data/forecast_results.csv",
            delim = ";")

