# -- Load all the needed packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)

results_df <- read_delim("Data/forecast_results.csv", 
                         delim = ";",
                         col_types = cols(
                            p_auto.arima = col_double(),
                            p_arfima = col_double(),
                            p_tbats = col_double(),
                            p_ets = col_double(),
                            p_xgbar = col_double(),
                            p_nnetar = col_double(),
                            p_prophet_lin = col_double(),
                            p_es = col_double(),
                            p_ensemble = col_double(),
                            y = col_double()
                            )
                         ) %>% 
              gather(key = model, value = value, y:p_ensemble, 
                     na.rm = FALSE) %>%
              mutate(value = as.numeric(str_replace(value, ",", ".")))

models <- results_df %>%
  select(model) %>%
  unique()
