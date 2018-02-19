########################################
## Script to begin analyzing forecasts
########################################

library(tidyverse)
library(epiforecast)
library(cowplot)
library(here)

# source(here("R/helper_data_fxns.R")) ## Loading and using the epi data
# source(here("R/helper_forecast_fxns.R")) ## Important functions for forecasting
# source(here("R/helper_pred_parsing_fxns.R")) ## Taking predictions and parsing for outputting


forecast_files <- list.files("forecasts", full.names = T)

check_csv_na <- function(path){
  path <- forecast_files[1]
  df <- read_csv(path)
  if(any(is.na(df$Value))){
    return(path)
  }
  return(NA)
}

any_na_vec <- forecast_files %>% map(check_csv_na) %>%
                unlist()

any(!is.na(any_na_vec)) ## If this is true, there is a definite error

