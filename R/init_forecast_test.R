#############################################
## Created by Spencer Fox
## January 16, 2018
## First flu forecasting with EDM attempt
#############################################

library(tidyverse)
library(rEDM)
library(epiforecast)
library(cowplot)
library(here)

source(here("R/helper_data_fxns.R")) ## Loading and using the epi data
source(here("R/helper_forecast_fxns.R")) ## Important functions for forecasting
source(here("R/helper_pred_parsing_fxns.R")) ## Taking predictions and parsing for outputting
source(here("R/main_forecast_fxns.R")) ## Main functions automating the forecasts


## Load the baseline wili dictionary, and setup variables for prediction
epi_week = 201801L

test <- load_all_fluview("nat")
test %>% filter(season>=2010, week>=40) %>%
  select(epiweek) %>% pull -> large_weeks
test %>% filter(season>=2010, week<=20) %>%
  select(epiweek) %>% pull -> low_weeks

unique(c(low_weeks,large_weeks)) %>% map(get_all_regional_forecasts)

get_all_regional_forecasts(epi_week)

