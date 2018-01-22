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
length(unique(c(low_weeks,large_weeks)))
unique(c(low_weeks,large_weeks)) %>% map(get_all_regional_forecasts)

get_all_regional_forecasts(epi_week)


# file_names <- list.files("forecasts", full.names = T)
# file_names %>% rev() %>% map(change_filename) %>% unlist()
#
# change_filename_again <- function(path){
#   split_string <- str_split(path, pattern = "_")[[1]]
#   if( as.numeric(substring(split_string[1], 13, last = 14)) <= 20 ){
#       # split_string[[2]][2] <- as.numeric(split_string[[2]][2]) - 1
#       new_name <- paste(c(paste(split_string[1:3], collapse ="-"), split_string[4]), collapse = "_")
#       file.rename(from = path, to = new_name)
#       return("changed value")
#   } else{
#     return("didn't change")
#   }
# }
#
#
#
# file_names[1]
#
# file_names %>% map(change_filename) %>% unlist()
#
# change_filename <- function(path){
#   split_string <- str_split(path, pattern = "-")[[1]]
#   if( as.numeric(substring(split_string[1], 13, last = 14)) <= 20 ){
#       # browser()
#       split_string[2] <- as.numeric(split_string[2]) + 1
#       file.rename(from = path, to = paste(split_string, collapse = "-"))
#       return("changed value")
#   } else{
#     return("didn't change")
#   }
# }
#
#   file.rename()
