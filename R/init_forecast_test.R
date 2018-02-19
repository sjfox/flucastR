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


## First get the embeddings for all regions and years being used
regions <- c("nat", "hhs1", "hhs2", "hhs3", "hhs4", "hhs5", "hhs6", "hhs7", "hhs8", "hhs9", "hhs10")
season_starts <- c(201040L, 201140L, 201240L, 201340L, 201440L, 201540L, 201640L, 201740L)
season_starts %>% map(get_all_regions_embedding, regions=regions) %>%
  bind_rows() -> seasonal_embeddings
seasonal_embeddings <- seasonal_embeddings %>%
  mutate(season = season_from_epi_week(epi_week)) %>%
  select(-epi_week)


## Load the baseline wili dictionary, and setup variables for prediction
test <- load_all_fluview("nat")
test %>% filter(season>=2010, week>=40) %>%
  select(epiweek) %>% pull -> large_weeks
test %>% filter(season>=2010, week<=20) %>%
  select(epiweek) %>% pull -> low_weeks
length(unique(c(low_weeks,large_weeks)))

results <- unique(c(low_weeks,large_weeks)) %>%
  map(get_all_regional_forecasts, seasonal_embeddings = seasonal_embeddings)




# Unused code that might be useful later ----------------------------------

# ## Test forecasting issues
#
# epi_week <- 201647L
# testcast <- get_regional_forecast(region = "hhs3", epi_week = epi_week)
#
#
# file_names <- list.files("forecasts", full.names = T)
# file_names %>% rev() %>% map(change_filename) %>% unlist()
# path <- file_names[1]
#
#
#
# change_filename_again <- function(path){
#   split_string <- str_split(path, pattern = "-")[[1]]
#
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
