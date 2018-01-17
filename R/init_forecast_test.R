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

## Load the baseline wili dictionary, and setup variables for prediction
epi_week = 201801L

get_all_regional_forecasts(epi_week)


get_all_regional_forecasts <- function(epi_week, save=TRUE){
  regions <- c("nat", "hhs1", "hhs2", "hhs3", "hhs4", "hhs5", "hhs6", "hhs7", "hhs8", "hhs9", "hhs10")
  all_forecasts <- regions %>%
    map(get_regional_forecast, epi_week=epi_week) %>%
    bind_rows()

  if(save){
    file_name <- paste0("EW", substring(epi_week, 5), "-", substring(epi_week, 1,4), "-UTAustin_edm.csv")
    write_csv(all_forecasts, path = here(file.path("forecasts", file_name)))
  }else{
    return(all_forecasts)
  }
}




get_regional_forecast <- function(region, epi_week) {

  # Getting the data ready --------------------------------------------------
  # Load in the baselines for all seasons
  load_baselines()

  ## Load the mimicked fluview data for the date of interest
  fluview <- load_historic_fluview(region, epi_week)


  # Forecast forward through the season -------------------------------------
  ## Process that data into the proper form for forecasting
  forecast_ts <- convert_to_forecast_form(fluview)

  ## Get the forecasts
  forecasts <- make_edm_preds(forecast_ts)

  ## From the forecasts we simulate epidemic trajectories
  sims <- sim_pred_epi_traj(forecasts)


  # Process the forecasted trajectories and output --------------------------
  ## Summarizes the trajectories into convenient form for processing and output
  sim_summaries <- get_forecast_targets(sims, fluview)

  ## converts summary to proper format and returns it
  format_sim_data(sims_summaries, fluview)
}
