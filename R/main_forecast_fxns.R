#############################################
## Created by Spencer Fox
## January 16, 2018
## Master functions controlling the forecasts
#############################################


get_all_regional_forecasts <- function(epi_week, seasonal_embeddings, save=TRUE){
  regions <- c("nat", "hhs1", "hhs2", "hhs3", "hhs4", "hhs5", "hhs6", "hhs7", "hhs8", "hhs9", "hhs10")
  all_forecasts <- regions %>%
    map(get_regional_forecast, seasonal_embeddings = seasonal_embeddings, epi_week=epi_week) %>%
    bind_rows()

  # year <- as.numeric(substring(epi_week, 1,4))
  # year <- if_else(as.numeric(substring(epi_week, 5)) <= 20, year - 1, year)
  if(save){
    file_name <- paste0("EW", str_pad(week_from_epi_week(epi_week), 2, pad = "0"), "-", year_from_epi_week(epi_week), "-UTAustin_edm.csv")
    write_csv(all_forecasts, path = here(file.path("forecasts", file_name)))
  }else{
    return(all_forecasts)
  }
}



get_regional_forecast <- function(region, seasonal_embeddings, epi_week) {

  # Getting the data ready --------------------------------------------------
  # Load in the baselines for all seasons
  load_baselines()

  ## Load the mimicked fluview data for the date of interest
  fluview <- load_historic_fluview(region, epi_week)


  # Forecast forward through the season -------------------------------------
  ## Process that data into the proper form for forecasting
  forecast_ts <- convert_to_forecast_form(fluview, epi_week)

  ## Get the forecasts
  embedding <- extract_embedding(region, epi_week, seasonal_embeddings)
  forecasts <- make_edm_preds(forecast_ts, embedding)

  ## From the forecasts we simulate epidemic trajectories
  sims <- sim_pred_epi_traj(forecasts)


  # Process the forecasted trajectories and output --------------------------
  ## Summarizes the trajectories into convenient form for processing and output
  sim_summaries <- get_forecast_targets(sims, fluview)

  ## converts summary to proper format and returns it
  format_sim_data(sim_summaries, fluview)
}
