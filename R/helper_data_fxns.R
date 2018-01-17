#############################################
## Created by Spencer Fox
## January 16, 2018
## Functions for loading/caching and working with the data
#############################################

load_baselines <- function(from_internet=FALSE){
  ## from_internet specifies whether to download from the github repo
  ## otherwise file must be in the data folder in repository

  if(from_internet){
    wili_baselines <- read_csv("https://raw.githubusercontent.com/FluSightNetwork/cdc-flusight-ensemble/master/baselines/wILI_Baseline.csv")
  } else{
    wili_baselines <- read_csv(here("data/wILI_Baseline.csv"))
  }

  wili_baselines <- wili_baselines %>%
    mutate(region =  case_when(location == "US National" ~ "nat",
                               location == "HHS Region 1" ~ "hhs1",
                               location == "HHS Region 2" ~ "hhs2",
                               location == "HHS Region 3" ~ "hhs3",
                               location == "HHS Region 4" ~ "hhs4",
                               location == "HHS Region 5" ~ "hhs5",
                               location == "HHS Region 6" ~ "hhs6",
                               location == "HHS Region 7" ~ "hhs7",
                               location == "HHS Region 8" ~ "hhs8",
                               location == "HHS Region 9" ~ "hhs9",
                               location == "HHS Region 10" ~ "hhs10"))
  wili_baselines <<- wili_baselines
}



load_all_fluview <- function(region) {
  ## Loads all of the historic fluview data for a given region
  ## region needs to be "nat", "hhs1", "hhs2", ... "hhs10"

  epidata_cache_dir = here("data-cache")
  if (!dir.exists(epidata_cache_dir)) {
    dir.create(epidata_cache_dir)
  }

  fetchEpidataHistoryDF(
    "fluview", region, 0:51,
    first.week.of.season = 31L,
    cache.file.prefix=file.path(epidata_cache_dir, paste0("fluview_", region))
  )
}

load_historic_fluview <- function(region, epi_week){
  ## Loads a snapshot of historic fluview data for a given date
  ##    some records get updated through time, so this should allow for realistic forecasts
  ## region needs to be "nat", "hhs1", "hhs2", ... "hhs10"
  ## epi_week needs to be in the form YYYYWW

  epi_data <- load_all_fluview(region)
  mimicPastEpidataDF(epi_data, epi_week)
}

convert_to_forecast_form <- function(df){
  ## takes a data frame and epi_week, and returns a vector ready for forecasting
  ## df must be df from load_historic_fluview()
  ## epi_week needs to be in form YYYYWW
  df %>% select(wili) %>%
    pull()
}
