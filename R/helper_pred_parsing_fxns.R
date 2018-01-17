#############################################
## Created by Spencer Fox
## January 16, 2018
## Functions for parsing the predictions and outputting to csv
#############################################

gen_rnorm_sims <- function(forecasts){
  ## Generates random sims from the normal distribution of the forecasts
  ## Any values less than zero are set to 0
  sims <- rnorm(nrow(forecasts), mean = forecasts$pred, sd = sqrt(forecasts$pred_var))
  sims[sims < 0] <- 0
  sims
}

sim_pred_epi_traj <- function(forecasts, n=1000){
  ## Simulates epidemic trajectories based on the forecast and data
  replicate(n, gen_rnorm_sims(forecasts)) %>% as_data_frame()
}




get_seasonal_targets <- function(pred_ts, past_ts, baseline){
  ## Finds the targets based on a single time series
  ## See: https://predict.phiresearchlab.org/post/57f3f440123b0f563ece2576 for target definitions
  ts <- c(past_ts, pred_ts)
  runs_above_baseline <- rle(ts >= baseline)

  start_ind = match(T, (runs_above_baseline$lengths > 2 & runs_above_baseline$values == T))
  if(is.na(start_ind)){
    start_ind = "none"
  }
  start_ind = ifelse(start_ind==1, 1, sum(runs_above_baseline$lengths[1 : (start_ind - 1)]) + 1)

  peak_intensity <- max(ts, na.rm = T)
  peak_ind <- match(peak_intensity, ts)

  data_frame(ahead_1 = pred_ts[1],
             ahead_2 = pred_ts[2],
             ahead_3 = pred_ts[3],
             ahead_4 = pred_ts[4],
             start_ind = start_ind,
             peak_intensity = peak_intensity,
             peak_ind = peak_ind
  )
}

get_season_baseline <- function(epi_data){
  ## Gets the baseline threshold for a given season of fluview data
  wili_baselines %>% filter(region == unique(epi_data$region),
                            year == unique(epi_data$season)) %>%
    select(value) %>% pull
}

get_curr_season_data <- function(epi_data){
  ## Returns the full season of data for the most recent season in the data
  ## Keeps in the rows with NAs
  curr_season <- epi_data %>% tail(n=1) %>% select(season) %>% pull()
  epi_data %>% filter(season==curr_season)
}

get_forecast_targets <- function(sims, epi_data){
  ## For a given flu season, takes the sims and seasonal data,
  ## calculates the forecast targets for the CDC challenge.
  curr_data <- get_curr_season_data(epi_data)

  baseline <- get_season_baseline(curr_data)

  ## For each simulation, now calculate the targets
  sims %>%
    map(~get_seasonal_targets(pred_ts=., past_ts = curr_data$wili[!is.na(curr_data$wili)], baseline=baseline)) %>%
    bind_rows()
}



format_sim_data <- function(sim_summaries, epi_data){
  ## Massive function to get all the summary sim data into properly
  ## formatted data_frame and return it

  ## Initialize some common variables
  n <- nrow(sim_summaries)
  curr_data <- get_curr_season_data(epi_data)

  ## Setup the percentage data_frames
  prcnt_brks <- c(seq(0,13,by=0.1), 100)

  percent_sums <- data_frame(Bin_start_incl = prcnt_brks[-length(prcnt_brks)],
                             Bin_end_notincl = prcnt_brks[-1],
                             Unit = "percent") %>%
    mutate(`1 wk ahead` = hist(sim_summaries$ahead_1, prcnt_brks, plot = FALSE, right = FALSE)$counts / n,
           `2 wk ahead` = hist(sim_summaries$ahead_2, prcnt_brks, plot = FALSE, right = FALSE)$counts / n,
           `3 wk ahead` = hist(sim_summaries$ahead_3, prcnt_brks, plot = FALSE, right = FALSE)$counts / n,
           `4 wk ahead` = hist(sim_summaries$ahead_4, prcnt_brks, plot = FALSE, right = FALSE)$counts / n,
           `Season peak percentage` = hist(sim_summaries$peak_intensity, prcnt_brks, plot = FALSE, right= FALSE)$counts / n) %>%
    gather(Target, Value, `1 wk ahead`:`Season peak percentage`)


  ## Setup the weekly data_frames
  week_brks <- curr_data %>%
    filter((week >= 40 & year == unique(season)) | week <= 20 & year != unique(season)) %>%
    select(week) %>%
    pull

  ## Need to translate the indices to weeks, so the nudge figures out the difference between the
  ## first week used during the forecasting process, and the first week used for printing (week 40)
  ind_nudge <- rle(curr_data$week < 40 & curr_data$year == unique(curr_data$season))$lengths[1]
  ind_brks <- seq_along(week_brks) + ind_nudge

  ## Now compule the data_frame for the week data
  week_sums <- data_frame(Bin_start_incl = week_brks[-length(week_brks)],
                          Bin_end_notincl = week_brks[-1],
                          Unit = "week") %>%
    mutate(`Season onset` = hist(sim_summaries$start_ind, ind_brks, plot = FALSE, right= FALSE)$counts / n,
           `Season peak week` = hist(sim_summaries$peak_ind, ind_brks, plot = FALSE, right= FALSE)$counts / n) %>%
    gather(Target, Value, `Season onset`:`Season peak week`)


  ## Combine the binned data_frames and get into proper format
  binned_sums <- bind_rows(percent_sums, week_sums) %>%
    mutate(Type = "Bin")

  ## Setup the point estimate data_frame
  point_sums <- sim_summaries %>% summarise_all(mean) %>%
    mutate(`Season onset` = curr_data$week[start_ind],
           `Season peak week` = curr_data$week[peak_ind],
           Type = "Point",
           Bin_start_incl = NA,
           Bin_end_notincl = NA) %>%
    rename(`1 wk ahead` = ahead_1,
           `2 wk ahead` = ahead_2,
           `3 wk ahead` = ahead_3,
           `4 wk ahead` = ahead_4,
           `Season peak percentage` = peak_intensity) %>%
    select(-peak_ind, -start_ind) %>%
    gather(Target, Value, `1 wk ahead`:`Season peak week`) %>%
    mutate(Unit = c(rep("percent", 5), rep("week", 2)))


  ## Combine binned with point data, properly format, and return
  region <- get_location(curr_data)

  bind_rows(point_sums, binned_sums) %>%
    mutate(`Location` = region) %>%
    select(Location, Target, Type, Unit, Bin_start_incl, Bin_end_notincl, Value) %>%
    arrange(Target)
}

get_region <- function(epi_data){
  ## Get the proper location name, using the shorthand
  wili_baselines$location[match(unique(curr_data$region[!is.na(curr_data$region)]), wili_baselines$region)]
}






