#############################################
## Created by Spencer Fox
## January 16, 2018
## EDM Forecast convenience functions
#############################################


extract_embedding <- function(target_region, epi_week, embeddings){
  embeddings %>% filter(region == target_region, season == season_from_epi_week(epi_week)) %>%
    select(embedding) %>% pull
}

find_embedding <- function(train, test, ts){
  ## Uses simple method to find embedding dimension for time series
  ## Basically finds the first local maxima in the forecast skill and chooses that
  simplex_output <- simplex(ts, train, test)
  embedding <- try(which(diff(simplex_output$rho) < 0)[1])

  ## If there's no local maximum look further in embedding,
  ## and just set it if still can't find
  if(is.na(embedding)){
    simplex_output <- simplex(ts, train, test, E=10:30)
    embedding <- try(which(diff(simplex_output$rho) < 0)[1])
    if(is.na(embedding)){
      embedding=30
    }
  }

  if(class(embedding) == "try-error"){
    write_csv("data_from_embedding_error.csv", x = data_frame(train,test,ts))
    browser()
  }

  embedding
}

get_embedding <- function(region, first_epi_week, test_prop=0.8){
  fluview <- load_historic_fluview(region, first_epi_week)

  ts <- convert_to_forecast_form(fluview, first_epi_week)
  weeks_ahead <- get_weeks_pred(ts)
  n <- length(ts) - weeks_ahead

  ## Setup the train and test.
  train <- c(1, round(n*test_prop))
  test <- c(round(n*test_prop)+1, n+weeks_ahead)


  # get embedding and make predictions --------------------------------------
  embed_dimension <- find_embedding(train, test, ts)
  data_frame(epi_week = first_epi_week, region = region, embedding = embed_dimension)
}
get_all_regions_embedding <- function(first_epi_week, regions, test_prop=0.8){
  regions %>% map(get_embedding, first_epi_week=first_epi_week) %>% bind_rows()
}



make_edm_preds <- function(ts, embed_dimension, train_percent = 0.8) {
  ## ts should be vector containing the time series to predict on
  ##    ts should be numeric vector, with NAs at the end of the vector indicating how many weeks to predict
  ## train_percent should be 0-1 percentage of data to use for training

  #  setup data for prediction ----------------------------------------------
  weeks_ahead <- get_weeks_pred(ts)
  if(weeks_ahead < 4){
    nas_needed <- 4-weeks_ahead
    ts <- c(ts, rep(NA,nas_needed))
    weeks_ahead <- 4
  }
  n <- length(ts) - weeks_ahead
  train <- c(1, round(n*train_percent))

  # Augment data to have NAs that will be filled in with forward predictions
  ts <- c(ts, rep(NA, weeks_ahead))
  test <- c(round(n*train_percent)+1, n+weeks_ahead)


  # Make predictions --------------------------------------
  preds <- simplex(time_series = ts, lib = train, pred = test, E = embed_dimension, tp = 1:weeks_ahead, stats_only = F)
  # preds <- simplex(time_series = ts, lib = c(1,700), pred = c(701, 721), E = embed_dimension, tp = 1:weeks_ahead, stats_only = F)

  # clean the predictions ---------------------------------------------------
  # properly combines the preds into a data_frame with predictions and their variances
  preds %>%
    select(model_output, tp) %>%
    mutate(tp = n+tp) %>%
    as_data_frame %>%
    pmap(~.x[.x$time==.y, ]) %>% ## Extracts the proper row from each prediction data_frame
    bind_rows() %>%
    select(pred, pred_var)
}


get_weeks_pred <- function(ts){
  ## Function gets the number of weeks of prediction from a time series
  ## Figures how many weeks to predict based on trailing NAs in vector
  if(is.na(tail(ts, 1))){
    runs <- rle(is.na(ts))
    tail(runs$lengths,1)
  } else {
    0
  }
}

