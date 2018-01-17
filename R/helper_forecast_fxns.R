#############################################
## Created by Spencer Fox
## January 16, 2018
## EDM Forecast convenience functions
#############################################


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


make_edm_preds <- function(ts, train_percent = 0.8) {
  ## ts should be vector containing the time series to predict on
  ##    ts should be numeric vector, with NAs at the end of the vector indicating how many weeks to predict
  ## train_percent should be 0-1 percentage of data to use for training

  #  setup data for prediction ----------------------------------------------
  weeks_ahead <- get_weeks_pred(ts)
  n <- length(ts) - weeks_ahead
  train <- c(1, round(n*.8))

  # Augment data to have NAs that will be filled in with forward predictions
  ts <- c(ts, rep(NA, weeks_ahead))
  test <- c(round(n*.8)+1, n+weeks_ahead)


  # get embedding and make predictions --------------------------------------
  embed_dimension <- find_embedding(train, test, ts)
  preds <- simplex(time_series = ts, lib = train, pred = test, E = embed_dimension, tp = 1:weeks_ahead, stats_only = F)

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
    NA
  }
}

