# FlucastR

This is a repository containing the code for running influenza forecasts using empirical dynamic modeling forecasting method as part of the [flusight](https://github.com/FluSightNetwork/cdc-flusight-ensemble) forecasting group.

# Running the code
To run the forecasts for all weeks, one can simply open up the `init_forecast_test.R` file, and run the code. This takes ~12 hours to run right now, because it's completing retrospective forecasts from 2010-2018. To dig into the functions and code I've tried to elaborate off of the names of the files.


## Main functions to look at and use
1. `init_forecast_test.R` - This file will do everything to forecast 2010-current flu epidemics. It loads in the required packages, data, and functions used in forecasting. It also figures out the seasonal embeddings for each flu region and season. Finally, it identifies all of the necessary forecasts, and calls the main forecasting functions to run.
2. `main_forecast_fxns.R` - This is the next most important script, which holds the main forecasting functions. These functions start with a specific epidemic week, and carry out the task of obtaining and formatting the data, forecasting, and parsing the forecasts into the format for submission.


## Helper functions
1. `helper_data_fxns.R` - Helper functions used to load/read/format the data for forecasting
2. `helper_forecast_fxns.R` - Functions that control the processes that control the forecasting algorithms
3. `helper_pred_parsing_fxns.R` - Functions used to parse the forecasts into the proper format for submitting into the flusightnetwork.


## Analysis functions
1. `analyze_forecasts.R` - This right now is simply used to make sure there weren't errors in the forecasting and exporting processes. Hopefully it will be used as a script to actually analyze how well the forecasts work in the future.


## Useless code
1. `load_baselines.R` -- not implemented or necessary
2. `suts_figure_creations.R` -- Used to make figures for Spencer's recent public talk.


# Contact
Contact [Spencer Fox](mailto:spncrfx@gmail.com) with any questions.
