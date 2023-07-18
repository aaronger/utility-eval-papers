get_forecast_data <- function(forecast_dates, models, locations){
  require(covidHubUtils)
  load_forecasts(
    dates = forecast_dates,
    date_window_size = 6,
    models = models,
    locations = locations,
    types = c("quantile"),
    targets = paste(14:20, "day ahead inc hosp"),
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter(relative_horizon == 14)
}

get_truth_data <- function(){
  require(covidHubUtils)
  load_truth(
    truth_source = "HealthData",
    target_variable = "inc hosp"
  )
}


get_forecast_scores <- function(forecast_data, truth_data){
  require(covidHubUtils)
  score_forecasts(
    forecasts = forecast_data,
    truth = truth_data,
    use_median_as_point = TRUE
  )
}

filter_problematic_data <- function(forecast_data){
  require(tidyverse)
  forecast_data |>
    ## this removes two state-week-model that has a deterministically zero forecast.
    filter(!(abbreviation=="KS" & reference_date=="2022-02-21" & model=="CU-select")) |>
    filter(!(abbreviation=="WA" & reference_date=="2022-02-28" & model=="CU-select"))
}
