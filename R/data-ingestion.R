get_forecast_data <- function(forecast_dates){
  require(covidHubUtils)
  load_forecasts(
    dates = forecast_dates,
    date_window_size = 6,
    types = c("quantile"),
    targets = paste(0:30, "day ahead inc hosp"),
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter(
      relative_horizon == 14,
      location < 57)
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

