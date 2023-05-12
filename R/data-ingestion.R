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

process_forecast_data <- function(forecast_data, truth_data){
  require(tidyverse)
  require(distfromq)

  # keep selected models
  mkeep <- c("BPagano-RtDriven",
             "COVIDhub-4_week_ensemble",
             "COVIDhub-baseline",
             "CU-select",
             "IHME-CurveFit",
             "JHUAPL-Bucky",
             "JHUAPL-Gecko",
             "MUNI-ARIMA",
             "USC-SI_kJalpha",
             "UVA-Ensemble")

  forecast_data |>
    dplyr::select(-type) |>
    nest(ps = quantile, qs = value) |>
    relocate(ps, qs) |>
    mutate(
      ps = map(ps, deframe),
      qs = map(qs, deframe)
    ) |>
    dplyr::filter(model %in% mkeep) %>%
    dplyr::mutate(
      model = ifelse(model == "COVIDhub-4_week_ensemble", "COVIDhub-ensemble", model)
    ) |>
    add_pdqr_funs(dist = "distfromq", types = c("p", "q")) |>
    relocate(dist, F, Q) |>
    left_join(
      truth_data |> select(location, target_end_date, value),
      by = c("location", "target_end_date"))
}
