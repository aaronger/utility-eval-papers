determine_eligible_models <- function(forecast_dates, locations){
  require(covidHubUtils)
  require(tidyverse)

  ## Drop several covidhub ensembles: COVIDhub-4_week_ensemble, COVIDhub_CDC-ensemble (keep trained and COVIDhub-ensemble)
  ## drop CU non-primary models: CU-nochange, CU-scenario_low, CU-scenario_mid
  models_to_drop <- c("COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble",
                      "CU-nochange", "CU-scenario_low", "CU-scenario_mid")

  ## load and filter forecasts
  load_forecasts(
    dates = forecast_dates,
    date_window_size = 6,
    types = c("quantile"),
    ## need to make sure we are getting everyone's relative 14th horizon
    targets = paste(14:20, "day ahead inc hosp"),
    locations = locations,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter(
      !(model %in% models_to_drop),
      relative_horizon == 14
    ) |>
    group_by(model, reference_date) |>
    summarize(nlocs = length(unique(location_name))) |>
    ungroup() |>
    filter(nlocs == length(locations)) |>
    group_by(model) |>
    mutate(ncomplete_weeks = n()) |>
    ## ensure that any model has at least 4 weeks
    ##  excludes 3 teams (UT, PSI, CUB)
    filter(ncomplete_weeks >= 4) |>
    pull(model) |>
    unique()
}
