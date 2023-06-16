run_alloscore <- function(forecast_data, truth_data, Ks, mkeep, reference_dates){
  require(tidyverse)
  require(alloscore)
  require(distfromq)

  ## process forecast data, adding distfromq output
  forecast_data_processed <- forecast_data |>
    ## forecast dates are different but reference dates are Mondays
    dplyr::filter(reference_date %in% reference_dates) |>
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

  ## run alloscore
  ascores <- forecast_data_processed %>%
  group_by(model, reference_date) %>%
  group_modify(~ alloscore(
    .x,
    K = Ks,
    y = .x$value,
    target_names = "abbreviation"
  ))

  return(ascores)
}



