## engine to run the alloscores
run_alloscore <- function(
    forecast_data,
    truth_data,
    one_reference_date,
    one_model,
    one_K = NULL){
  require(tidyverse)
  require(alloscore)
  require(distfromq)

  ## process forecast data, adding distfromq output
  forecast_data_processed <- forecast_data |>
    ## forecast dates are different but reference dates are Mondays
    dplyr::filter(reference_date %in% as.Date(one_reference_date)) |>
    dplyr::select(-type) |>
    nest(ps = quantile, qs = value) |>
    relocate(ps, qs) |>
    mutate(
      ps = map(ps, deframe),
      qs = map(qs, deframe)
    ) |>
    dplyr::filter(model == one_model) %>%
    dplyr::mutate(
      model = ifelse(model == "COVIDhub-4_week_ensemble", "COVIDhub-ensemble", model)
    ) |>
    add_pdqr_funs(dist = "distfromq", types = c("p", "q")) |>
    relocate(dist, F, Q) |>
    left_join(
      truth_data |> select(location, target_end_date, value),
      by = c("location", "target_end_date"))

  if (nrow(forecast_data_processed) > 0) {
    if (!is.null(one_K)) {
      Ks <- one_K
    } else {
      ytot <- sum(forecast_data_processed$value)
      Ks <- seq(200, min(60000, 4 * ytot), by = 200)
    }

  ## run alloscore
  ascores <- forecast_data_processed %>%
  alloscore(
    K = Ks,
    y = .[["value"]],
    target_names = "abbreviation",
    slim = TRUE
  )
  ascores <- ascores %>% dplyr::mutate(
    reference_date = one_reference_date,
    model = one_model,
    .before = 1
  )
  } else {
    ascores <- tibble(
      reference_date = one_reference_date,
      model = one_model,
      message = "no forecasts"
    )
  }
  return(ascores)
}


## take estimated alloscores and put them in one clean dataset
## returned object has a row for each model, reference_date, K, state
assemble_alloscores <- function() {
  require(dplyr)

  ## load all alloscore targets and get their names
  tar_load(starts_with("alloscore"))
  ascore_tars <- ls(pattern="alloscore*")

  ## bind all alloscore dataframes together and filter out ones with no forecasts
  alloscores_with_data <- do.call(bind_rows, mget(ascore_tars)) |>
    mutate(target = ascore_tars) |>
    dplyr::filter(is.na(message))

  ## extract alloscore matrices
  alloscore::slim(alloscores_with_data, id_cols = c("model", "reference_date"))
}

