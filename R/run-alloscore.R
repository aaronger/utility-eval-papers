run_alloscore <- function(forecast_data, truth_data, one_forecast_date){
  require(tidyverse)
  require(alloscore)
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

  ## process forecast data, adding distfromq output
  forecast_data_processed <- forecast_data |>
    ## forecast dates are different but reference dates are Mondays
    dplyr::filter(reference_date == one_forecast_date) |>
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
  Ks <- seq(from = 500, to = 80000, by = 500)
  Kdf <- data.frame(matrix(Ks,nrow = 1))
  names(Kdf) <- paste0("K=",Ks)

  ascores <- forecast_data_processed  |>
      bind_cols(Kdf) |>
      group_by(model) |>
      summarise(
        ytot = sum(value),
        across(starts_with("K"), ~alloscore:::alloscore(
          y = value,
          F = F,
          Q = Q,
          w = 1,
          K = unique(.x),
          kappa = 1,
          alpha = 1,
          dg = 1,
          eps_K = .01,
          eps_lam = 1e-5,
          verbose = TRUE,
          against_oracle = TRUE
        ))
      )

  ascores_long <- ascores |>
    tidyr::pivot_longer(
      starts_with("K="),
      names_prefix = "K=",
      names_to = "K"
    ) |>
    dplyr::mutate(K = as.numeric(K))

  return(ascores_long)
}



