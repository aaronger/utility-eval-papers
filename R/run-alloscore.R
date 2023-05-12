run_alloscore <- function(forecast_data_processed){
  require(alloscore)
  require(tidyverse)

  Ks <- seq(from = 5000, to = 80000, by = 500)
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
