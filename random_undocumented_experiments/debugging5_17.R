library(targets)
tar_load(forecast_data)

## create a group of alloscore targets
values <- tibble(forecast_dates = as.character(seq.Date(as.Date("2021-11-22"), as.Date("2022-02-28"), by = "7 days")))
one_forecast_date <- values$forecast_dates[11]

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

ascores <- forecast_data_processed %>% filter(model == "BPagano-RtDriven")
with(ascores[5:50,] ,
      allocate(
  F = F,
  Q = Q,
  w = 1,
  K = 5000,
  kappa = 1,
  alpha = 1,
  dg = 1,
  eps_K = .01,
  eps_lam = 1e-5,
  verbose = TRUE
))

nrow(ascores)
ggplot() + map(2:5, function(i) {
  geom_function(fun = ascores$F[[i]], aes(color = ascores$abbreviation[i]), linewidth =2)
  }) +
  map(c(1,6:51), function(i) geom_function(fun = ascores$F[[i]], alpha = .5)) + lims(x=c(0,500)) +
  labs(color = "problem states", y = "distfromq F")


