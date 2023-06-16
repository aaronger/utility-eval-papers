library(targets)
tar_load(forecast_data)
tar_load(truth_data)

## create a group of alloscore targets
values <- tibble(forecast_dates = as.character(seq.Date(as.Date("2021-11-22"), as.Date("2022-02-28"), by = "7 days")))
one_forecast_date <- values$forecast_dates[14]
mkeep <- c("BPagano-RtDriven", "CMU-TimeSeries")

forecast_data_processed <- forecast_data |>
  ## forecast dates are different but reference dates are Mondays
  ## dplyr::filter(reference_date == values$forecast_dates[c(13,14)]) |>
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

test_dat <- forecast_data_processed %>% filter(
  model %in% c("BPagano-RtDriven", "CMU-TimeSeries"),
  reference_date %in% as.Date(c("2022-02-14", "2022-02-21"))
)
Ks <- c(2500, 3000, 3500, 4000, 5000)
ascores <- test_dat %>%
  group_by(model, forecast_date) %>%
  group_modify(~ alloscore(
    .x,
    K = Ks,
    y = .x$value,
    target_names = "abbreviation"
  ))
ascores %>% select(1:score_oracle)
ascores %>% filter(
  model == "BPagano-RtDriven",
  forecast_date == "2022-02-13",
  K == 3500) %>% pull(xdf)


ascores <- forecast_data_processed %>%
  mutate(scored_dfs = map(
    data, ~alloscore(.,
      K = Ks,
      y = .$value,
      target_names = "abbreviation"
    )),
    scores = map(scored_dfs, ~select(., K, score)))



map2_dbl(ascores$Q[1:16], .5, exec)
ascores$Q[[17]](.5)
map2_dbl(ascores$Q[18:51], .5, exec)

nrow(ascores)
ggplot() + map(2:5, function(i) {
  geom_function(fun = ascores$F[[i]], aes(color = ascores$abbreviation[i]), linewidth =2)
  }) +
  map(c(1,6:51), function(i) geom_function(fun = ascores$F[[i]], alpha = .5)) + lims(x=c(0,500)) +
  labs(color = "problem states", y = "distfromq F")


