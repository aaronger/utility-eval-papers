
library(targets)
adf <- tar_read(alloscore_BPagano.RtDriven_2022.01.03)

tar_load(truth_data)

# copying from run_alloscore.R

require(tidyverse)
require(alloscore)
require(distfromq)

one_model <- mkeep[1]
one_reference_date <- "2021-12-27"

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

  ytot <- sum(forecast_data_processed$value)
  Ks <- seq(2000, 30000, by = 2000)

# Lets plot the distfromq Fs

dist_plot <- forecast_data_processed %>% select(abbreviation, F) %>%
    mutate(x_values = list(seq(0, 1000, length.out = 1000)),
           y_values = map2(F, x_values, function(F, xl) F(xl))) %>%
    unnest(-F) %>%
    ggplot(aes(x = x_values, y = y_values, color = abbreviation)) +
    geom_line()

dist_plot + xlim(c(0, 1000))

  ## run alloscore
  ascores <- forecast_data_processed %>%
    alloscore(
      starting_K = 30000,
      K = 4000,
      y = .[["value"]],
      target_names = "abbreviation"
    )
  ascores <- ascores %>% dplyr::mutate(
    reference_date = one_reference_date,
    model = one_model,
    .before = 1
  )

# make a plot of the xs iterations for one model across multiple dates

iters_bp <- ascores %>% filter(model == "BPagano-RtDriven", K == 4000) %>%
  select(model, reference_date, K, xs) %>% unnest(xs) %>%
  mutate(init_alloc = ntile(`1`, 7), .after = abbreviation)

itnum <- "10"
states_to_keep <- 10
keep_states <- iters_bp %>% filter(reference_date==min(reference_date)) %>%
  filter(min_rank(desc(.data[[itnum]])) < states_to_keep) %>% pull(abbreviation)

plot_iter_dat <- iters_bp %>%
  select(1:.data[[itnum]]) %>%
  select(-`0`) %>%
  mutate(
    States = ifelse(abbreviation %in% keep_states, abbreviation, "other"),
    States = fct_relevel(States, "other", after = Inf),
    abbreviation = fct_reorder(abbreviation, desc(.data[[itnum]])),
    .before = 1
  ) %>%
  tidyr::pivot_longer(
    cols = -c(1:init_alloc),
    names_to = "iteration",
    values_to = "allocation") %>%
  mutate(iteration = as.numeric(iteration))

# Get the default color palette
default_colors <- scales::hue_pal()(length(keep_states))

# Create a named vector of colors
color_palette <- setNames(c(default_colors, "lightgrey"), c(keep_states, "other"))

(p_iter <- plot_iter_dat %>%
  ggplot(aes(x = iteration, y = allocation, group = abbreviation)) +
  #geom_point(aes(color =abbreviation, alpha = 1-.5*is.na(States))) +
  geom_area(aes(group = abbreviation),
            position = "stack", color = "black", size = .2, fill = NA) +
  geom_area(aes(fill = States, alpha = 1-.5*(States == "other")),
            position = "stack") +
  geom_hline(aes(yintercept = K), alpha = .5)+
    facet_grid(cols = vars(reference_date)) +
  geom_vline(aes(xintercept = iteration), alpha = .2) +
  scale_alpha_continuous(guide = "none") +
  scale_x_continuous(breaks = 1:as.integer(itnum)) +
  scale_fill_manual(values = color_palette) + theme_classic())



