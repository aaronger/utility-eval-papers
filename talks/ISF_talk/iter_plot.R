library(scales)
library(targets)
adf <- tar_read(alloscore_BPagano.RtDriven_2022.01.03)

tar_load(truth_data)
tar_load(forecast_data)

# copying from run_alloscore.R

require(tidyverse)
require(alloscore)
require(distfromq)

one_model <- mkeep[4]
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

dist_plot_dat <- forecast_data_processed %>% select(abbreviation, F) %>%
  mutate(x_values = list(c(seq(0, 3, length.out = 300), seq(3, 1500, length.out = 1000))),
           y_values = map2(F, x_values, function(F, xl) F(xl))) %>%
  select(-F) %>% unnest(-abbreviation)

dist_plot <- dist_plot_dat %>%
  ggplot(aes(x = x_values, y = y_values, color = abbreviation)) + geom_line()

dist_plot + xlim(c(0, 1000))

# with quantiles for a subset of states

make_dist_pqs_plot <- function(states) {
pqs <- forecast_data_processed %>% filter(abbreviation %in% states) %>%
  select(ps, qs, abbreviation, value) %>% unnest(cols = c(ps, qs))

dist_pqs_plot <- pqs %>% ggplot() +
  geom_rect(
    xmin = pqs$qs[pqs$ps == .025],
    xmax = pqs$qs[pqs$ps == .975],
    ymin = -Inf,
    ymax = .1,
    fill = "grey",
    alpha = .01
  ) +
  geom_rect(
    xmin = pqs$qs[pqs$ps == .1],
    xmax = pqs$qs[pqs$ps == .9],
    ymin = -Inf,
    ymax = .1,
    fill = "grey",
    alpha = .02
  ) +
  geom_rect(
    xmin = pqs$qs[pqs$ps == .25],
    xmax = pqs$qs[pqs$ps == .75],
    ymin = -Inf,
    ymax = .1,
    fill = "grey",
    alpha = .07
  ) +
  geom_line(data = dist_plot_dat %>% filter(abbreviation %in% states),
            aes(x = x_values, y = y_values, color = abbreviation)) +
  geom_segment(aes(x = qs, xend = qs, y = 0, yend = ps),
               color = "black", linewidth = .1) +
  geom_segment(aes(x = min(pqs$qs)*.9, xend = qs, y = ps, yend = ps),
               color = "black", linewidth = .1) +
  geom_point(aes(x = qs, y = ps)) +
  scale_x_continuous(
    expand = c(0,0),
    breaks = pqs$qs,
    limits = c(min(pqs$qs)*.9, max(pqs$qs)*1.1),
    labels = number_format(accuracy = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0,.1)), breaks = c(.025, .1, .25, .5, .75, .9, .975),
                     labels = c(".025", ".1", ".25", ".5", ".75", ".9", ".975")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  labs(x = "Forecast Quantiles", y = "Probability Levels")
dist_pqs_plot
}

dist_pqs_plot_OH <- make_dist_pqs_plot("OH") + theme(legend.position = "none")
dist_pqs_plot_VT <- make_dist_pqs_plot("") + theme(legend.position = "none")
dist_pqs_plot_VT
sc = 1.4
ggsave(plot = dist_pqs_plot_OH, filename = "dist_pqs_plot_OH.png",
       width = 7*sc, height = 3*sc, path = save_dir)

  ## run alloscore
one_K = 10000

  ascores <- forecast_data_processed %>%
    alloscore(
      starting_K = 60000,
      K = one_K,
      y = .[["value"]],
      target_names = "abbreviation"
    )
  ascores <- ascores %>% dplyr::mutate(
    reference_date = one_reference_date,
    model = one_model,
    .before = 1
  )

# make a plot of the xs iterations for one model across multiple dates

iters_bp <- ascores %>% filter(model == one_model, K == one_K) %>%
  select(model, reference_date, K, xs) %>% unnest(xs) %>%
  mutate(init_alloc = ntile(`1`, 7), .after = abbreviation)

itnum <- "14"
states_to_keep <- 6
keep_states <- iters_bp %>% filter(reference_date==min(reference_date)) %>%
  filter(min_rank(desc(.data[[itnum]])) <= states_to_keep) %>% pull(abbreviation)

plot_iter_dat <- iters_bp %>%
  select(1:.data[[itnum]]) %>%
  select(-`0`) %>%
  mutate(
    allo_rank = min_rank(desc(.data[[itnum]])), .before = 1) %>%
  arrange(allo_rank) %>%
  mutate(
    States = ifelse(allo_rank <= states_to_keep, abbreviation, "other"),
    States = fct_inorder(States),
    abbreviation = fct_inorder(abbreviation),
    .before = 1) %>%
  # mutate(
  #   States = fct_inorder(States),
  # ) %>%
  tidyr::pivot_longer(
    cols = -c(1:init_alloc),
    names_to = "iteration",
    values_to = "allocation") %>%
  mutate(iteration = as.numeric(iteration))

State_nms <- unique(plot_iter_dat$States)
l <- length(State_nms)
# Get the default color palette
default_colors <- scales::hue_pal()(l)[sample(1:l, l)]

# Create a named vector of colors
color_palette <- setNames(default_colors, State_nms)
color_palette["other"] <- "lightgrey"

p_iter <- plot_iter_dat %>%
  ggplot(aes(x = iteration, y = allocation, group = abbreviation)) +
  #geom_point(aes(color =abbreviation, alpha = 1-.5*is.na(States))) +
  geom_area(aes(group = abbreviation),
            position = "stack", color = "black", size = .3, fill = NA) +
  geom_area(aes(fill = States, alpha = 1-.5*(States == "other")),
            position = "stack") +
  geom_hline(aes(yintercept = K), alpha = .5)+
    facet_grid(cols = vars(reference_date)) +
  geom_vline(aes(xintercept = iteration), alpha = .2) +
  scale_alpha_continuous(guide = "none") +
  geom_text(aes(x = as.integer(itnum), y = K, label = paste("constraint =", K)),
            hjust = 1, vjust = -1, size = 3) +
  scale_x_continuous(breaks = 1:as.integer(itnum), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = color_palette) + theme_classic() +
  labs(y = "Allocations")

sc = .85
ggsave(plot = p_iter, filename = "p_iter.png",
         width = 7*sc, height = 5.5*sc, path = save_dir)

### plot for iteration illustration

iter_dist_illus <- plot_hosp(models = one_model,
                             start_date = "2021-12-20",
                             stop_date = "2022-01-20",
                             locations = keep_states,
                             f_width1 = 8, f_alpha = .5,
                             st_colors = color_palette[1:(states_to_keep)],
                             allocations = FALSE, f_colors = c("#4A708B")) +
  labs(x = NULL) + theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + facet_grid(cols = vars(code))
iter_dist_illus

{sc = .9;
ggsave(plot = iter_dist_illus, filename = "iter_dist_illus.png",
       width = sc*7, height = sc*5, path = save_dir, dpi = 320)
  }

iter_combo <- iter_dist_illus + p_iter

sc_iter_combo = 1.1
ggsave(plot = iter_combo, filename = "iter_combo.png",
       width = sc_iter_combo*8, height = sc_iter_combo*3.5, path = save_dir, dpi = 320)

