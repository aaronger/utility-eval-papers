
library(targets)
tar_load(alloscore_df)
adf <- alloscore_df

iters_bp <- adf %>% filter(model == "BPagano-RtDriven", K == 2000) %>%
  select(model, reference_date, xs) %>% unnest(xs) %>%
  mutate(init_alloc = ntile(`1`, 5), .after = abbreviation)

keep_states <- iters_bp %>% filter(reference_date==min(reference_date)) %>%
  filter(min_rank(desc(`1`))<5) %>% pull(abbreviation)

plot_iter_dat <- iters_bp %>% select(-qs) %>%
  tidyr::pivot_longer(
    cols = -c(1:4),
    names_to = "iteration",
    values_to = "allocation") %>%
  mutate(iteration = as.numeric(iteration)) %>%
  mutate(give_color = ifelse(abbreviation %in% keep_states, abbreviation, NA))

(p_iter <- plot_iter_dat %>%
  ggplot(aes(x = iteration, y = allocation, color = give_color, alpha = 1-.5*is.na(give_color))) +
  facet_grid(cols = vars(reference_date)) + geom_point() +
  geom_line() + theme_classic() +
  scale_alpha_continuous(guide = "none"))
