
iters_bp <- adf %>% filter(model == "BPagano-RtDriven", K == 2000) %>%
  select(model, reference_date, xs) %>% unnest(xs) %>%
  mutate(init_alloc = ntile(`1`, 5), .after = abbreviation)

keep_states <- iters_bp %>% filter(reference_date==min(reference_date)) %>%
  filter(min_rank(desc(`1`))<40) %>% pull(abbreviation)

iters_bp %>% select(-qs) %>%
  tidyr::pivot_longer(
    cols = -c(1:4),
    names_to = "iteration",
    values_to = "allocation") %>%
  mutate(
    iteration = as.numeric(iteration)
  ) %>%
  filter(iteration < 8, abbreviation %in% keep_states) %>%
  ggplot(aes(x = iteration, y = allocation, color = abbreviation, alpha = init_alloc)) +
  facet_grid(cols = vars(reference_date)) +
  geom_line() + geom_point() + theme_classic()
