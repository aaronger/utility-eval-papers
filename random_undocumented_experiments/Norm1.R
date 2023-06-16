library(alloscore)
library(tidyverse)
library(scoringRules)
library(patchwork)
library(ggridges)
source("random_undocumented_experiments/scoring_helpers.R")

dgp_params <- tibble(
  model = "dgp",
  target_names = paste0("site", 1:2),
  dist = "norm",
  mean = c(200, 100),
  sd = 50)

dgp <- dgp_params %>%
  add_pdqr_funs() %>%
  add_crps_funs()

y_gen <- function() {with(dgp, map2_dbl(r, 1, exec) %>% set_names(target_names))}

modA <- dgp_params %>% mutate(
  model = "A",
  mean = mean[c(2,1)],
  sd = 10) %>%
  add_pdqr_funs() %>%
  add_crps_funs()

modB <- dgp_params %>% mutate(
  model = "B",
  mean = .5 * mean,
  sd = 20) %>%
  add_pdqr_funs() %>%
  add_crps_funs()

modC <- dgp_params %>% mutate(
  model = "C",
  sd = c(3,20)) %>%
  add_pdqr_funs() %>%
  add_crps_funs()

mods <- bind_rows(dgp, modA, modB, modC)

# select(mods, model, target_names, f) %>%
#   pivot_wider(values_from = f, names_from = c(model, target_names))

ys <- tibble(samp = 1:200, ys = map(seq_along(samp), ~y_gen()))
ytot = map_vec(ys$ys, sum)
p_y_tot <- data.frame(x = ytot) %>% ggplot(aes(x=x)) +
  geom_histogram() + labs(x = "ytot")
p_y_dens <- ys %>% unnest_wider(ys) %>% ggplot(aes(x=site1,y=site2)) + geom_point()
(y_plot <- p_y_tot / p_y_dens)

K_lims <- quantile(ytot, probs = c(.1, .9)) %>% round(-1)
(Ks <- seq(K_lims[1], K_lims[2], length.out = 5))
allos <- mods %>% group_nest(model) %>%
  mutate(allocated = map(data, ~allocate(.x, K= Ks)))
allos <- allos %>% mutate(slim = map(allocated, make_slim))

allos <- allos %>% mutate(slim_scored = map(slim, ~alloscore(., ys$ys)))

plot_allos <- allos %>% select(-c(data, allocated)) %>%
  unnest(slim) %>%
  select(-score_fun) %>%
  pivot_wider(values_from = x, names_from = target_names) %>%
  mutate(excess = site1 + site2 - K)

# This is weird
plot_allos %>% ggplot(aes(x=site1, y=site2, color = model)) +
  geom_line() + theme(aspect.ratio = 1)

mods_crps <- mods %>%
  #filter(model == "B") %>%
  select(model, crps) %>%
  nest(crps_fns = crps) %>%
  mutate(mean_crps = map(crps_fns, ~mean_crps_samp(crps_fns = .[[1]], y_vec_samp = ys$ys))) %>%
  select(model, mean_crps) %>% unnest(mean_crps)

mods_crps %>% pivot_wider(values_from = mean_crps, names_from = model)
mods_crps %>% ggplot(aes(x = mean_crps, color = model)) + geom_histogram()

plot_dat <- allos %>%
  mutate(scores = map(slim_scored, ~select(.,-xdf) %>% unnest(scores))) %>%
  select(-c(data, allocated, slim, slim_scored)) %>%
  unnest(scores) %>%
  mutate(K = factor(K, levels = sort(unique(K)))) %>%
  bind_rows(
    mods_crps %>%
      mutate(K = as.factor("mean CRPS"), .after = samp, samp = as.character(samp)) %>%
      rename(score = mean_crps)) %>%
  mutate(K = fct_relevel(K, "mean CRPS"), model = fct_rev(model))


#Ks_to_show <- as.character(seq(150, 130, by = 20))
Ks_to_show <- as.character(Ks)
(p <- plot_dat %>% filter(K %in% c("mean CRPS", Ks_to_show), model != "dgp") %>%
    ggplot() +
    geom_density(aes(x = score, fill = model), color = NA, alpha = .5) +
    facet_wrap(vars(K)) + theme_bw() + ylim(c(0,.15)))

p + geom_density(data = plot_dat %>% filter(K %in% c("mean CRPS", Ks_to_show), model == "dgp"),
                 aes(x = score), fill = NA)

p_ridges <- plot_dat %>% filter(K %in% c("mean CRPS", Ks_to_show)) %>% mutate(K = fct_rev(K)) %>%
  ggplot(aes(x = score, y = model, fill = model)) + geom_density_ridges(color = NA) +
  facet_grid(rows = vars(K)) +
    labs(y = "Density of scores") +
    guides(fill = guide_legend(reverse = TRUE)) +
   scale_alpha_discrete(guide = FALSE)
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text.y = element_text(angle = 0))

(p_y_tot / p_y_dens) | p_ridges



