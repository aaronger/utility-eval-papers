library(alloscore)
library(tidyverse)

dgp_params <- tibble(
  model = "dgp",
  target_names = paste0("site", 1:2),
  dist = "norm",
  mean = c(100,25),
  sd = 5)

dgp <- dgp_params %>% add_pdqr_funs()

y_gen <- function() {with(dgp, map2_dbl(r, 1, exec) %>% set_names(target_names))}

modA <- dgp_params %>% mutate(
  model = "A",
  mean = mean[c(2,1)],
  sd = 2,
) %>% add_pdqr_funs(types = c("p","q"))

modB <- dgp_params %>% mutate(
  model = "B",
  mean = .5 * mean,
  sd = 20
) %>% add_pdqr_funs(types = c("p","q"))

modC <- dgp_params %>% mutate(
  model = "C",
  sd = c(3,20),
) %>% add_pdqr_funs(types = c("p","q"))

mods <- bind_rows(dgp, modA, modB, modC)

Ks <- seq(10,500, by = 10)
allos <- mods %>% group_nest(model) %>%
  mutate(allocated = map(data, ~allocate(.x, K= Ks)))
allos <- allos %>% mutate(slim = map(allocated, make_slim))

plot_allos <- allos %>% select(-c(data, allocated)) %>%
  unnest(slim) %>%
  select(-score_fun) %>%
  pivot_wider(values_from = x, names_from = target_names) %>%
  mutate(excess = site1 + site2 - K)

plot_allos %>% ggplot(aes(x=site1, y=site2, color = model)) +
  geom_line() + theme(aspect.ratio = 1)

ys <- map(1:1000, ~y_gen())

allos <- allos %>% mutate(slim_scored = map(slim, ~alloscore(., ys)))

plot_dat <- allos %>%
  mutate(scores = map(slim_scored, ~select(.,-xdf) %>% unnest(scores))) %>%
  select(-c(data, allocated, slim, slim_scored)) %>%
  unnest(scores)

(p <- plot_dat %>% filter(K %in% seq(20, 180, by = 20)) %>% ggplot(aes(x = score, fill = model)) +
    geom_histogram(alpha = 0.5, position = 'identity', bins = 100) +
    facet_wrap(vars(K)))

p + ylim(c(0,120))

data.frame(x = ys %>% map_dbl(sum)) %>% ggplot(aes(x=x)) + geom_density()

tibble(y = ys) %>% unnest_wider(y) %>%
  ggplot(aes(x=site1,y=site2)) + geom_point()



