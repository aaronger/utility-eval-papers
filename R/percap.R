
score_per_capita_allocation <- function(dat, pops, Kmax = 60000) {
  require(tidyverse)

	percap <- dat %>%
	  filter(model == "COVIDhub-baseline") %>%
	  select(-K) %>%
	  tidyr::expand_grid(K = make_K_grid(Kmax)) %>%
	  relocate(K, .after = reference_date) %>%
	  slice(1, .by = c(reference_date, K ,abbreviation)) %>%
	  mutate(model = "per-capita") %>%
	  left_join(pops[c('POPESTIMATE2021', 'abbreviation')], by = "abbreviation") %>%
	  group_by(K, reference_date) %>%
	  mutate(popprop = POPESTIMATE2021 / sum(POPESTIMATE2021), .before = x)  %>%
	  mutate(x = K * popprop, components_raw = pmax(y-x,0),
	  	oracle = y * K / sum(y),
	  	components_oracle = pmax(y - oracle, 0),
	  	components = components_raw - components_oracle) %>%
	  ungroup() %>%
	  select(-popprop, -POPESTIMATE2021)

	return(percap)
}
