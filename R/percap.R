
score_per_capita_allocation <- function(dat, pops, Kmax = 60000) {
  require(tidyverse)

	percap <- dat %>% 
	  filter(model == "COVIDhub-baseline") %>% 
	  select(-K) %>% 
	  tidyr::expand_grid(K = make_K_grid(Kmax)) %>% 
	  relocate(K, .after = reference_date) %>%
	  slice(1, .by = c(reference_date, K ,abbreviation)) %>% 
	  mutate(model = "percap") %>% 
	  left_join(pops[c('POPESTIMATE2021', 'abbreviation')], by = "abbreviation") %>% 
	  mutate(popprop = POPESTIMATE2021 / sum(POPESTIMATE2021), 
	         .before = x, .by = c(K, reference_date))  %>% 
	  mutate(x = K * popprop, components_raw = pmax(y-x,0), 
	         components = components_raw - components_oracle) %>% 
	  select(-popprop, -POPESTIMATE2021)

	return(percap)
}