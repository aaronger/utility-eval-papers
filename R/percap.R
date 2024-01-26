
score_per_capita_allocation <- function(
		all_allscore_data, 
		Kat15k_alloscores) {
  require(tidyverse)

  pops22 <- readr::read_csv("https://raw.githubusercontent.com/reichlab/flusion/main/data-raw/us-census/NST-EST2022-ALLDATA.csv") %>% 
  	select(full_location_name = NAME, POPESTIMATE2021) %>% 
  	inner_join(hub_locations, by = join_by(full_location_name))

	percap <- bind_rows(all_alloscore_data, Kat15k_alloscores) %>% 
	  filter(model == "COVIDhub-baseline") %>% 
	  mutate(model = "percap") %>% 
	  left_join(pops22[c('POPESTIMATE2021', 'abbreviation')], by = "abbreviation") %>% 
	  slice(1, .by = c(reference_date, K ,abbreviation)) %>% 
	  mutate(popprop = POPESTIMATE2021 / sum(POPESTIMATE2021), 
	         .before = x, .by = c(K, reference_date))  %>% 
	  mutate(x = K * popprop, components_raw = pmax(y-x,0), 
	         components = components_raw - components_oracle) %>% 
	  select(-popprop, -POPESTIMATE2021)

	 return(percap)
}