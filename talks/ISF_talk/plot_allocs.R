library(covidHubUtils)
library(alloscore)
library(tidyverse)
library(geofacet)

source("_targets.R")

file_names <- paste0("talks/ISF_talk/slim_dfs/slim_", mkeep)
# create an empty list to store the loaded objects
# slim_dfs <- list()
# for(i in seq_along(file_names)){
#   slim_dfs[[mkeep[i]]] <- readRDS(file_names[i])
# }

slim_dfs$`COVIDhub-4_week_ensemble`$model <- "COVIDhub-ensemble"

get_ytot <- function(df) {
  if ("xdf" %in% names(df)) {
    df %>% group_by(reference_date) %>% slice(1) %>%
    transmute(map_vec(xdf, ~ summarise(., ytot = sum(y))))
  } else {
    df %>% group_by(reference_date) %>%
    summarise(ytot = sum(y))
  }
}
add_ytot <- function(df) {
  ytot <- get_ytot(df)
  df %>% left_join(ytot, by = "reference_date")
}

plot_components <- function(...) {
  alloscore::plot_components_slim(
    ...,
    origin_time_col_name = "reference_date",
    model_col_name = "model",
    target_col_name = "abbreviation"
  )
}

K1 <- slim_dfs$`COVIDhub-4_week_ensemble` %>%
  filter(reference_date == forecast_dates[14])

K1_long <- K1 %>% unnest(xdf)

K1_long
K1_long_plus <- K1_long %>% mutate(components = components - min(components))

p1 <- plot_components(K1_long_plus, show_oracle = FALSE, order_at_K = 15800, show_raw = FALSE)

p1 + lims(x=c(1000,25000)) + theme_bw()

df <- bind_rows(slim_dfs$`MUNI-ARIMA`, slim_dfs$`JHUAPL-Bucky`)

p <- plot_components(df %>% filter(K == 15000, reference_date == "2021-12-27") %>% unnest(xdf))




# Create a repeating vector of these colors that matches the number of levels in your fill variable
color_vector <- rep(my_colors, length.out = 51)

p +  scale_fill_manual(values = color_vector)



