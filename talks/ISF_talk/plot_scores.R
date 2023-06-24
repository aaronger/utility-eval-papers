library(covidHubUtils)
library(alloscore)
library(tidyverse)
library(geofacet)

source("_targets.R")

score_viz <- tribble(
  ~palette,  ~linetypes, ~model,
  "#004488", 2,          "BPagano-RtDriven",
  "#117733", 4,          "COVIDhub-ensemble",
  "#44AA99", 6,          "CU-select",
  "#88CCEE", 2,          "IHME-CurveFit",
  "#DDCC77", 4,          "JHUAPL-Bucky",
  "#999933", 5,          "JHUAPL-Gecko",
  "#CC6677", 1,          "MUNI-ARIMA",
  "#882255", 1,          "USC-SI_kJalpha",
  "#AA4499", 1,          "UVA-Ensemble",
  "#000000", 1,          "COVIDhub-baseline"
)

score_palette <- deframe(score_viz[c("model", "palette")])
score_linetypes <- deframe(score_viz[c("model", "linetypes")])

peak_day <- bind_rows(slim_dfs) %>% filter(reference_date == "2021-12-27")

three_days <- bind_rows(slim_dfs) %>% filter(reference_date %in% forecast_dates[13:15])

plot_scores_slim(peak_day)
plot_scores_slim(three_days) + lims(x=c(500, 30000))

p_peak <- plot_scores_slim(peak_day)

p_peak + theme_bw()
