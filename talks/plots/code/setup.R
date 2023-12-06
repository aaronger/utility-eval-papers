library(covidHubUtils)
library(tidyverse)
require(alloscore)
require(distfromq)
library(geofacet)
library(scales)
library(latex2exp)
library(targets)

here::i_am("talks/plots/code/setup.R")
library(here)
plotpath <- here("talks","plots")
codepath <- here(plotpath,"code")
datapath <- here(plotpath,"data")
outputpath <- here(plotpath,"output")
source(here(codepath,"plot_functions.R"))

save_dir <- outputpath
#hub_repo_path <- "~/research/epi/covid/covid19-forecast-hub/"
hub_repo_path <- "~/covid/covid19-forecast-hub/"

source("_targets.R")
tar_load(truth_data)
tar_load(forecast_data)
tar_load(all_alloscore_data)

forecasts_hosp <- slim_dfs <- filter(all_alloscore_data, reference_date == "2021-12-27")

# forecasts_hosp <- forecast_data %>%
#   bind_rows(
#     load_forecasts(
#       dates = "2021-12-27",
#       date_window_size = 6,
#       locations = "US",
#       types = c("quantile"),
#       targets = paste(0:30, "day ahead inc hosp"),
#       source = "local_hub_repo",
#       hub_repo_path = hub_repo_path,
#       verbose = FALSE,
#       as_of = NULL,
#       hub = c("US")) |>
#       align_forecasts() |>
#       dplyr::filter(
#         relative_horizon == 14)
#   ) %>%
#   filter(reference_date == "2021-12-27")
truth <- truth_data

mkeep <- c("BPagano-RtDriven",
           "COVIDhub-ensemble",
           "COVIDhub-baseline",
           "CU-select",
           "IHME-CurveFit",
           "JHUAPL-Bucky",
           "JHUAPL-Gecko",
           "MUNI-ARIMA",
           "USC-SI_kJalpha",
           "UVA-Ensemble")

three_colors <- c("#4A708B", "goldenrod", "#CD2626")
