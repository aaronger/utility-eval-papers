# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(tibble)
library(crew)

# Set target options:
tar_option_set(
  controller = crew_controller_local(workers = 4),
  packages = c("tidyverse", "covidHubUtils", "distfromq", "alloscore", "gh"), # packages that your targets need to run
  imports = c("alloscore"),
  format = "rds" # default storage format
)

## for custom package install
# remotes::install_github("aaronger/alloscore")
# remotes::install_github("reichlab/distfromq")
# remotes::install_github("reichlab/covidHubUtils")
# Run the R scripts in the R/ folder:
tar_source(files = c("R/data-ingestion.R", "R/plot-alloscores.R", "R/run-alloscore.R"))

mkeep <- c("BPagano-RtDriven",
           "COVIDhub-4_week_ensemble",
           "COVIDhub-baseline",
           "CU-select",
           "IHME-CurveFit",
           "JHUAPL-Bucky",
           "JHUAPL-Gecko",
           "MUNI-ARIMA",
           "USC-SI_kJalpha",
           "UVA-Ensemble")

forecast_dates = as.character(seq.Date(as.Date("2021-10-18"), as.Date("2022-02-28"), by = "7 days"))

forecast_dates_quick <- forecast_dates[c(12,13)]

## create a group of alloscore targets
values_test <- tidyr::expand_grid(forecast_dates = forecast_dates_quick, models = mkeep[1])
values <- tidyr::expand_grid(models = mkeep, forecast_dates = forecast_dates)

# hard coding the names from tar_map
# this seems like it should be its own target but I can't figure this out right now
a_target_names <- values %>% dplyr::mutate(
  target_name = paste0("alloscore_",
                       gsub(x = models, pattern = "-", replacement = "."),
                       "_",
                       gsub(x = forecast_dates, pattern = "-", replacement = ".")
  )) %>% dplyr::select(-forecast_dates)

a_target_names_nested <- a_target_names %>% tidyr::nest(target_list = target_name)

# function to create a single slim df for each model's alloscore time series
# Don't use this! Will blow up your computer.
# slim_dfs <- function(target_list) {
#   target_list <- target_list %>% unlist(use.names = F)
#   targets::tar_load(target_list)
#   purrr::map(target_list, get) %>%
#     purrr::map(purrr::possibly(~ alloscore::slim(., id_cols = c(
#       "reference_date", "model"
#     )))) %>%
#     dplyr::bind_rows()
#   rm(list = target_list)
# }

slim_df <- function(adf) {
  require(tidyverse)
  require(alloscore)
  alloscore::slim(adf, id_cols = c("reference_date", "model"))
}

# List of targets:
list(
  tar_target(
    name = forecast_data,
    command = get_forecast_data(forecast_dates)
  ),
  # tar_target(
  #   name = filtered_forecast_data,
  #   command = filter_problematic_data(forecast_data)
  # ),
  tar_target(
    name = truth_data,
    command = get_truth_data()
  ),
  tar_target(
    name = score_data,
    command = get_forecast_scores(forecast_data, truth_data)
  ),
  tar_map(values = values,
          tar_target(
            alloscore,
            run_alloscore(forecast_data,
                          truth_data,
                          forecast_dates,
                          models)
          )
  # tar_map(
  #   values = a_target_names,
  #   names = target_name,
  #   tar_target(slim, slim_df(target_name))
  )
  # tar_target(
  #   name = figure_K_v_alloscore,
  #   command = plot_K_v_alloscore(alloscore_df),
  #   format = "file"
  # )
)
