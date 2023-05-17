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
  format = "rds" # default storage format
)

## for custom package install
# remotes::install_github("aaronger/alloscore")
# remotes::install_github("reichlab/distfromq")
# remotes::install_github("reichlab/covidHubUtils")

# Run the R scripts in the R/ folder:
tar_source()

## create a group of alloscore targets
values <- tibble(forecast_dates = as.character(seq.Date(as.Date("2021-12-13"), as.Date("2021-12-27"), by = "7 days")))

# List of targets:
list(
  tar_target(
    name = forecast_data,
    command = get_forecast_data(values$forecast_dates)
  ),
  tar_target(
    name = truth_data,
    command = get_truth_data()
  ),
  tar_target(
    name = score_data,
    command = get_forecast_scores(forecast_data, truth_data)
  ),
  tar_map(
    values = values,
    tar_target(alloscore, run_alloscore(forecast_data, truth_data, forecast_dates))
  ),
  tar_target(
    name = figure_K_v_alloscore_2021.12.13,
    command = plot_K_v_alloscore(alloscore_2021.12.13),
    format = "file"
  )


)
