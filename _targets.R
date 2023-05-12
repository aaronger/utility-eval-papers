# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "covidHubUtils", "distfromq", "alloscore", "gh"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

## for custom package install
# remotes::install_github("aaronger/alloscore")
# remotes::install_github("reichlab/distfromq")
# remotes::install_github("reichlab/covidHubUtils")

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(
    name = forecast_data_raw,
    command = get_forecast_data("2021-12-27")
  ),
  tar_target(
    name = truth_data,
    command = get_truth_data()
  ),
  tar_target(
    name = score_data,
    command = get_forecast_scores(forecast_data, truth_data)
  ),
  tar_target(
    name = forecast_data_processed,
    command = process_forecast_data(forecast_data, truth_data)
  ),
  tar_target(
    name = run_alloscore,
    command = run_alloscore(forecast_data)
  )
)
