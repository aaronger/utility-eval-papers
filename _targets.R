# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "covidHubUtils", "distfromq", "alloscore", "gh"), # packages that your targets need to run
  format = "rds" # default storage format
)

## for custom package install
# remotes::install_github("aaronger/alloscore")
# remotes::install_github("reichlab/distfromq")
# remotes::install_github("reichlab/covidHubUtils")

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder:
tar_source()

# List of targets:
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
    command = get_forecast_scores(forecast_data_raw, truth_data)
  ),
  tar_target(
    name = forecast_data_processed,
    command = process_forecast_data(forecast_data_raw, truth_data)
  ),
  tar_target(
    name = alloscores,
    command = run_alloscore(forecast_data_processed)
  ),
  tar_target(
    name = figure_K_v_alloscore,
    command = plot_K_v_alloscore(alloscores),
    format = "file"
  )
)
