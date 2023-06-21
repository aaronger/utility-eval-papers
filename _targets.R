# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(tibble)
library(crew)

# Set target options:
tar_option_set(
  controller = crew_controller_local(workers = 8),
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

## create a group of alloscore targets
values <- tidyr::expand_grid(models = mkeep, forecast_dates = forecast_dates)

forecast_dates_quick <- forecast_dates[c(12,13)]
values_test <- tidyr::expand_grid(forecast_dates = forecast_dates_quick, models = mkeep[1])

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
  # tar_target(
  #   name = alloscore_df,
  #   command = run_alloscore(forecast_data, truth_data, K = Ks,
  #                           mkeep = mkeep, reference_dates = forecast_dates_quick)
  # ),
  tar_map(values = values,
          tar_target(
            alloscore,
            run_alloscore(forecast_data,
                          truth_data,
                          forecast_dates,
                          models)
          ))
  # tar_target(
  #   name = figure_K_v_alloscore,
  #   command = plot_K_v_alloscore(alloscore_df),
  #   format = "file"
  # )
)
