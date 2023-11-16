# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(tibble)
library(crew)
library(covidHubUtils)
data("hub_locations")

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
tar_source(files = c("R/data-ingestion.R",
                     "R/plot-alloscores.R",
                     "R/run-alloscore.R",
                     "R/determine-model-eligibility.R",
                     "R/exponential-examples.R"))

values <- tibble(forecast_dates = as.character(seq.Date(as.Date("2021-11-22"), as.Date("2022-02-28"), by = "7 days")))

## create a group of alloscore targets
##values <- tidyr::expand_grid(models = mkeep, forecast_dates = forecast_dates)

## set of required locations: all states + DC
reqd_locs <- hub_locations |>
  dplyr::filter(geo_type == "state", !(geo_value %in% c("us", "as", "gu", "mp", "pr", "um", "vi"))) |>
  dplyr::pull(fips)

# Lists of targets:
setup <- list(
  tar_target(
    name = eligible_models,
    command = determine_eligible_models(values$forecast_dates, locations = reqd_locs)
  ),
  tar_target(
    name = forecast_data,
    command = get_forecast_data(values$forecast_dates, models = eligible_models, locations = reqd_locs)
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
    name = exponential_example,
    command = make_exponential_example_figure()
  ),
  tar_target(
    name = Kat15k_alloscores,
    command = run_and_assemble_alloscores(forecast_data,
                                          truth_data,
                                          reference_dates = values$forecast_dates,
                                          one_K = 15000)
  )
)

## from Ben's "overk" analysis
# tar_map(
#   values=values,
#   tar_target(alloscore_overk, run_alloscore_overk(forecast_data, truth_data, forecast_dates))
# )

mapped <- tar_map(
  unlist = FALSE,
  values = values,
  tar_target(
    alloscore,
    run_alloscore_one_date(forecast_data, truth_data, forecast_dates)
    )
)

combined <- tar_combine(
  name = all_alloscore_data,
  mapped[["alloscore"]],
  command = assemble_alloscores(dplyr::bind_rows(!!!.x))
)
# tar_target(
#   name = figure_K_v_alloscore,
#   command = plot_K_v_alloscore(alloscore_df),
#   format = "file"
# )

list(setup, mapped, combined)
