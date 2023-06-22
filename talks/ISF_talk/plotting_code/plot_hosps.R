library(covidHubUtils)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geofacet)

#hub_repo_path <- "~/research/epi/covid/covid19-forecast-hub/"
hub_repo_path <- "~/covid/covid19-forecast-hub/"

inc_hosp_targets <- paste(0:30, "day ahead inc hosp")
forecasts_hosp <- load_forecasts(
  #  models = c("COVIDhub-ensemble", "CMU-TimeSeries"),
  #   dates = "2022-11-08",
  dates = "2021-12-27",
  date_window_size = 6,
  #locations = "US",
  types = c("quantile"),
  targets = inc_hosp_targets,
  source = "local_hub_repo",
  hub_repo_path = hub_repo_path,
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
) %>%
  covidHubUtils::align_forecasts() %>%
  dplyr::filter(relative_horizon == 14)

truth <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp"
)

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

us_ens_fc <- forecasts_hosp %>%
  dplyr::filter(location == "US", model == "COVIDhub-4_week_ensemble")
state_ens_fc <- forecasts_hosp %>%
  dplyr::filter(location < 57, model == "COVIDhub-4_week_ensemble")

forecasts_hosp %>%
  dplyr::filter(location < 57, model == "COVIDhub-4_week_ensemble")


plot_hosp(models = c("CU-select"), f_width1 = 3, locations = "MA")
plot_hosp(models = c("CU-select","COVIDhub-4_week_ensemble", "MUNI-ARIMA"),
          space = 1, f_colors = pal1)

nat_hosps <- plot_hosp(f_width1 = 4) +
  ggtitle("US National Level Hospitalizations - Ensemble Forecast, 14 day horizon")
nat_hosps

p <- plot_hosp(
  start_date = "2021-12-01",
  stop_date = "2022-01-31",
  locations = 1:57,
  models = c("JHUAPL-Bucky","COVIDhub-4_week_ensemble", "MUNI-ARIMA"),
  f_colors = c("mediumblue", "goldenrod", "#CD2626"),
  f_alpha = .4,
  f_width1 = 4,
  space = 1)

p + theme_bw()

pdf("nat_hosps.pdf", width = 7, height = 3.5)
print(p_nat)
dev.off()

p_state <- plot_hosp(
  start_date = "2021-12-01",
  stop_date = "2022-01-31",
  locations = 1:57,
  models = c("JHUAPL-Bucky","COVIDhub-4_week_ensemble", "MUNI-ARIMA"),
  f_colors = c("#4A708B", "goldenrod", "#CD2626"),
  f_alpha = .4,
  f_width1 = 5,
  key_width = .05,
  space = 1) + theme_bw()

ggsave(plot = p_state, filename = "state_hosps.pdf", width = 16, height = 10)


ggplot() +
  geom_line(
    data = truth %>%
      dplyr::filter(location < 57, target_end_date >= "2021-11-01", target_end_date <= "2022-01-17"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  facet_geo(~ abbreviation, grid = geofacet::us_state_grid2)# +
#   scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
#  ylab("Daily Hospitalizations") +
#  theme_bw()
