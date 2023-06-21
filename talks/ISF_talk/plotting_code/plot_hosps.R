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


plot_one_hosp()
plot_one_hosp(models = c("COVIDhub-4_week_ensemble", "MUNI-ARIMA"), location = "10")

ggtitle("US National Level Hospitalizations - Ensemble Forecast, 14 day horizon")

pdf("nat_hosps.pdf", width = 7, height = 3.5)
print(p_nat)
dev.off()


plot_state_fc <- function(
    models = "COVIDhub-4_week_ensemble",
    start_date = "2021-12-01",
    stop_date = "2022-01-31",
    f_colors = c("mediumblue", "royalblue", "dodgerblue"),
    f_width = 3,
    f_space = 1
    ) {
  fc_dat <- forecasts_hosp %>%
    dplyr::filter(location < 57, model %in% models)
  truth_dat <- truth %>%
    dplyr::filter(location < 57,
                  target_end_date >= start_date,
                  target_end_date <= stop_date) %>%
    dplyr::mutate(code = toupper(geo_value))

  p_state <- ggplot(data = truth_dat) +
  geom_line(
    mapping = aes(x = target_end_date, y = value)
  ) +
  geom_vline(xintercept = as.Date("2021-12-27"), size = 0.5, linetype = 1) +
  geom_vline(xintercept = as.Date("2022-01-10"), size = 0.5, linetype = 2) +
  geom_rect(
    data = get_intervals(fc_dat) %>% dplyr::mutate(code = toupper(geo_value)),
    mapping = aes(
      xmin = target_end_date - f_width1,
      xmax = target_end_date + f_width1,
      ymin = lower,
      ymax = upper,
      fill = interval_level),
    alpha = .5) +
  scale_fill_manual(values = f_colors, name = "Predictive Interval") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  facet_geo(~ code, grid = geofacet::us_state_grid1) +
  # coord_cartesian(xlim = as.Date(c("2021-07-01", "2022-02-01"))) +
  coord_cartesian(xlim = as.Date(c(start_date, stop_date))) +
  # ylim = c(0, 30000)) +
  scale_x_date(breaks = as.Date(c("2021-12-01", "2022-01-01")), date_labels = "%b %y") +
  xlab("Date") +
  ylab("Daily Hospitalizations") +
  #   scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  xlab("Date") +
  ylab("Daily Hospitalizations") +
  theme_bw() +
  theme(panel.spacing = unit(0.1, "lines"))
p_state
}
plot_state_fc(model = "MUNI-ARIMA")

p_state

pdf("state_hosps.pdf", width = 12, height = 8)
print(p_state)
dev.off()



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
