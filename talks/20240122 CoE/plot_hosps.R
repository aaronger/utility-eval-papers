library(covidHubUtils)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geofacet)

hub_repo_path <- "~/research/epi/covid/covid19-forecast-hub/"

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

us_ens_fc <- forecasts_hosp %>%
    dplyr::filter(location == "US", model == "COVIDhub-4_week_ensemble")
state_ens_fc <- forecasts_hosp %>%
    dplyr::filter(location < 57, model == "COVIDhub-4_week_ensemble")
    

p_nat <- ggplot(data = truth %>% filter(location == "US",
                                        target_end_date >= "2021-07-01",
                                        target_end_date <= "2022-01-31")) +
    geom_line(mapping = aes(x = target_end_date, y = value)) +
    geom_vline(
        data = data.frame(
            tedate = as.Date(c("2021-12-27", "2022-01-10")),
            Date = c("Forecast Date", "Target Date")
        ),
        mapping = aes(xintercept = tedate, linetype = Date),
        linewidth = 0.5) +
    geom_point(
        data = us_ens_fc %>% filter(quantile == 0.5),
        mapping = aes(x = target_end_date, y = value),
        color = "orange") +
    geom_errorbar(
        data = us_ens_fc %>%
            dplyr::filter(quantile %in% c(0.025, 0.1, 0.25, 0.75, 0.9, 0.975)) %>%
            dplyr::mutate(
                lu = ifelse(quantile < 0.5, "lower", "upper"),
                interval_level_num = ifelse(quantile < 0.5,
                                            100 * (1 - 2 * quantile),
                                            100 * (1 - 2 * (1 - quantile))),
                interval_level = paste0(as.integer(interval_level_num), "%")
            ) %>%
            dplyr::select(target_end_date, value, lu, interval_level) %>%
            tidyr::pivot_wider(names_from = "lu", values_from = "value"),
        mapping = aes(x = target_end_date, ymin = lower, ymax = upper, width = as.numeric(factor(interval_level)) * 3),
        color = "orange") +
    scale_x_date(date_labels = "%b %y") +
    coord_cartesian(xlim = as.Date(c("2021-07-01", "2022-02-01")),
                    ylim = c(0, 30000)) +
    xlab("Date") +
    ylab("Daily Hospitalizations") +
    ggtitle("US National Level Hospitalizations - Ensemble Forecast, 14 day horizon") +
    theme_bw()
p_nat


pdf("nat_hosps.pdf", width = 7, height = 3.5)
print(p_nat)
dev.off()



p_state <- ggplot(
    data = truth %>%
        dplyr::filter(location < 57,
                        # target_end_date >= "2021-07-01",
                        # target_end_date >= "2021-11-01",
                        target_end_date >= "2021-10-15",
                        target_end_date <= "2022-01-31") %>%
        dplyr::mutate(code = toupper(geo_value))) +
    geom_line(
        mapping = aes(x = target_end_date, y = value)
    ) +
    geom_vline(xintercept = as.Date("2021-12-27"), size = 0.5, linetype = 1) +
    geom_vline(xintercept = as.Date("2022-01-10"), size = 0.5, linetype = 2) +
    facet_geo(~ code, grid = geofacet::us_state_grid1) +
    # coord_cartesian(xlim = as.Date(c("2021-07-01", "2022-02-01"))) +
    coord_cartesian(xlim = as.Date(c("2021-10-15", "2022-02-01"))) +
                    # ylim = c(0, 30000)) +
    scale_x_date(breaks = as.Date(c("2021-11-01", "2022-01-01")), date_labels = "%b %y") +
    xlab("Date") +
    ylab("Daily Hospitalizations") +
#   scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
    xlab("Date") +
    ylab("Daily Hospitalizations") +
    theme_bw() +
    theme(panel.spacing = unit(0.1, "lines"))
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
