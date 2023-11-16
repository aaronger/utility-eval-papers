library(covidHubUtils)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geofacet)

source("plot_functions.R")
save_dir <- file.path("talks/plots/output/")

#hub_repo_path <- "~/research/epi/covid/covid19-forecast-hub/"
hub_repo_path <- "~/covid/covid19-forecast-hub/"

forecasts_hosp <- targets::tar_read(forecast_data) %>%
  bind_rows(
    load_forecasts(
      dates = "2021-12-27",
      date_window_size = 6,
      locations = "US",
      types = c("quantile"),
      targets = paste(0:30, "day ahead inc hosp"),
      source = "local_hub_repo",
      hub_repo_path = hub_repo_path,
      verbose = FALSE,
      as_of = NULL,
      hub = c("US")) |>
      align_forecasts() |>
      dplyr::filter(
        relative_horizon == 14)
  ) %>%
  filter(reference_date == "2021-12-27")
truth <- targets::tar_read(truth_data)

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

p_VA_JHU_MUNI <- plot_hosp(start_date = "2021-11-01", 
                           stop_date = "2022-01-15",
                           models = c("JHUAPL-Bucky","COVIDhub-ensemble", "MUNI-ARIMA"),
                           f_width1 = 3, f_alpha = .4, locations = c("VA", "GA"))

p_2Ls_CU_IHME <- plot_hosp(models = c("CU-select","COVIDhub-ensemble", "MUNI-ARIMA"),
                          f_width1 = 3, f_alpha = .4, locations = c("FL", "NY"))

sc = 1
ggsave(plot = p_VA_CU_IHME, filename = "p_VA_CU_IHME.png",
       width = 11*sc, height = 6*sc, path = save_dir)
ggsave(plot = p_VA_JHU_MUNI, filename = "p_VA_JHU_MUNI.svg",
       width = 8*sc, height = 4*sc, path = save_dir, device = "svg")


p_VA_CU_IHME
### plot with allocations
plot_hosp(models = c("CU-select", "UVA-Ensemble"), f_width1 = 4, f_alpha = .4, locations = "MA",
          allocations = TRUE, f_colors = c("blue", "red"))

plot_hosp(models = c("CU-select","COVIDhub-4_week_ensemble", "MUNI-ARIMA"),locations = c("CA", "MA"),
          space = 1, f_colors = pal1)


# First plot for application section

{
nat_hosps <- plot_hosp(start_date = "2021-07-01", key_width = .08,
                       f_width1 = 6, locations = "US", f_colors = "black", f_alpha = .3) +
  guides(fill = "none") + labs(x = NULL) +
  ggtitle("US National Level Hospitalizations \nEnsemble Forecast, 14 day horizon") +
    theme(plot.title = element_text(vjust = - 10, hjust = .25))
sc <- .7
ggsave(plot = nat_hosps, filename = "nat_hosps.png",
       width = 11*sc, height = 6*sc, path = save_dir)
}

p_ens_only <- p <- plot_hosp(
  start_date = "2021-11-15",
  stop_date = "2022-01-31",
  locations = 1:57,
  models = c("COVIDhub-4_week_ensemble"),
  f_colors = c("darkgrey"),
  f_alpha = .4,
  f_width1 = 10,
  space = 1,
  key_width = .005,
  geofacet = TRUE)

p_ens_only <- p_ens_only + theme_bw() + guides(fill="none") + labs(x=NULL) +
  theme(legend.position = c(.93, 0.28)) +
  ggtitle("State Level Hospitalizations, \nEnsemble Forecast, 14 day horizon") +
  theme(plot.title = element_text(vjust = - 10, hjust = .25, size = 16),
        axis.text.x = element_text(size = 8))

ar = 3/2
h = 9
w = ar*h
ggsave(plot = p_ens_only, filename = "state_hosps_ens_only.jpeg",
       width = w, height = h, path = save_dir, device = "jpeg", dpi = 600)

  p <- plot_hosp(
  start_date = "2021-12-01",
  stop_date = "2022-01-31",
  locations = 1:57,
  models = c("JHUAPL-Bucky","COVIDhub-4_week_ensemble", "MUNI-ARIMA"),
  f_colors = c("mediumblue", "goldenrod", "#CD2626"),
  f_alpha = .4,
  f_width1 = 4,
  space = 1)

p2 <- p <- plot_hosp(
  start_date = "2021-12-01",
  stop_date = "2022-01-31",
  locations = c("CA", "IL", "FL", "TX"),
  models = c("JHUAPL-Bucky","COVIDhub-4_week_ensemble", "MUNI-ARIMA"),
  f_colors = c("mediumblue", "goldenrod", "#CD2626"),
  f_alpha = .4,
  f_width1 = 2,
  space = .5)

p2 + theme_bw()

p_state <- plot_hosp(
  start_date = "2021-12-01",
  stop_date = "2022-01-31",
  locations = 1:57,
  models = c("JHUAPL-Bucky","COVIDhub-4_week_ensemble", "MUNI-ARIMA"),
  f_colors = c("#4A708B", "goldenrod", "#CD2626"),
  f_alpha = .4,
  f_width1 = 5,
  geofacet = TRUE,
  key_width = .05,
  space = 1) + theme_bw()

ggsave(plot = p_state, filename = "state_hosps.pdf",
       width = 16, height = 10, path = save_dir)



### plot for muni-vs-bucky

mvbucky_dist_alloc <- plot_hosp(models = c("JHUAPL-Bucky", "MUNI-ARIMA"),
          start_date = "2021-12-20",
          locations = c("FL", "CA", "TX", "GA", "NJ", "NC", "PA", "VA", "IL"),
          f_width1 = 4, f_alpha = .4,
          allocations = TRUE, f_colors = c("#4A708B", "#CD2626")) + labs(x = NULL)

ggsave(plot = mvbucky_dist_alloc, filename = "mvbucky_dist_alloc.png",
       width = 10, height = 6, path = save_dir)



