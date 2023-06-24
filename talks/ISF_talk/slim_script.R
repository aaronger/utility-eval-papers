
library(tidyverse)
library(alloscore)
library(targets)

args <- commandArgs(trailingOnly = TRUE)
j <- as.integer(args[1])
print(j)


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

slim_dfs <- function(i) {
  target_list <- purrr::map(a_target_names_nested$target_list[[i]][[1]], ~tar_read_raw(.))

  saveRDS(target_list %>%
    purrr::map(possibly(~ alloscore::slim(., id_cols = c("reference_date", "model")))) %>%
    bind_rows(), file = paste0("talks/ISF_talk/slim_dfs/slim_", a_target_names_nested$models[i]))

  rm(target_list)
}

# Get the names of all objects in the global environment
#
# for (i in 6:10) {
#   slim_dfs(i)
# }

slim_dfs(j)


