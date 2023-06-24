library(targets)
library(tidyverse)
library(alloscore)
library(distfromq)
library(lobstr)
source("_targets.R")

tar_load(names = c("forecast_data", "truth_data"))
tar_load(starts_with("alloscore_BPagano.RtDriven"))


.mkeep <- sub(x = mkeep, pattern = "-", replacement = ".")

tar_names <- tar_manifest(fields = name)

BPagano.RtDriven <- mget(grep(paste0("alloscore_", .mkeep[1]), tar_names$name, value = TRUE)) %>% 
  map(possibly(~slim(., id_cols = c("reference_date", "model")))) %>% 
  bind_rows()

obj_size(BPagano.RtDriven)

run_alloscore(forecast_data = forecast_data, 
              truth_data = truth_data, K = Ks,
              mkeep = mkeep, reference_dates = forecast_dates_quick)

alloscore_BPagano.RtDriven_2021.12.27$xdf[[1]]
as1 <- slim(alloscore_BPagano.RtDriven_2021.12.27, id_cols = c("reference_date", "model"))

as1$xdf[[1]]
as2 <- as1 %>% mutate(xdf = map(xdf, ~select(.,-score_fun)))

obj_size(as1)
obj_size(as2) 





obj_size(alloscore_df)

tar_load(alloscore_CU.select_2022.01.03)

alloscore_CU.select_2022.01.03


obj_size(s1)
obj_size(s1_slim)





