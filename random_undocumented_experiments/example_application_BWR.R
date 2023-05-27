#Changed example allocation as a couple of covidhubUtil functions were not working.

library(tidyverse)
library(covidHubUtils)
if (!require("distfromq")) {
  devtools::install_github("reichlab/distfromq")
}
library(distfromq)
if (!require("alloscore")) {
  devtools::install_github("aaronger/alloscore")
}
library(alloscore)

# Load the gh package to check for recent and probably crucial commits to alloscore
if (!require("gh")) {
  install.packages("gh")
}
if (
  gh::gh("/repos/aaronger/alloscore/commits", .limit = 1)[[1]]$commit$committer$date > packageDate("alloscore")
) {
  message("Alloscore may be out of date...")
}


########################
### Working versions of required covidHubUtils functions:

align_forecasts_one_temporal_resolution <- function(
    forecasts,
    reference_dates,
    reference_weekday,
    reference_windows,
    drop_nonpos_relative_horizons
) {
  if (length(unique(forecasts$temporal_resolution)) > 1) {
    stop("standardize_forecasts_one_temporal_resolution only supports forecasts at a single temporal resolution.")
  }
  
  if (is.null(reference_windows)) {
    if (reference_weekday == "Saturday") {
      reference_windows <- -4:2
    } else if (reference_weekday == "Monday") {
      reference_windows <- -6:0
    } else {
      stop("Reference windows undefined")
    }
  }
  
  if (!is.list(reference_windows)) {
    reference_windows <- list(reference_windows)
  }
  
  if (!is.null(reference_dates)) {
    # ensure we have dates
    reference_dates <- as.Date(reference_dates)
  } else {
    # every date from that of first forecast - diameter of first window
    # to that of last forecast + diameter of last window
    all_dates <- seq(
      min(forecasts$forecast_date) - (
        max(sort(reference_windows[[1]])) -
          min(sort(reference_windows[[1]]))
      ),
      max(forecasts$forecast_date) + (
        max(sort(reference_windows[[length(reference_windows)]])) -
          min(sort(reference_windows[[length(reference_windows)]])) 
      ),
      by = 1
    )
    
    # keep the dates identified above that are the specified reference_weekday
    reference_dates <- all_dates[weekdays(all_dates) == reference_weekday]
  }
  
  # create a tibble where each row contains:
  # - a possible forecast date
  # - a reference date to which that forecast date should be assigned
  ref_df <- tibble(
    reference_date = reference_dates,
    forecast_date = purrr::map2(
      reference_date, 
      reference_windows, 
      ~.x+.y
    )
  ) %>% unnest(cols = forecast_date)
  
  # ensure that in the tibble constructed above, each forecast date is
  # associated with at most one reference date
  # this could be violated if some windows are overlapping
  reps <- ref_df %>%
    dplyr::group_by(forecast_date) %>%
    dplyr::tally() %>% 
    dplyr::filter(n > 1)
  if (nrow(reps) > 0) {
    stop(paste0(
      "The following forecast dates are associated with multiple reference dates: ",
      paste(reps %>% dplyr::pull(forecast_date), collapse = ", ")
    ))
  }
  
  # join with the reference date lookup table above
  # and calculate the relative horizon
  forecasts <- forecasts %>% 
    dplyr::left_join(ref_df, by = "forecast_date") %>% 
    dplyr::mutate(
      ts_days = ifelse(temporal_resolution == "wk", 7, 1),
      relative_horizon = 
        ceiling(as.numeric((target_end_date - reference_date) / ts_days))
    ) %>%
    dplyr::select(-ts_days)
  
  if (drop_nonpos_relative_horizons) {
    forecasts <- forecasts %>%
      dplyr::filter(relative_horizon > 0)
  }
  
  return(forecasts)
}

 hub_repo_path <- "~/Documents/Research/covid19-forecast-hub/"
#hub_repo_path <- "~/research/epi/covid/covid19-forecast-hub/"
# hub_repo_path <- "~/covid/covid19-forecast-hub/"

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
    align_forecasts() %>%
    dplyr::filter(
        relative_horizon == 14,
        location < 57)

truth <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp"
)

trad_scores <- covidHubUtils::score_forecasts(
    forecasts = forecasts_hosp,
    truth = truth,
    use_median_as_point = TRUE
)

fhosp1 <- forecasts_hosp %>%
  select(-type) %>%
  nest(ps = quantile, qs = value) %>%
  relocate(ps, qs) %>%
  mutate(
    ps = map(ps, deframe),
    qs = map(qs, deframe)
  )

fhosp1 %>% count(model)

# keep selected models, rename 4 week ensemble to ensemble for plot legend purposes
mkeep <- c("BPagano-RtDriven", "COVIDhub-4_week_ensemble", "COVIDhub-baseline",
    "CU-select", "IHME-CurveFit", "JHUAPL-Bucky", "JHUAPL-Gecko",
    "MUNI-ARIMA", "USC-SI_kJalpha", "UVA-Ensemble")
fhosp1 <- fhosp1 %>%
  dplyr::filter(model %in% mkeep) %>%
  dplyr::mutate(
    model = ifelse(model == "COVIDhub-4_week_ensemble", "COVIDhub-ensemble", model)
  )
mkeep[mkeep == "COVIDhub-4_week_ensemble"] <- "COVIDhub-ensemble"

trad_scores %>%
    filter(model %in% mkeep) %>%
    group_by(model) %>%
    summarize(wis = sum(wis)) %>%
    arrange(wis)

fhosp1 <- fhosp1 %>%
  add_pdqr_funs(dist = "distfromq", types = c("p", "q")) %>%
  relocate(dist, F, Q)

fhosp1 <- fhosp1 %>% left_join(
  truth %>% select(location, target_end_date, value),
  by = c("location", "target_end_date"))

Ks <- seq(from = 5000, to = 80000, by = 500)
Kdf <- data.frame(matrix(Ks,nrow = 1))
names(Kdf) <- paste0("K=",Ks)

(ascores <- fhosp1 %>%
    bind_cols(Kdf) %>%
    group_by(model) %>%
  summarise(
    ytot = sum(value),
    across(starts_with("K"), ~alloscore:::alloscore(
        y = value,
        F = F,
        Q = Q,
        w = 1,
        K = unique(.x),
        kappa = 1,
        alpha = 1,
        dg = 1,
        eps_K = .01,
        eps_lam = 1e-5,
        verbose = TRUE,
        against_oracle = TRUE
        ))
  ))

ascores_long <- ascores %>%
    tidyr::pivot_longer(
        starts_with("K="),
        names_prefix = "K=",
        names_to = "K"
    ) %>%
    dplyr::mutate(K = as.numeric(K))

palette <- c("#004488",
             "#117733",
             "#44AA99",
             "#88CCEE",
             "#DDCC77",
             "#999933",
             "#CC6677",
             "#882255",
             "#AA4499",
             "#000000")
names(palette) <- c(mkeep[mkeep != "COVIDhub-baseline"], "COVIDhub-baseline")

solid_models <- c("COVIDhub-baseline", "MUNI-ARIMA", "CU-select")
linetypes <- c(2, 4, 5, 6, 2, 4, 5, rep(1, 3))
names(linetypes) <- c(mkeep[!(mkeep %in% solid_models)], solid_models)

p <- ggplot(
    data = ascores_long,
    mapping = aes(x = K, y = value, color = model, linetype = model)
) +
    geom_line() +
    scale_color_manual(values = palette) +
    scale_linetype_manual(values = linetypes) +
    geom_vline(xintercept = unique(ascores$ytot), linetype = 2) +
    xlab("Total Allocation Constraint (K)") +
    ylab("Allocation Score\n(Excess unmet need)") +
    theme_bw()

pdf("allo_scores_wide.pdf", width = 7, height = 3.5)
print(p)
dev.off()


p <- ggplot(
    data = ascores_long,
    mapping = aes(x = K, y = value, color = model, linetype = model)
) +
    geom_line() +
    scale_color_manual(values = palette) +
    scale_linetype_manual(values = linetypes) +
    geom_vline(xintercept = unique(ascores$ytot), linetype = 2) +
    coord_cartesian(xlim = c(5000, 40000)) +
    xlab("Total Allocation Constraint (K)") +
    ylab("Allocation Score\n(Excess unmet need)") +
    theme_bw()

pdf("allo_scores_trunc.pdf", width = 6, height = 3.5)
print(p)
dev.off()


model_highlight <- "JHUAPL-Gecko"
p <- ggplot(
    data = ascores_long %>%
        dplyr::mutate(model_highlight = as.character(model %in% !!model_highlight)),
    mapping = aes(x = K, y = value, color = model, linetype = model,
                  alpha = model_highlight, linewidth = model_highlight)
) +
    geom_line() +
    scale_color_manual(values = palette) +
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4), guide = FALSE) +
    scale_linewidth_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4), guide = FALSE) +
    geom_vline(xintercept = unique(ascores$ytot), linetype = 2) +
    coord_cartesian(xlim = c(5000, 40000)) +
    xlab("Total Allocation Constraint (K)") +
    ylab("Allocation Score\n(Excess unmet need)") +
    theme_bw()

pdf("allo_scores_gecko.pdf", width = 6, height = 3.5)
print(p)
dev.off()



model_highlight <- c("COVIDhub-ensemble", "JHUAPL-Gecko", "MUNI-ARIMA")
p <- ggplot(
    data = ascores_long %>%
        dplyr::mutate(model_highlight = as.character(model %in% !!model_highlight)),
    mapping = aes(x = K, y = value, color = model, linetype = model,
                  alpha = model_highlight, linewidth = model_highlight)
) +
    geom_line() +
    scale_color_manual(values = palette) +
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4), guide = FALSE) +
    scale_linewidth_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4), guide = FALSE) +
    guides(color = guide_legend(nrow=4, byrow=TRUE),
           linetype = guide_legend(nrow=4, byrow=TRUE)) +
    geom_vline(xintercept = unique(ascores$ytot), linetype = 2) +
    coord_cartesian(xlim = c(5000, 40000)) +
    xlab("Total Allocation Constraint (K)") +
    ylab("Allocation Score\n(Excess unmet need)") +
    theme_bw() +
    theme(legend.pos = "bottom")
    # theme(legend.pos = "none")

pdf("allo_scores_top3.pdf", width = 6, height = 5)
print(p)
dev.off()



model_highlight <- c("JHUAPL-Bucky", "MUNI-ARIMA")
p <- ggplot(
    data = ascores_long %>%
        dplyr::mutate(model_highlight = as.character(model %in% !!model_highlight)),
    mapping = aes(x = K, y = value, color = model, linetype = model,
                  alpha = model_highlight, linewidth = model_highlight)
) +
    geom_line() +
    scale_color_manual(values = palette) +
    scale_linetype_manual(values = linetypes) +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4), guide = FALSE) +
    scale_linewidth_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4), guide = FALSE) +
    guides(color = guide_legend(nrow=4, byrow=TRUE),
           linetype = guide_legend(nrow=4, byrow=TRUE)) +
    geom_vline(xintercept = unique(ascores$ytot), linetype = 2) +
    coord_cartesian(xlim = c(5000, 40000)) +
    xlab("Total Allocation Constraint (K)") +
    ylab("Allocation Score\n(Excess unmet need)") +
    theme_bw() +
    theme(legend.pos = "bottom")
    # theme(legend.pos = "none")

pdf("allo_scores_bucky_muni.pdf", width = 6, height = 5)
print(p)
dev.off()




act_vs_pred_df <- truth %>%
    dplyr::filter(location < 57, target_end_date == "2022-01-10") %>%
    dplyr::select(all_of(c("abbreviation", "value"))) %>%
    dplyr::left_join(
        forecasts_hosp %>%
            filter(
                relative_horizon == 14,
                location < 57,
                quantile == 0.5,
                model %in% c("MUNI-ARIMA", "JHUAPL-Bucky")
            ) %>%
            select(model, abbreviation, forecast = value),
        by = "abbreviation",
        multiple = "all"
    ) %>%
    tidyr::pivot_longer(
        c("value", "forecast")
    ) %>%
    dplyr::mutate(
        name = ifelse(name == "value", "observed", "predictive\nmedian"),
        state = ifelse(abbreviation == "FL", "FL", "Other")
    )

p <- ggplot(data = act_vs_pred_df) +
    geom_line(mapping = aes(x = name, y = value, group = abbreviation)) +
    xlab("") +
    ylab("Daily hospital admissions") +
    facet_wrap(~ model) +
    theme_bw()

pdf("obs_v_pred_bucky_muni.pdf", width = 9, height = 4)
print(p)
dev.off()

p <- ggplot(data = act_vs_pred_df) +
    geom_line(mapping = aes(x = name, y = value, group = abbreviation, color = state)) +
    scale_color_manual(values = c("FL" = "orange", "Other" = "black")) +
    xlab("") +
    ylab("Daily hospital admissions") +
    facet_wrap(~ model) +
    theme_bw()

pdf("obs_v_pred_bucky_muni_highlight_fl.pdf", width = 9.5, height = 4)
print(p)
dev.off()






align_forecasts <- function (forecasts, reference_dates = list(wk = NULL, day = NULL), 
          reference_weekday = list(wk = "Saturday", day = "Monday"), 
          reference_windows = list(wk = -4:2, day = -6:0), drop_nonpos_relative_horizons = TRUE) 
{
  if (!all(c("wk", "day") %in% (names(reference_dates)) & c("wk", 
                                                          "day") %in% (names(reference_weekday)) & c("wk", "day") %in% 
        (names(reference_windows)))) {
    stop(paste0("reference_dates, reference_weekday, and reference_windows ", 
                "must be named lists with entries for 'wk' and 'day'."))
  }
  aligned_forecasts <- purrr::map_dfr(unique(forecasts$temporal_resolution), 
                                      function(temporal_res) {
                                        align_forecasts_one_temporal_resolution(forecasts = forecasts %>% 
                                                                                  dplyr::filter(temporal_resolution == temporal_res), 
                                                                                reference_dates = reference_dates[[temporal_res]], 
                                                                                reference_weekday = reference_weekday[[temporal_res]], 
                                                                                reference_windows = reference_windows[[temporal_res]], 
                                                                                drop_nonpos_relative_horizons = drop_nonpos_relative_horizons)
                                      })
  return(dplyr::right_join(forecasts, aligned_forecasts, by = names(forecasts)) %>% 
           dplyr::relocate(reference_date, .after = forecast_date) %>% 
           dplyr::relocate(relative_horizon, .after = horizon))
}



