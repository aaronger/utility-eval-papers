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


