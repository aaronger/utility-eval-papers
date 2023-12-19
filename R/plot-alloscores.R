
plot_K_v_alloscore <- function(alloscores) {
  library(ggplot2)

  models <- unique(alloscores$model)

  # rename 4 week ensemble to ensemble for plot legend purposes
  models[models == "COVIDhub-4_week_ensemble"] <- "COVIDhub-ensemble"


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

  palette <- palette[1:length(models)]

  names(palette) <- c(models[models != "COVIDhub-baseline"], "COVIDhub-baseline")

  solid_models <- c("COVIDhub-baseline", "MUNI-ARIMA", "CU-select")
  linetypes <- c(2, 4, 5, 6, 2, 4, 5, rep(1, 3))[1:length(models)]
  names(linetypes) <- c(models[!(models %in% solid_models)], solid_models)

  p <- ggplot(
    data = alloscores,
    mapping = aes(x = K, y = score, color = model, linetype = model)
  ) +
    geom_line() +
    scale_color_manual(values = palette) +
    scale_linetype_manual(values = linetypes) +
    geom_vline(aes(xintercept = ytot), linetype = 2) +
    xlab("Total Allocation Constraint (K)") +
    ylab("Allocation Score\n(Excess unmet need)") +
    facet_wrap(vars(reference_date)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  pdf("figures/allo_scores_wide.pdf", width = 7, height = 3.5)
  print(p)
  dev.off()

  "figures/allo_scores_wide.pdf"
}

### thermometer plot for muni-vs-bucky

plot_thermo <- function(all_alloscore_data, forecast_data, truth_data) {

mvbucky_dist_alloc <- plot_hosp(models = c("JHUAPL-Bucky", "MUNI-ARIMA"),
          start_date = "2021-12-20",
          locations = c("FL", "CA", "TX", "NY", "PA", "OH", "NJ", "IL", "GA"),
          f_width1 = 4, f_alpha = .4,
          allocations = TRUE, 
          forecasts_hosp = forecast_data %>%  filter(reference_date == "2021-12-27"),
          alloscores = all_alloscore_data %>% filter(reference_date == "2021-12-27"),
          truth = truth_data,
          f_colors = c("#4A708B", "#CD2626")) + labs(x = NULL)

pdf("figures/mvbucky_dist_alloc.pdf", width = 10, height = 6)
print(mvbucky_dist_alloc)
dev.off()

"figures/mvbucky_dist_alloc.pdf"
}






