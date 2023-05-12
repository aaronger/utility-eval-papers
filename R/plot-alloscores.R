
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

  names(palette) <- c(models[models != "COVIDhub-baseline"], "COVIDhub-baseline")

  solid_models <- c("COVIDhub-baseline", "MUNI-ARIMA", "CU-select")
  linetypes <- c(2, 4, 5, 6, 2, 4, 5, rep(1, 3))
  names(linetypes) <- c(models[!(models %in% solid_models)], solid_models)

  p <- ggplot(
    data = alloscores,
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

  "allo_scores_wide.pdf"
}
