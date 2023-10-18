## plot to create a one-location allocation inset plot

plot_allocation_forecasts <- function(forecast_data, truth_data, loc_abbr, tar_date, intervals = c(.50, .80, .95)) {
  require(ggplot2)
  require(patchwork)
  require(tidyr)

  p_medians <- forecast_data |>
    filter(abbreviation == loc_abbr, quantile == 0.5) |>
    ggplot(aes(x=target_end_date, y= value)) +
    geom_point(alpha=.3) +
    geom_point(data=filter(truth_data, location=="25", target_end_date <= as.Date("2022-03-15"), target_end_date >= as.Date("2021-12-01")),
               aes(x=target_end_date), shape=4) +
    xlab(NULL) +
    ylab("New hospital admissions") +
    ggtitle(paste("Observed and forecasted hospital admissions in", loc_abbr))


  qtiles <- as.character(c((1-intervals)/2, 1-(1-intervals)/2))
  fdat_focus <- forecast_data |>
    mutate(quantile_str = as.character(quantile)) |>
    filter(abbreviation == loc_abbr,
           target_end_date == as.Date(tar_date),
           quantile_str %in% qtiles) |>
    tidyr::pivot_wider(id_cols= -quantile,
                       names_from = quantile_str,
                       values_from = value,
                       names_prefix="q")

  truth_value <- truth_data |>
    filter(abbreviation == loc_abbr,
           target_end_date == tar_date) |>
    pull(value)

  p_one_date <- ggplot(fdat_focus, aes(x=model)) +
    geom_linerange(aes(ymin=q0.025, ymax=q0.975), linewidth=2, alpha=.3) +
    geom_linerange(aes(ymin=q0.1, ymax=q0.9), linewidth=2, alpha=.3) +
    geom_linerange(aes(ymin=q0.25, ymax=q0.75), linewidth=2, alpha=.3) +
    geom_hline(yintercept = truth_value, linetype=2) +
    scale_x_discrete(name="models", labels=NULL) +
    ggtitle(tar_date)

  p_medians + patchwork::inset_element(p_one_date, left = .6, right = .99, top = .99, bottom = .5)
}
