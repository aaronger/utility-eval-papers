


get_intervals <- function(df) {
  df %>% dplyr::filter(quantile %in% c(0.025, 0.1, 0.25, 0.75, 0.9, 0.975)) %>%
    dplyr::mutate(
      lu = ifelse(quantile < 0.5, "lower", "upper"),
      interval_level_num = ifelse(quantile < 0.5,
                                  100 * (1 - 2 * quantile),
                                  100 * (1 - 2 * (1 - quantile))),
      interval_level = paste0(as.integer(interval_level_num), "%")
    ) %>%
    dplyr::select(target_end_date, value, lu, interval_level, interval_level_num,
                  any_of(c("model", "location", "geo_value", "xmin", "xmax"))) %>%
    tidyr::pivot_wider(names_from = "lu", values_from = "value")
}


plot_one_hosp <- function(
    start_date = "2021-09-01",
    stop_date = "2022-01-31",
    f_colors = c("mediumblue", "royalblue", "dodgerblue"),
    f_width1 = 2,
    space = .5,
    f_width2 = f_width1 + .5,
    models = "COVIDhub-4_week_ensemble",
    location = "US"
) {
  truth <- truth %>% filter(location == !!location,
                            target_end_date >= start_date,
                            target_end_date <= stop_date)
  total_width <- (length(models) - 1)*(f_width1 + space) + f_width1
  half_width <- total_width/2
  fc_dat <- forecasts_hosp %>%
    dplyr::filter(location == !!location, model %in% models) %>%
    dplyr::mutate(
      xmin = target_end_date - half_width + (which(model == models) - 1)*(f_width1 + space),
      xmax = xmin + f_width1
    )
  p <- ggplot(data = truth) +
    geom_line(mapping = aes(x = target_end_date, y = value)) +
    geom_vline(
      data = data.frame(
        tedate = as.Date(c("2021-12-27", "2022-01-10")),
        Date = c("Forecast Date", "Target Date")
      ),
      mapping = aes(xintercept = tedate, linetype = Date),
      linewidth = 0.5) +
    geom_segment(
      data = fc_dat %>% filter(quantile == 0.5),
      mapping = aes(
        x = target_end_date - f_width2,
        xend = target_end_date + f_width2,
        y = value, yend = value), size = 1.5) +
    geom_rect(
      data = get_intervals(fc_dat),
      mapping = aes(
        xmin = xmin,
        xmax = xmax,
        ymin = lower,
        ymax = upper,
        fill = interval_level),
      alpha = .5) +
    scale_fill_manual(values = f_colors, name = "Predictive Interval") +
    scale_x_date(date_labels = "%b %y") +
    coord_cartesian(xlim = as.Date(c(start_date, stop_date)),
                    ylim = c(0, 1.3*max(truth$value))) +
    xlab("Date") +
    ylab("Daily Hospitalizations") +
    theme_classic() + guides(fill = guide_legend(override.aes = list(alpha = 1)))
  p
}

