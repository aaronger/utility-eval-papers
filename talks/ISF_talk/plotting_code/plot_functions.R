


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
                  any_of(c("model", "location", "code", "xmin", "xmax"))) %>%
    tidyr::pivot_wider(names_from = "lu", values_from = "value")
}

pal1 = c("mediumblue", "goldenrod", "#CD2626")


plot_hosp <- function(
    start_date = "2021-09-01",
    stop_date = "2022-01-31",
    f_date = "2021-12-27",
    te_date = "2022-01-10",
    f_colors = pal1,
    f_alpha = .5,
    f_width1 = 2,
    space = .5,
    models = "COVIDhub-4_week_ensemble",
    locations = "US",
    key_width = .15
) {
  locations <- locations %>% purrr::map(
    function(loc) {
      if (grepl("^[A-Z]{2}$", loc)) {
        hub_locations %>%
          dplyr::filter(abbreviation == loc, nchar(fips) == 2) %>% pull(fips)
      } else {
        sprintf("%02d", as.integer(loc))
      }
    })
  # compute aggregate rect and segment widths.
  total_width <- (length(models) - 1)*(f_width1 + space) + f_width1
  half_width <- total_width/2

  truth <- truth %>%
  dplyr::filter(location %in% locations,
    target_end_date >= start_date,
    target_end_date <= stop_date) %>%
  dplyr::mutate(
    validation = ifelse(target_end_date > as.Date(f_date), "Validation", "Historical"),
    code = toupper(geo_value),
    xmin = target_end_date - half_width - space,
    xmax = xmin + total_width + 2 * space)

  target_dates <- data.frame(
    Date = as.Date(c(f_date, te_date)),
    Date_name = c("Forecast Date", "Target Date"))

  fc_dat <- forecasts_hosp %>%
    dplyr::filter(location %in% locations, model %in% models) %>%
    dplyr::mutate(
      code = toupper(geo_value),
      model = forcats::fct_relevel(model, models),
      xmin = target_end_date - half_width + (match(model, models) - 1)*(f_width1 + space),
      xmax = xmin + f_width1)

  p <- ggplot(data = truth) +
    # truth data
    geom_line(mapping = aes(x = target_end_date, y = value, color = validation)) +
    scale_color_manual(
      values = c(
        "Historical" = "black",
        "Validation" = "darkgrey")) +
    # mark forecast and target end dates
    geom_vline(
      data = target_dates,
      mapping = aes(xintercept = Date, linetype = Date_name)) +
    labs(linetype = NULL) +
    # mark the relevant observation
    geom_segment(
      data = truth %>% dplyr::filter(target_end_date == te_date),
      mapping = aes(
        x = xmin,
        xend = xmax,
        y = value, yend = value, color = "Observed Value"), size = 1) +
    # Dummy layer for legend
    geom_blank(data = data.frame(validation = "Observed Value"), aes(color = validation)) +
    scale_color_manual(
      values = c("Observed Value" = "darkred",
                 "Historical" = "black",
                 "Validation" = "darkgrey"),
      name = NULL,
      breaks = "Observed Value",
      guide = guide_legend(override.aes = list(size = 1.5))) +
    # mark the predictive median
    geom_segment(
      data = fc_dat %>% filter(quantile == 0.5),
      mapping = aes(
        x = xmin,
        xend = xmax,
        y = value, yend = value), size = 1.5) +
    # predictive interval rects
    geom_rect(
      data = get_intervals(fc_dat),
      mapping = aes(
        xmin = xmin,
        xmax = xmax,
        ymin = lower,
        ymax = upper,
        fill = model),
      alpha = f_alpha) +
    scale_fill_manual(values = f_colors, name = "Model") +
    geom_rect(
      data = get_intervals(fc_dat) %>% dplyr::filter(model == models[1]),
      mapping = aes(
        xmin = xmin,
        xmax = xmin,
        ymin = lower,
        ymax = upper,
        alpha = forcats::fct_rev(interval_level))) +
    scale_alpha_manual(
      values = c("95%" = .3, "80%" = .6, "50%" = .9),
      name = "Predictive Interval") +
    scale_x_date(date_labels = "%b %y") +
    coord_cartesian(xlim = as.Date(c(start_date, stop_date)),
                    ylim = c(0, 1.3*max(truth$value))) +
    xlab("Date") +
    ylab("Daily Hospitalizations")  +
    theme_classic() +
    guides(
      alpha = guide_legend(order = 1),
      fill = guide_legend(override.aes = list(alpha = 1), order = 2),
      color = guide_legend(order = 3))
  if (length(locations) > 1) {
    p <- p + facet_geo(~ code, grid = geofacet::us_state_grid1) +
    scale_x_date(breaks = as.Date(c("2021-12-01", "2022-01-01")),
                 date_labels = "%b %y") +
    theme(axis.title = element_blank())
  }
  p <- p + theme(
      legend.key.width = unit(f_width1*key_width, "cm"),
      panel.spacing = unit(0.1, "lines"),
      axis.text.x = element_text(hjust = -0.1))
  p
}
