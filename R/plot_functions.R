


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
    st_colors = NULL,
    f_alpha = .5,
    f_width1 = 2,
    space = .5,
    models = "COVIDhub-ensemble",
    locations = "US",
    geofacet = FALSE,
    facet_ncol=3,
    key_width = .15,
    allocations = FALSE,
    one_K = NULL,
    forecasts_hosp = forecasts_hosp,
    alloscores,
    truth,
    free_y = FALSE
) {
  ## removing below as it adds a dependency on hub_locations and I don't think is necessary if we use the abbreviations
  # loc_abbrevs <- locations
  # locations <- locations %>% purrr::map(
  #   function(loc) {
  #     if (grepl("^[A-Z]{2}$", loc)) {
  #       hub_locations %>%
  #         dplyr::filter(abbreviation == loc, nchar(fips) == 2) %>% pull(fips)
  #     } else {
  #       sprintf("%02d", as.integer(loc))
  #     }
  #   })
  # compute aggregate rect and segment widths.
  total_width <- (length(models) - 1)*(f_width1 + space) + f_width1
  half_width <- total_width/2

  truth <- truth %>%
  dplyr::filter(abbreviation %in% locations,
    target_end_date >= start_date,
    target_end_date <= stop_date) %>%
  dplyr::mutate(
    validation = ifelse(target_end_date > as.Date(f_date), "Validation", "Historical"),
    code = geo_value,
    # code = as.factor(toupper(geo_value)),
    xmin = target_end_date - half_width - space,
    xmax = xmin + total_width + 2 * space)

  target_dates <- data.frame(
    Date = as.Date(c(f_date, te_date)),
    Date_name = c(paste("Forecast Date,", f_date), paste("Target Date,", te_date)))

  fc_dat <- forecasts_hosp %>%
    dplyr::filter(abbreviation %in% locations, model %in% models) %>%
    dplyr::mutate(
      code = geo_value,
      # code = as.factor(toupper(geo_value)),
      model = forcats::fct_relevel(model, models),
      xmin = target_end_date - half_width + (match(model, models) - 1)*(f_width1 + space),
      xmax = xmin + f_width1)

  # fc_dat <- fc_dat %>% mutate(
  #   code = fct_relevel(code, loc_abbrevs))
  # truth <- truth %>% mutate(
  #   code = fct_relevel(code, loc_abbrevs))

  # if (!is.null(st_colors)) {
  #   fc_dat <- fc_dat %>% mutate(
  #     code = fct_relevel(code, names(st_colors)))
  #   truth <- truth %>% mutate(
  #     code = fct_relevel(code, names(st_colors)))
  # }

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
        y = value, yend = value, color = "Observed Value"), linewidth = .5) +
    # Dummy layer for legend
    geom_blank(data = data.frame(validation = "Observed Value"), aes(color = validation)) +
    scale_color_manual(
      values = c("Observed Value" = "darkred",
                 "Historical" = "black",
                 "Validation" = "darkgrey"),
      name = NULL,
      breaks = "Observed Value",
      guide = guide_legend(override.aes = list(linewidth = 1.5))) +
    # mark the predictive median
    geom_segment(
      data = fc_dat %>% filter(quantile == 0.5),
      mapping = aes(
        x = xmin,
        xend = xmax,
        y = value, yend = value), linewidth = 1.5)
    # predictive interval rects
    if (!is.null(st_colors)) {
      p <- p + geom_rect(
      data = get_intervals(fc_dat),
      mapping = aes(
        xmin = xmin,
        xmax = xmax,
        ymin = lower,
        ymax = upper,
        fill = code),
      alpha = f_alpha) +
        scale_fill_manual(values = st_colors, name = "State")
   } else {
    p <- p + geom_rect(
      data = get_intervals(fc_dat),
      mapping = aes(
        xmin = xmin,
        xmax = xmax,
        ymin = lower,
        ymax = upper,
        fill = model),
      alpha = f_alpha) + scale_fill_manual(values = f_colors, name = "Model")
    }
  p <- p +
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
    scale_x_date(date_labels = "%b %d") +
    coord_cartesian(xlim = as.Date(c(start_date, stop_date)),
                    ylim = c(0, 1.3*max(truth$value))) +
    xlab("Date") +
    ylab("Daily Hospitalizations")  +
    theme_classic() +
    guides(
      alpha = guide_legend(order = 1),
      fill = guide_legend(override.aes = list(alpha = 1), order = 2),
      color = guide_legend(order = 3))
  if (allocations) {
    adf <- alloscores %>% filter(reference_date == f_date, model %in% models, abbreviation %in% locations)
    if (is.null(one_K)) {
      y_tot <- get_ytot(adf) %>% filter(reference_date == f_date) %>%
        pull(ytot)
      K_toplot <- adf$K[which.min(abs(adf$K - y_tot))]
    } else {
      K_toplot <- one_K
    }
    adf <- adf %>% filter(
      reference_date == f_date,
      K == K_toplot
    ) %>%
      mutate(
        reference_date = as.Date(reference_date)) %>%
      rename(code = abbreviation) %>%
      # mutate(code = fct_relevel(code, loc_abbrevs)) %>%
      select(model:y)
    adf_x <- fc_dat %>%
      left_join(adf, by = c("reference_date", "model", "code")) %>%
      mutate(
        xmin = xmin + f_width1*2/5,
        xmax = xmax - f_width1*2/5
      ) |>
      rename(allocation = x,
             need = y) |>
      mutate(`unmet resource need` = ifelse(allocation < need,
                                            need - allocation,
                                            0),
             `need met by allocated resources` = ifelse(allocation < need,
                                                        allocation,
                                                        need),
             `allocation exceeding need` = ifelse(allocation < need,
                                                  0,
                                                  allocation - need)
      )
    p <- p +
      ## add the unmet need
      geom_rect(
      data = adf_x,
      mapping = aes(
        xmin = xmin,
        xmax = xmax,
        ymin = `need met by allocated resources`,
        ymax = `need met by allocated resources` + `unmet resource need`),
      fill = "#fb6a4a") +
      ## add the need met by allocated resources
      geom_rect(
        data = adf_x,
        mapping = aes(
          xmin = xmin,
          xmax = xmax,
          ymin = 0,
          ymax = `need met by allocated resources`),
        fill = "#807dba") +
      ## add the allocation exceeding need
      geom_rect(
        data = adf_x,
        mapping = aes(
          xmin = xmin,
          xmax = xmax,
          ymin = `need met by allocated resources`,
          ymax = `need met by allocated resources` + `allocation exceeding need`),
        fill = "#9ecae1") +
      scale_y_continuous(expand=c(0,0))
  }
  if (geofacet) {
    require(geofacet)
    p <- p + facet_geo(~ code, grid = geofacet::us_state_grid2) +
    scale_x_date(breaks = as.Date(c("2021-12-01", "2022-01-01")),
                 date_labels = "%b %d") +
    theme(axis.title = element_blank())
  } else if (length(locations) > 1) {
    if (free_y) {
      p <- p + facet_wrap(~ code, scales = "free_y", ncol=facet_ncol)
    } else {
      p <- p + facet_wrap(~ code, ncol=facet_ncol)
    }
  }
  p <- p + theme(
      legend.key.width = unit(f_width1*key_width, "cm"),
      panel.spacing = unit(0.1, "lines"),
      #axis.text.x = element_text(hjust = -0.05)
      )
  p
}

get_ytot <- function(df) {
  if ("xdf" %in% names(df)) {
    df %>% group_by(reference_date) %>% slice(1) %>%
      transmute(map_vec(xdf, ~ summarise(., ytot = sum(y))))
  } else {
    df %>% group_by(model, reference_date, K) %>%
      summarise(ytot = sum(y))
  }
}
add_ytot <- function(df) {
  ytot <- get_ytot(df)
  df %>% left_join(ytot, by = "reference_date")
}
