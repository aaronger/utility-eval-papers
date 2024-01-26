library(tidyverse)
library(covidHubUtils)

loc <- "US"

inc_death_targets <- paste(1:4, "wk ahead inc death")
forecasts_death <- load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    dates = as.Date("2021-08-30"),
    date_window_size = 6,
    locations = loc,
    types = c("point", "quantile"),
    targets = inc_death_targets,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")
)

truth_for_plotting <- covidHubUtils::load_truth(
    locations = "US",
    truth_source = "JHU",
    target_variable = "inc death",
    data_location = "covidData"
)

plot_data <- get_plot_forecast_data(
    forecast_data = forecasts_death,
    truth_data = truth_for_plotting,
    models_to_plot = "COVIDhub-4_week_ensemble",
    forecast_dates_to_plot = as.Date(unique(forecasts_death$forecast_date)),
    horizons_to_plot = 4,
    quantiles_to_plot = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
    locations_to_plot = "US",
    plot_truth = TRUE,
    truth_source = "JHU",
    target_variable_to_plot = "inc death",
    hub = "US")

plot_data_forecast <- plot_data %>%
    dplyr::filter(truth_forecast == "forecast")

plot_data_truth <- plot_data %>%
    dplyr::filter(!is.na(point), truth_forecast == "truth") %>%
    dplyr::rename(truth_model = model) %>%
    dplyr::select(-forecast_date)

graph <- ggplot2::ggplot(
    data = plot_data_forecast,
    ggplot2::aes(x = target_end_date))
graph <- graph +
    ggplot2::geom_ribbon(
        data = plot_data_forecast %>%
            dplyr::filter(type == "quantile"),
        mapping = ggplot2::aes(
            ymin = lower,
            ymax = upper,
            group = `Prediction Interval`,
            fill = `Prediction Interval`
        ),
        alpha = 1.0, show.legend = TRUE
    ) +
    ggplot2::geom_line(
        data = plot_data_forecast %>%
          dplyr::filter(!is.na(point)),
        mapping = ggplot2::aes(
            x = target_end_date,
            y = point,
            group = interaction(model, location, forecast_date)
        ),
        color = "#08519c"
    ) +
    ggplot2::scale_fill_manual(
        name = "Prediction interval",
        values = c("#c6dbef", "#6baed6", "#4292c6")) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_color() +
    ggplot2::geom_point(
        data = plot_data_forecast %>%
            dplyr::filter(!is.na(point)),
        mapping = ggplot2::aes(
            x = target_end_date,
            y = point
        ),
        color = "#08519c"
    ) +
    ggplot2::geom_line(
        data = plot_data_truth %>%
            dplyr::filter(!is.na(point)),
        mapping = ggplot2::aes(
            x = target_end_date,
            y = point#,
            # color = truth_model
        )
    ) +
    ggplot2::geom_point(
        data = plot_data_truth %>%
            dplyr::filter(!is.na(point)),
        mapping = ggplot2::aes(
            x = target_end_date,
            y = point#,
            # color = truth_model
        )
    ) +
    # ggplot2::scale_color_manual(name = "Observed data", values = "black") +
    # ggplot2::scale_fill_brewer(name = "Prediction Interval", type="seq", palette = "Blues") +
    # ggplot2::scale_fill_viridis_d(name = "Prediction Interval", option = "D", begin = 0.2, end = 0.8) +
    ylab("Num. weekly deaths") +
    ggtitle("Ensemble forecasts of weekly deaths in the U.S.") +
    scale_x_date(
        "Date",
        limits = c(as.Date("2021-07-01"), as.Date("2021-11-30"))) +
    theme_bw() +
    theme(legend.position = "bottom")
graph

pdf("death_forecasts_cone.pdf", width = 9, height = 3)
graph
dev.off()

used_ps <- c(0.5, 0.975)

forecasts_w_dp <- forecasts_death %>%
    dplyr::filter(type != "point", quantile %in% used_ps) %>%
    dplyr::group_by(forecast_date, location, horizon, target_end_date) %>%
    dplyr::summarize(
        loc_scale = list(distfromq:::calc_loc_scale_params(ps = quantile,
                                                      qs = value,
                                                      dist = "norm")),
        ps = list(quantile),
        qs = list(value)
    ) %>%
    dplyr::mutate(
        d_fn = lapply(loc_scale, function(ls) {
            function(x) dnorm(x, mean = (ls$a), sd = (ls$b))
        }),
        p_fn = lapply(loc_scale, function(ls) {
            function(x) pnorm(x, mean = (ls$a), sd = (ls$b))
        }),
        q_fn = lapply(loc_scale, function(ls) {
            function(x) qnorm(x, mean = (ls$a), sd = (ls$b))
        })
        # d_fn = list(function(x) dnorm(x, mean = eval(loc_scale$a), sd = eval(loc_scale$b))),
        # p_fn = list(function(x) pnorm(x, mean = eval(loc_scale$a), sd = eval(loc_scale$b)))
    )




# used_ps <- c(0.025, 0.25, 0.5, 0.75, 0.975)
used_ps <- c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
# used_ps <- unique(forecasts_death$quantile)
# used_ps <- c(0.025, 0.5, 0.975)
# used_ps <- c(0.5, 0.975)
forecasts_w_dp <- forecasts_death %>%
    dplyr::filter(type != "point") %>%
    # dplyr::filter(quantile %in% c(0.025, 0.1, 0.25, 0.75, 0.9, 0.975)) %>%
    dplyr::filter(quantile %in% used_ps) %>%
    dplyr::group_by(forecast_date, location, horizon, target_end_date) %>%
    dplyr::summarize(
        d_fn = list(distfromq::make_d_fn(ps = quantile, qs = value,
                                         interior_method = "monoH.FC",
                                         upper_tail_dist = "norm")),
        p_fn = list(distfromq::make_p_fn(ps = quantile, qs = value,
                                         interior_method = "monoH.FC",
                                         upper_tail_dist = "norm")),
        q_fn = list(distfromq::make_q_fn(ps = quantile, qs = value,
                                         interior_method = "monoH.FC",
                                         upper_tail_dist = "norm")),
        ps = list(quantile),
        qs = list(value)
    )

pdfs_cdfs <- purrr::pmap_dfr(
    forecasts_w_dp[c("horizon", "d_fn", "p_fn", "q_fn")],
    function(horizon, d_fn, p_fn, q_fn) {
        xlim <- c(0.0, 25000)
        x_grid <- seq(from = xlim[1], to = xlim[2], length = 1001)
        data.frame(
            horizon = paste0("Horizon ", horizon),
            x = x_grid,
            d_val = d_fn(x_grid),
            p_val = p_fn(x_grid)
        )
    }
)

ggplot(data = pdfs_cdfs) +
    geom_line(mapping = aes(x = x, y = d_val)) +
    facet_wrap( ~ horizon) +
    theme_bw()

ribbons <- purrr::pmap_dfr(
    forecasts_w_dp[c("horizon", "d_fn", "p_fn", "q_fn")],
    function(horizon, d_fn, p_fn, q_fn) {
        x_grid_95 <- seq(from = q_fn(0.025), to = q_fn(0.975), length = 1001)
        ribbon_df_95 <- data.frame(
            x = x_grid_95,
            ymax = d_fn(x_grid_95),
            ymin = 0,
            level = "95"
        )

        x_grid_80 <- seq(from = q_fn(0.1), to = q_fn(0.9), length = 1001)
        ribbon_df_80 <- data.frame(
            x = x_grid_80,
            ymax = d_fn(x_grid_80),
            ymin = 0,
            level = "80"
        )

        x_grid_50 <- seq(from = q_fn(0.25), to = q_fn(0.75), length = 1001)
        ribbon_df_50 <- data.frame(
            x = x_grid_50,
            ymax = d_fn(x_grid_50),
            ymin = 0,
            level = "50"
        )
        
        dplyr::bind_rows(
            ribbon_df_50,
            ribbon_df_80,
            ribbon_df_95
        ) %>%
            dplyr::mutate(
                horizon = paste0("Horizon ", horizon),
                level = factor(level, levels = c("95", "80", "50"))
            )
    }
)


medians <- purrr::pmap_dfr(
    forecasts_w_dp[c("horizon", "d_fn", "p_fn", "q_fn")],
    function(horizon, d_fn, p_fn, q_fn) {
        data.frame(
            x = q_fn(0.5),
            xend = q_fn(0.5),
            y = 0,
            yend = d_fn(q_fn(0.5)),
            horizon = paste0("Horizon ", horizon)
        )
    }
)

p_pdf <- ggplot() +
    geom_ribbon(
        data = ribbons,
        mapping = aes(x = x, ymin = ymin, ymax = ymax, fill = level)
    ) +
    ggplot2::scale_fill_manual(
        name = "Prediction interval",
        values = c("#c6dbef", "#6baed6", "#4292c6")) +
    geom_line(
        data = pdfs_cdfs,
        mapping = aes(x = x, y = d_val)
    ) +
    geom_segment(
        data = medians,
        mapping = aes(x = x, y = y, xend = xend, yend = yend),
        color = "#08519c"
    ) +
    facet_wrap( ~ horizon, nrow = 1) +
    ylab("Predictive density") +
    xlab("Number of weekly deaths") +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5)
    )


pdf("death_forecasts_pdf.pdf", width = 10, height = 2.25)
p_pdf
dev.off()


p_cdf <- ggplot() +
    geom_line(
        # data = data.frame(x = x_grid, y = d_vals),
        data = data.frame(x = x_grid, y = p_vals),
        mapping = aes(x = x, y = y)
    ) +
    # geom_point(
    #     data = data.frame(x = qs, y = ps),
    #     mapping = aes(x = x, y = y)
    # ) +
    theme_bw()

cowplot::plot_grid(
    p_cdf,
    p_pdf,
    nrow = 2,
    align = "v"
)





cowplot::plot_grid(
    p_case,
    p_death,
    nrow = 2,
    align = "v"
)
dev.off()



# plot for multiple locations
inc_death_targets <- paste(1:4, "wk ahead inc death")
forecasts_death <- load_forecasts(
    models = "COVIDhub-4_week_ensemble",
    dates = seq.Date(as.Date("2020-10-05"), as.Date("2022-09-15"), by = 35),
    date_window_size = 6,
    locations = c("06", "48", "US"),
    types = c("point", "quantile"),
    targets = inc_death_targets,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")
)


p_death <- plot_forecasts(
    forecast_data = forecasts_death,
    truth_source = "JHU",
    target_variable = "inc death",
    intervals = .95,
    facet = ~ location,
    facet_ncol = 1,
    facet_scales = "free_y",
    fill_by_model = TRUE,
    fill_transparency = 0.5,
    subtitle = "none",
    show_caption = FALSE
) +
    scale_x_date(
        "Date",
        limits = c(as.Date("2020-09-01"), as.Date("2022-10-14"))) +
    # ggtitle("Weekly Incident Deaths") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")


pdf("ensemble_death_forecasts_multi_loc.pdf", width = 13, height = 9)
p_death
dev.off()




# plot of baseline forecasts
inc_death_targets <- paste(1:4, "wk ahead inc death")
forecasts_death <- load_forecasts(
    models = "COVIDhub-baseline",
    dates = seq.Date(as.Date("2020-10-05"), as.Date("2022-09-15"), by = 35),
    date_window_size = 6,
    locations = "12",
    types = c("point", "quantile"),
    targets = inc_death_targets,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")
)

p_death <- plot_forecasts(
    forecast_data = forecasts_death,
    truth_source = "JHU",
    target_variable = "inc death",
    intervals = c(0.5, 0.95),
    fill_by_model = TRUE,
    fill_transparency = 0.5,
    subtitle = "none",
    show_caption = FALSE
) +
    scale_x_date(
        "Date",
        limits = c(as.Date("2020-09-01"), as.Date("2022-10-14"))) +
    ggtitle("") +
    # ggtitle("Baseline forecasts of weekly incident deaths in Florida") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none")

pdf("baseline_death_forecasts.pdf", width = 13, height = 6)
p_death
dev.off()
