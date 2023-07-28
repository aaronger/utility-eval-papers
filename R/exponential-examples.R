# Figures for examples with forecasts that are exponential distributions
library(ggplot2)
library(grid)
library(ggpubr)

sigma_1 <- 2
sigma_2 <- 3

K <- c(5, 10)

tau <- 1 - exp(-K / (sigma_1 + sigma_2))

x_1star <- qexp(tau, rate = 1 / sigma_1)
x_2star <- qexp(tau, rate = 1 / sigma_2)

n_grid <- 101
grid_l <- 0.0
grid_u <- 11.0

y_grid <- seq(from = grid_l, to = grid_u, length.out = n_grid)
y_grid_jt <- tidyr::expand_grid(
    y_1 = y_grid,
    y_2 = y_grid)

s_bar <- function(x_1, x_2) {
    sigma_1 * exp(-x_1 / sigma_1)*(x_1>=0) + sigma_2 * exp(-x_2 / sigma_2)*(x_2>=0) +
    (sigma_1 - x_1)*(x_1<0) + (sigma_2 - x_2)*(x_2<0) 
}

s_bar_df <- y_grid_jt %>%
    dplyr::mutate(
        s_bar = s_bar(y_1, y_2)
    )

# https://stackoverflow.com/questions/35703983/how-to-change-angle-of-line-in-customized-legend-in-ggplot2

# internal function borrowed from ggplot2
`%||%` <- function (a, b) {
    if (!is.null(a))
        a
    else b
}
GeomAbline$draw_key <- function (data, params, size) {
    segmentsGrob(0, 1, 1, 0,
                 gp = gpar(col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
                 lwd = (data$linewidth %||% 0.5) * .pt,
                 lty = data$linetype %||% 1,
                 lineend = params$lineend %||% "butt"))
}

label_parse <- function(breaks) {
   parse(text = breaks)
}

# alloc_labels <- paste0("paste('(', x[1], ' = ', ", x_1star, ", ', ', x[2], ' = ',", x_2star, ", ') at K = ", K, "')")

alloc_labels <- paste0("at K = ", K)

p_sbar <- ggplot(data = s_bar_df) +
    geom_raster(aes(x = y_1, y = y_2, fill = s_bar)) +
    # geom_contour(mapping = aes(x = y_1, y = y_2, z = s_bar),
    #              breaks = c(0.25, 0.5, 1, 2, 4),
    #              color = "grey", size = 0.2) + 
    geom_contour(mapping = aes(x = y_1, y = y_2, z = s_bar),
                #  breaks = c(s_bar(x_1star, x_2star), seq(0, 10, by = .5))) + 
                 breaks = s_bar(x_1star, x_2star)) +
    geom_abline(
        data = data.frame(K = K, K_label = paste0("K = ", K), slope = -1),
        mapping = aes(intercept = K, slope = slope, linetype = K_label)) +
    geom_point(
        data = data.frame(K_label = factor(alloc_labels, levels = rev(alloc_labels)),
                          x = x_1star, y = x_2star),
        mapping = aes(x = x, y = y, shape = K_label),
        size = 2,
        color = "orange"
    ) +
    scale_fill_viridis_c("Expected Loss", trans = "log",
                         breaks = c(0.125, 0.25, 0.5, 1, 2, 4),
                         labels = c("0.125", "0.25", "0.5", "1", "2", "4")) + #,
                        #  labels = 1:5) +
    # scale_color_viridis_c("Expected Loss", trans = "log",
    #                      breaks = c(0.25, 0.5, 1, 2, 4)) + #,
    #                     #  labels = 1:5) +
    scale_linetype("Constraint") +
    # scale_shape("Allocation", label = label_parse) +
    scale_shape("Allocation") +
    xlab("Resources Allocated to Location 1") +
    scale_x_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
    ylab("") +
    scale_y_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
    guides(
        fill = guide_colorbar(order = 1),
        linetype = guide_legend(order = 2),
        shape = guide_legend(order = 3, label.hjust = 0)
    ) +
    coord_cartesian(xlim = c(grid_l, grid_u),
                    ylim = c(grid_l, grid_u),
                    expand = FALSE) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          legend.margin = margin(),
          legend.box.margin = margin())

sbar_legend <- ggpubr::get_legend(p_sbar,
                                  position = "left")
                                #   position = "right")
p_sbar <- p_sbar + theme(legend.position = "none")

p_pred1 <- ggplot(
        data = data.frame(
            y = y_grid,
            density = dexp(y_grid, rate = 1 / sigma_1))
    ) +
    geom_line(mapping = aes(x = y, y = density)) +
    scale_x_continuous(breaks = seq(from = 0, to = 10, by = 2)) +
    coord_cartesian(xlim = c(grid_l, grid_u),
                    ylim = c(0, 0.5),
                    expand = FALSE) +
    xlab("Resource Demand in Location 1") +
    # ylab("Predictive Density") +
    ylab("") +
    theme_bw() +
    theme(axis.title.y = element_blank())

p_pred2 <- ggplot(
        data = data.frame(
            y = y_grid,
            density = dexp(y_grid, rate = 1 / sigma_2))
    ) +
    geom_line(mapping = aes(x = y, y = density)) +
    xlab("Resource Demand in Location 2") +
    ylab("Predictive Density") +
    scale_y_reverse(limits = c(0.5, 0)) +
    # scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(position = "top",
                       breaks = seq(from = 0, to = 10, by = 2)) +
    # scale_y_reverse(position = "right") +
    # scale_x_continuous(position = "top") +
    coord_flip(xlim = c(grid_l, grid_u),
               expand = FALSE) +
    # coord_cartesian(xlim = c(grid_l, grid_u),
    #                 expand = FALSE) +
    theme_bw() +
    theme(axis.title.y.right = element_text(angle = 90))


    # +
    # theme(axis.title.x.top = element_text(),
    #       axis.title.x.bottom = element_blank())


pdf('../figures/exponential_pred_expected_loss.pdf',
    width = 8,
    height = 6.2)
plot_layout <- grid.layout(
    nrow = 2, ncol = 6,
    widths = unit(c(0.42, 0.5, 1, 0.2, 1, 0.56), c("null", "lines", "lines", "lines", "null", "null")),
    heights = unit(c(1, 0.53), rep("null", 2)))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

print(as_ggplot(sbar_legend),
  vp = viewport(
    y = unit(0, "npc"),
    layout.pos.row = 1,
    # layout.pos.col = 1,
    layout.pos.col = 1,
    just = c("center", "top")))

grid.text(
  "Resources Allocated to Location 2",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(p_sbar, vp = viewport(layout.pos.row = 1, layout.pos.col = 5))

print(p_pred2, vp = viewport(layout.pos.row = 1, layout.pos.col = 6))

grid.text(
  "Predictive Density",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 2, layout.pos.col = 3))
print(p_pred1, vp = viewport(layout.pos.row = 2, layout.pos.col = 4:5))

dev.off()
