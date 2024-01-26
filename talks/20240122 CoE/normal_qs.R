library(ggplot2)
library(grid)

x_grid_1 <- seq(from = 0, to = 5, length = 101)

q_75 <- 5
sd <- 1.5
mu <- q_75 - sd * qnorm(0.75)

region_to_shade1 <- data.frame(
  x = c(0, x_grid_1, 5),
  y = c(0, dnorm(x_grid_1, mean = mu, sd = sd, log = FALSE), 0)
)

p1 <- ggplot(data = data.frame(x = c(0, 10)), mapping = aes(x = x)) +
  geom_polygon(
    mapping = aes(x = x, y = y),
    fill = "orange",
    data = region_to_shade1) +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sd), n = 201) +
  geom_vline(xintercept = 5) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 0.3), expand = FALSE) +
  theme_bw() +
  xlab("y") +
  ylab("Forecast\ndensity") +
  theme_bw(base_size = 20)
  # ggtitle(expression(paste("Shaded area is ", P(t <= -2))))



p2 <- ggplot(data = data.frame(x = c(0, 10)), mapping = aes(x = x)) +
  stat_function(fun = pnorm, args = list(mean = mu, sd = sd), n = 201) +
  geom_path(
    data = data.frame(
        x = c(0, 5, 5),
        tau = c(0.75, 0.75, 0)
      ),
    mapping = aes(x = x, y = tau),
    color = "orange"
  ) +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1), expand = FALSE) +
  theme_bw() +
  xlab("y") +
  ylab("Forecast\nCDF") +
  theme_bw(base_size = 20)

pdf("talks/20240122 CoE/norm_pdf_q.pdf", width = 4, height = 2)
print(p1)
dev.off()

pdf("talks/20240122 CoE/norm_cdf_q.pdf", width = 4, height = 2)
print(p2)
dev.off()
