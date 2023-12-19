library(tidyverse)
library(patchwork)
library(devtools)
library(alloscore)
source("random_undocumented_experiments/iter_pic_funs.R")

make_iter_pic(K = 2,
              alpha = c(.2,.6,.3,.9,.5),
              w = c(3,2,3,5,2),
              dists_and_params = list(
                list(dist = "norm", mean = 3, sd = 1),
                list(dist = "norm", mean = 2, sd = 4),
                list(dist = "exp", rate = .5),
                list(dist = "gamma", shape = 1, rate = .3),
                list(dist = "unif", min = 5, max = 6)
                #list(dist = "distfromq", ps = c(.1,.3,.6,.9,.95), qs = c(2,3,3,4,5))
              ))

make_iter_pic(K = 20,
              dists_and_params = list(
                list(dist = "unif", min = 0, max = 7),
                list(dist = "unif", min = 7, max = 15)
              ))

make_iter_pic(K = 2,
              dists_and_params = list(
                list(dist = "exp", rate = 1),
                list(dist = "exp", rate = 1/4)
              ))

dat <- make_param_df(dists_and_params = list(
  list(dist = "unif", min = 0, max = 7),
  list(dist = "unif", min = 7, max = 15)
))

dat_allo <- dat %>% allocate(K=20)

  # Plot MEBs
# with(dat,
#      {p <- ggplot() + xlim(-1, 10) +
#          map(1:nrow(dat), ~ geom_function(aes(color = name[[.]]), fun = Lambda[[.]]))
#      p}
# )

plotdat <- function(dat) {
  xl = 0
  xr = 10
  plt_dat <- dat
  p <- ggplot(data = data.frame(
    x = seq(from = xl, to = xr, length.out = 1000)
  ))
  # p <- p + map(1:nrow(plt_dat), function(i) geom_line(
  #   aes(x = x, y = plt_dat$F[[i]](x), color = plt_dat$name[[i]]), linetype = 5))
  # p_lam <- p + map(1:nrow(plt_dat), function(i) geom_line(
  #   aes(x = x, y = plt_dat$Lambda[[i]](x), color = plt_dat$name[[i]])))
  #p_lam <- p_lam+ylim(c(-1,1))
  p_loss <- p + map(1:nrow(plt_dat), function(i) geom_line(
    aes(x = x, y = plt_dat$Eloss[[i]](x), color = plt_dat$name[[i]])))
  p_loss
  #p_lam + p_loss + plot_layout(ncol=1)
}

plotdat(dat)
