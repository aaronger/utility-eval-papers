

# make function of lambda that gives total resource use at "lambda-quantiles"
# which are regular quantiles when lambda = 0.
# When `!is.null(forecasts))`, the total use is for the subset of rows given by
# vector `forecasts`
K_fun_factory <- function(df, forecasts = NULL) {
  if (!is.null(forecasts)) df <- df[forecasts,]
  Vectorize(with(df, function(lambda) {
    alpha_lam <- alpha-w*lambda
    positives <- alpha_lam>0
    qs_plus <- pmax(0, map2_dbl(Q, alpha_lam*positives, exec))
    qs <- qs_plus*positives
    sum(w * qs)
  }))
}


#' Plot the iterations of `allocate` relative to meb's and total resource use
#'
#' @param K
#' @param alpha
#' @param w
#' @param dists_and_params list of named lists each containing a `dist` entry indicating
#'  distribution family (e.g. "norm" or "distfromq") and entries for the required parameters
#'  (e.g. "mean" and "sd" or "ps" and "qs)
#'
#' @return
#' @export
#'
#' @examples
make_iter_pic <- function(K, alpha = 1, w = 1, dists_and_params) {
  dat <- make_param_df(alpha = alpha, w = w, dists_and_params = dists_and_params)
  dat_allo <- dat %>% allocate(K = K)
  ymax = max(map2_dbl(dat$Lambda,0,exec))*(1.1)
  xmax = max(dat_allo$xs[[1]][,2])*1.1
  n = nrow(dat_allo$xdf[[1]])
  K_fun <- K_fun_factory(dat)
  with(dat_allo,
       {
         p1 <- ggplot() + xlim(c(0,xmax)) + ylim(0, ymax) +
           geom_hline(yintercept = 0) +
           map(lam_seq[[1]], ~ geom_hline(yintercept = ., alpha = .3)) +
           geom_label(data = enframe(lam_seq[[1]]), aes(x=0,y=value,label=name), label.size = .05) +
           map(1:n, ~ geom_function(aes(color = dat$name[[.]]), fun = dat$Lambda[[.]])) +
           map(1:n, ~ geom_vline(aes(xintercept = x[[1]][.], color = dat$name[[.]]), alpha=.5)) +
           labs(color = "Distribution",
                y = expression(1-tau),
                x = expression(x[i])) +
           scale_x_continuous(breaks = seq(0, xmax, by = 1)) +
           theme_classic() +
           theme(legend.position = "left")

         p2 <- ggplot(data = tibble(y = seq(0, 1.01, length.out = 1000), x = K_fun(y)),
                      aes(x = x, y = y)) +
           ylim(0, ymax) + xlim(0, K_fun(0.01)) +
           geom_line() +
           geom_label(
             data = enframe(lam_seq[[1]]) %>%
               mutate(x = K_fun(value)), aes(x = x, y = value, label = name)) +
           # geom_line(aes(x =  K_funfac(forecasts = c(2, 4))(y)), color = "blue") +
           # #geom_line(aes(x =  K_funfac(forecasts = c(1,2,4))(y))) +
           #map(1:n, ~ geom_line(aes(x = K_funfac(forecasts = c(.))(y), color = name[[.]]))) +
           geom_vline(xintercept = K) +
           labs(x =expression(sum( q[tau * ", " * F[i]],i == 1, N))
                ) +
           theme(
             axis.text.y = element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank()
           )+
           theme(legend.position = "none")

         p1+p2
       })
}

make_param_df <- function(alpha = 1, w = 1, dists_and_params) {
  dat <- tibble(
    alpha = alpha,
    w = w,
    dists_and_params = dists_and_params,
    name = map(dists_and_params,
               ~toString(paste0(.[["dist"]], ", ",
                                toString(paste(names(.)[-1], .[-1], sep = "=")))))) %>%
    unnest_wider(col = dists_and_params) %>%
    add_pdqr_funs(types = c("p", "q")) %>% mutate(
      q_scale = map2_dbl(Q, alpha, exec),
      # add marginal expected benefit functions scaled to fit on single [0,1] interval,
      Lambda = pmap(.[c("alpha", "F", "w")],
                    function(alpha, F, w) {dexp_gpl_loss(alpha = alpha, F = F, kappa = -1/w)}),
      Eloss = pmap(.[c("alpha", "F")], exp_gpl_loss_fun)
    )
  dat
}
