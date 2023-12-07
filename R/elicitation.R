# Analysis to support supplemental section on use of distfromq

library(tidyverse)
library(distfromq)
library(alloscore)


obj_alloc <- function(par, F, Q, ps, K) {
  n_ps <- length(ps)
  n_locs <- length(par) / n_ps
  dim(par) <- c(n_ps, n_locs)
  
  fq_df <- tibble(
    ps = lapply(seq_len(n_locs), function(i) ps),
    qs = lapply(seq_len(n_locs), function(i) par[, i])
  ) |>
    add_pdqr_funs(dist = "distfromq", types = c("p", "q"))
  
  alloc <- alloscore::allocate(K = K, F = fq_df$F, Q = fq_df$Q)$x[[1]]
  
  return(alloc)
}


#' objective
objective <- function(par, F, Q, ps, K, target_alloc) {
  alloc <- obj_alloc(par, F, Q, ps, K)
  return(mean((alloc - target_alloc)^2))
}


#' Compute functionals theta that would be submitted by a forecaster with
#' specified marginal distributions who knew that distfromq + alloscore would
#' be used for forecast evaluation.
#'
#' @param F a list of length N = number of locations containing functions to
#' evaluate the marginal forecast cdfs in each location
#' @param Q a list of length N = number of locations containing functions to
#' evaluate the marginal forecast quantile functions in each location
#' @param ps a vector of target quantile levels specified by the hub
#' @param K constraint value to use for allocation
compute_submission_params <- function(F, Q, ps, K, target_alloc) {
  # # compute the allocation based on F
  # target_alloc <- alloscore::allocate(K = K, F = F, Q = Q)$x[[1]]
  
  # initialize at quantiles of the marginal distributions
  par_init <- lapply(Q, function(Q_i) Q_i(ps)) |>
    unlist()
  
  optim_result <- optim(par=par_init, fn=objective,
                        F=F, Q=Q, ps=ps, K=K, target_alloc=target_alloc,
                        method="L-BFGS-B",
                        lower = rep(0, length(par_init)))

  return(list(
    quantiles = par_init,
    target_alloc = target_alloc,
    theta = optim_result$par,
    optimized_alloc = obj_alloc(optim_result$par, F=F, Q=Q, ps=ps, K=K),
    optim_result = optim_result
  ))
}


# Exponential marginal distributions
F1 <- function(q) {
  pexp(q, rate = 1)
}
F2 <- function(q) {
  pexp(q, rate = 1/4)
}
F <- list(F1, F2)

Q1 <- function(p) {
  qexp(p, rate = 1/1)
}
Q2 <- function(p) {
  qexp(p, rate = 1/4)
}
Q <- list(Q1, Q2)

ps <- c(0.25, 0.5, 0.75)

exp_target_alloc <- function(sigma, K) {
  inv_sigma <- 1 / sigma
  scale <- inv_sigma / sum(inv_sigma)
  return(scale * K)
}

exp_submission_params <- purrr::map_dfr(
  c(5, 10, 100),
  function(K) {
    compute_submission_params(
      F=F, Q=Q, ps=ps, K=K,
      target_alloc = exp_target_alloc(c(1, 1/4), K=K)) |>
      lapply(list) |>
      as.tibble() |>
      dplyr::mutate(K = K)
  }) |>
  dplyr::mutate(
    max_abs_q_diff = map2_dbl(quantiles, theta,
                              function(q, t) { max(abs(q - t)) }),
    alloc_sq_error = map2_dbl(target_alloc, optimized_alloc,
                             function(t, o) { mean((t - o)^2) })
  )

exp_submission_params



# lognormal marginal distributions
taus <- c(pexp(1), pexp(2), pexp(20))

# curve(dlnorm(x, meanlog = 0, sdlog = 1), 0, 40, col='red')
# curve(dlnorm(x, meanlog = 3, sdlog = 1.4), 0, 40, add=TRUE)

F1 <- function(q) {
  plnorm(q, meanlog = 0, sdlog = 1)
}
F2 <- function(q) {
  plnorm(q, meanlog = 3, sdlog = 1.4)
}
F <- list(F1, F2)

Q1 <- function(p) {
  qlnorm(p, meanlog = 0, sdlog = 1)
}
Q2 <- function(p) {
  qlnorm(p, meanlog = 3, sdlog = 1.4)
}
Q <- list(Q1, Q2)

Ks <- map_dbl(taus, function(tau) { Q1(tau) + Q2(tau) })

ps <- c(0.25, 0.5, 0.75)

lnorm_submission_params <- purrr::map_dfr(
  Ks,
  function(K) {
    compute_submission_params(
      F=F, Q=Q, ps=ps, K=K,
      target_alloc = exp_target_alloc(c(1, 1/4), K=K)) |>
      lapply(list) |>
      as.tibble() |>
      dplyr::mutate(K = K)
  }) |>
  dplyr::mutate(
    max_abs_q_diff = map2_dbl(quantiles, theta,
                              function(q, t) { max(abs(q - t)) }),
    alloc_sq_error = map2_dbl(target_alloc, optimized_alloc,
                             function(t, o) { mean((t - o)^2) })
  )

lnorm_submission_params
