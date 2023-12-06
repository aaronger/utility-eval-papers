# Analysis to support supplemental section on use of distfromq

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
compute_submission_params <- function(F, Q, ps, K) {
  # compute the allocation based on F
  target_alloc <- alloscore::allocate(K = K, F = F, Q = Q)$x[[1]]
  
  # initialize at quantiles of the marginal distributions
  par_init <- lapply(Q, function(Q_i) Q_i(ps)) |>
    unlist()
  
  obj_alloc <- function(par) {
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
  objective <- function(par) {
    alloc <- obj_alloc(par)
    return(mean((alloc - target_alloc)^2))
  }
  
  optim_result <- optim(par=par_init, fn=objective,
                        method="L-BFGS-B",
                        lower = rep(0, length(par_init)))
  
  return(list(
    quantiles = par_init,
    target_alloc = target_alloc,
    theta = optim_result$par,
    optimized_alloc = obj_alloc((optim_result$par)),
    optim_result = optim_result
  ))
}

library(tidyverse) # seems to be necessary to avoid warnings like 'could not find function "%>%"' and 'could not find function "set_names"'?
library(alloscore)

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

submission_params_K5 <- compute_submission_params(
  F, Q, ps=c(0.25, 0.5, 0.75), K=5)

submission_params_K10 <- compute_submission_params(
  F, Q, ps=c(0.25, 0.5, 0.75), K=10)

submission_params_K1000 <- compute_submission_params(
  F, Q, ps=c(0.25, 0.5, 0.75), K=1000)

submission_params_K5[1:4]
submission_params_K10[1:4]
submission_params_K1000[1:4]
