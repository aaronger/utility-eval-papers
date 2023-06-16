
make_crps_fun <- function(...) {
  l <- list(...)
  params <- setdiff(names(formals(paste0("d",l$dist))), c("x", "log"))
  l$family <- l$dist
  l <- l[c("family", params)]
  function(y) do.call(crps, c(y,l))
}

add_crps_funs <- function(df) {
  df %>% mutate(crps = pmap(., make_crps_fun))
}

mean_crps <- function(crps_fns, y_vec, w = rep(1, length(y_vec))) {
  map2_vec(crps_fns, y_vec, function(fn, y) fn(y)) %>% weighted.mean(w = w)
}

mean_crps_samp <- function(crps_fns, y_vec_samp) {
  map_vec(y_vec_samp, ~mean_crps(crps_fns, y_vec = .)) %>%
    enframe(name = "samp", value = "mean_crps")
}

slim_xdf_check <- function(df) {
  df %>%
    mutate(s = pmax(y-x,0) - components_raw, o = pmax(y - oracle, 0) - components_oracle)
}
