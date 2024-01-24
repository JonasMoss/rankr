tau_weak <- \(x,y) {
  obj <- tau_data(x, y)
  inc <- (obj$n_d > obj$n_c) - (obj$n_d < obj$n_c)
  numer <- obj$n_c - obj$n_d + (obj$t_x - 2*obj$t_xy - 2*obj$t_y)*inc
  denom <- obj$N + obj$t_y + obj$t_xy
  numer / denom
}

tau_strict <- \(x,y) {
  obj <- tau_data(x, y)
  inc <- (obj$n_d > obj$n_c) - (obj$n_d < obj$n_c)
  numer <- obj$n_c - obj$n_d + (obj$t_x - 2*obj$t_xy)*inc
  denom <- obj$N + obj$t_y + obj$t_xy
  numer / denom
}
