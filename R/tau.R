#' Confidence intervals for variants of Kendall's tau
#'
#' @param x,y numeric vectors of data values. x and y must have the same length.
#' @param variant Which variant to return.
#' @param alpha confidence level for the returned confidence interval.
#' @param alternative indicates the alternative hypothesis and must be one of
#'    "two_sided", "greater" or "less". You can specify just the initial letter.
#'    "greater" corresponds to positive association, "less" to negative
#'    association.
#' @param method Method to used to calculate the confidence interval. Currently
#'    only "BCa", the accelerated bootstrap, is supportLed.
#' @param n_boot Number of bootstrap samples
#' @export
#' @return An interval containing the confidence limits.
#'
tau_ci <- \(x, y,
            variant = c("tau", "tau_strict", "tau_a", "tau_b", "tau_c", "somers_d", "gk_gamma", "wilsons_e"),
            alpha = 0.05,
            alternative = c("two_sided", "greater", "less"),
            method = "BCa",
            n_boot = 1000) {
  variant <- get(match.arg(variant))
  alternative <- match.arg(alternative)

  n <- length(x)
  theta_hat <- variant(x, y)

  theta_star <- replicate(n_boot, {
    samples <- sample(x = seq_along(x), size = length(x), replace = TRUE)
    variant(x[samples], y[samples])
  })

  loo <- theta_hat - vapply(1:length(x), function(i) variant(x[-i], y[-i]), 0.1)
  a <- 1 / 6 * sum(loo^3) / sum(loo^2)^(3 / 2)

  u <- if (alternative == "two_sided") {
    c(alpha / 2, 1 - alpha / 2)
  } else if (alternative == "greater") {
    alpha
  } else if (alternative == "less") {
    1 - alpha
  }

  z0 <- stats::qnorm(mean(theta_star <= theta_hat))
  zu <- stats::qnorm(u)
  u_adjusted <- stats::pnorm(z0 + (z0 + zu) / (1 - a * (z0 + zu)))

  limits <- stats::setNames(stats::quantile(theta_star, u_adjusted), u)

  if (alternative == "greater") {
    c(limits, Inf)
  } else if (alternative == "less") {
    c(-Inf, limits)
  } else {
    limits
  }
}

#' Estimate variants of Kendall's tau for tied data
#' @param x Covariate data.
#' @param y Response data.
#' @return Kendall's tau.
#' @name tau
#' @export
NULL


#' @describeIn tau Alternative weakly monotone PPR tau (Moss, 2024)
#' @export
tau_alt_weak <- \(x, y) {
  obj <- tau_data(x, y)
  numer <- obj$n_c - obj$n_d + obj$t_x * ((obj$n_d > obj$n_c) - (obj$n_d < obj$n_c))
  denom <- obj$n_c + obj$n_d + obj$t_x
  numer / denom
}

#' @describeIn tau Alternative strictly monotone PPR tau (Moss, 2024)
#' @export
tau_alt_stric <- \(x, y) {
  obj <- tau_data(x, y)
  numer <- obj$n_c - obj$n_d + obj$t_x * ((obj$n_d > obj$n_c) - (obj$n_d < obj$n_c))
  denom <- obj$n_c + obj$n_d + obj$t_x
  numer / denom
  stop("Not implemented.")
}

#' @describeIn tau Weakly monotone PPR tau (Moss, 2024)
#' @export
tau <- \(x, y) {
  obj <- tau_data(x, y)
  inc <- ((obj$n_d > obj$n_c) - (obj$n_d < obj$n_c))
  numer <- obj$n_c - obj$n_d + inc * (obj$t_x - 2*obj$t_xy - 2*obj$t_y)
  denom <- obj$N + obj$t_y + obj$t_xy
  numer / denom
}

#' @describeIn tau Strictly monotone PPR tau (Moss, 2024)
#' @export
tau_strict <- \(x, y) {
  obj <- tau_data(x, y)
  inc <- ((obj$n_d > obj$n_c) - (obj$n_d < obj$n_c))
  numer <- obj$n_c - obj$n_d + inc * (obj$t_x - 2*obj$t_xy)
  denom <- obj$N + obj$t_y + obj$t_xy
  numer / denom
}

#' @describeIn tau Original Kendall's tau (Kendall, 1938)
#' @export
tau_a <- \(x, y) {
  obj <- tau_data(x, y)
  (obj$n_c - obj$n_d) / obj$N
}

#' @describeIn tau Kendall's tau b (Kendall, 1945)
#' @export
tau_b <- \(x, y) {
  obj <- tau_data(x, y)
  numer <- obj$n_c - obj$n_d
  denom <- (obj$n_c + obj$n_d + obj$t_x) * (obj$n_c + obj$n_d + obj$t_y)
  numer / sqrt(denom)
}

#' @describeIn tau Stuart's tau (Stuart, 1953)
#' @export
tau_c <- \(x, y) {
  obj <- tau_data(x, y)
  n <- length(x)
  r <- length(unique(x))
  c <- length(unique(y))
  numer <- obj$n_c - obj$n_d
  denom <- n^2 * (min(r, c) - 1) / min(r, c)
  numer / denom
}

#' @describeIn tau Somers' D (Somers, 1962)
#' @export
somers_d <- \(x, y) {
  obj <- tau_data(x, y)
  numer <- obj$n_c - obj$n_d
  denom <- obj$n_c + obj$n_d + obj$t_x
  numer / denom
}

#' @describeIn tau Wilson's e (Wilson, 1974)
#' @export
wilsons_e <- \(x, y) {
  obj <- tau_data(x, y)
  numer <- obj$n_c - obj$n_d
  denom <- obj$n_c + obj$n_d + obj$t_y + obj$t_x
  numer / denom
}

#' @describeIn tau Goodman-Kruskall gamma (Goodman, Kruskall, 1954)
#' @export
gk_gamma <- \(x, y) {
  obj <- tau_data(x, y)
  numer <- obj$n_c - obj$n_d
  denom <- obj$n_c + obj$n_d
  numer / denom
}

#' @describeIn tau Leik-Gove D (Leik, Gove, 1969)
#' @export
lg_d <- \(x, y) {
  obj <- tau_data(x, y)
  numer <- obj$n_c - obj$n_d
  denom <- obj$n_c + obj$n_d + 2 * obj$t_x
  numer / denom
}
