#' Confidence intervals for variants of Kendall's tau
#'
#' @param x,y numeric vectors of data values. x and y must have the same length.
#' @param n_boot number of bootstrap samples
#' @param alpha confidence level for the returned confidence interval.
#' @param alternative indicates the alternative hypothesis and must be one of
#'    "two_sided", "greater" or "less". You can specify just the initial letter.
#'    "greater" corresponds to positive association, "less" to negative
#'    association.
#' @param method Method to used to calculate the confidence interval. Currently
#'    only "BCa", the accelerated bootstrap, is supported.
#' @param variant
#' @return An interval containing the confidence limits.
#'
tau_ci = function(x, y, n_boot = 1000, alternative = c("two_sided", "greater", "less"), alpha = 0.05, method = "BCa",
                  variant = c("tau_d", "tau_a", "tau_b", "tau_c", "somers_d", "gk_gamma")) {

  variant = get(match.arg(variant))
  alternative = match.arg(alternative)

  n = length(x)
  theta_hat = variant(x, y)

  theta_star = replicate(n_boot, {
    samples = sample(x = seq_along(x), size = length(x), replace = TRUE)
    variant(x[samples], y[samples])
  })

  loo = theta_hat - vapply(1:length(x), function(i) variant(x[-i], y[-i]), 0.1)
  a = 1 / 6 * sum(loo ^ 3) / sum(loo ^ 2) ^ (3 / 2)

  u = if(alternative == "two_sided") {
    c(alpha / 2, 1 - alpha / 2)
  } else if (alternative == "greater") {
    alpha
  } else if (alternative == "less") {
    1 - alpha
  }

  z0 = qnorm(mean(theta_star <= theta_hat))
  zu = qnorm(u)
  u_adjusted = pnorm(z0 + (z0 + zu) / (1 - a * (z0 + zu)))

  limits = stats::setNames(quantile(theta_star, u_adjusted), u)

  if (alternative == "greater") {
    c(limits, Inf)
  } else if (alternative == "less") {
    c(-Inf, limits)
  } else {
    limits
  }

}
