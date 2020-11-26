set.seed(313)


tau_d_ci = function(x, y, n_boot = 1000, alpha = 0.05) {
  
  n = length(x)
  
  theta_star = replicate(n_boot, {
    samples = sample(x = seq_along(x), size = length(x), replace = TRUE)
    tau_d(x[samples], y[samples])
  })
  
  loo = tau_d(x, y) - vapply(1:length(x), function(i) tau_d(x[-i], y[-i]), 0.1)
  a = 1 / 6 * sum(loo ^ 3) / sum(loo ^ 2) ^ (3 / 2)
  
  u = c(alpha / 2, 1 - alpha / 2) 
  z0 = qnorm(mean(theta_star <= theta_hat))
  zu = qnorm(u)
  u_adjusted = pnorm(z0 + (z0 + zu) / (1 - a * (z0 + zu))) 
  
  stats::setNames(quantile(theta_star, u_adjusted), u)
  
}

result = replicate(10000, {
  x = runif(6)
  y = runif(6)
  tau_d(x, y)
})



n = 100
k = 4 * 6
p = rexp(k)
p = p / sum(pi)
x = as.table(matrix(rmultinom(1, 1, p), nrow = 4))
vec <- rep(names(x), x)


x = c(0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3)
y = c(0, 1, 1, 1, 0, 2, 3, 3, 4, 5, 5, 2)

mat = MASS::mvrnorm(
  n = 100, 
  mu = c(0, 0), 
  Sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))

x = mat[, 1]
y = mat[, 2]
n = length(x)
tau = 2 / pi * asin(0.5)


n = 50000
x = runif(n)
x = sample(1:100, n, replace = TRUE)/100
y = x^2 * (x < 0.5) + 0.25 * (x >= 0.5) 
y = rank(y) + sample(-1:1, n, replace = TRUE)
tau = tau_d(x, y)

result = replicate(1000, {
  n = 50
  x = runif(n)
  x = sample(1:100, n, replace = TRUE)/100
  y = x^2 * (x < 0.5) + 0.25 * (x >= 0.5) + runif(n, 0, 0.01)
  y = x^2 * (x < 0.5) + 0.25 * (x >= 0.5)
  y = rank(y) + sample(-1:1, n, replace = TRUE)
  
  alpha = 0.05
  theta_hat = tau_d(x, y)
  theta_boot = replicate(1000, {
    samples = sample(1:length(x), length(x), replace = TRUE)
    tau_d(x[samples], y[samples])
  })
  
  tau_is = sapply(1:length(x), function(i) {
    tau_d(x[-i], y[-i])
  })
  
  is = (n - 1) * (tau_d(x, y) - tau_is)
  
  #Desired quantiles
  u <- c(alpha/2, 1-alpha/2) 
  
  #Compute constants
  z0 <- qnorm(mean(theta_boot <= theta_hat))
  zu <- qnorm(u)
  a <- 1 / 6 * sum(is ^ 3) / sum(is ^2)^(3/2)
  
  #Adjusted quantiles
  u_adjusted <- pnorm(z0 + (z0+zu)/(1-a*(z0+zu))) 
  
  #Accelerated Bootstrap CI
  quants = quantile(theta_boot, u_adjusted)  
  tau > quants[1] & tau < quants[2]
})
