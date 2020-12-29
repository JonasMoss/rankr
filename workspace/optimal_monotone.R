## Comparison.
set.seed(313)
n = 20
x = sort(sample(1:100, n, replace = TRUE))
x0 = 33
x1 = 66
f = function(x) 0 * (x < x0) + (x - x0) * (x >= x0 & x < x1) + (x1) * (x >= x1)
#f = function(x) 0 * (x < x0) + x0 * (x >= x0 & x < x1) + x1 * (x > x1)
y = f(x) + rbinom(n, 1, 0.7)
#y = f(x)
x = rank(x, ties.method = "min")
y = rank(y, ties.method = "min")


x = 1:10
#x[6] = 5
y = c(1, 1, 2, 1, 3, 4, 3, 2, 6, 3)
z = c(1, 1, 1, 1, 2, 3, 4, 5, 5, 5)

x = 1:9
y = c(rep(1,3), rep(3, 3), rep(2, 3))
x = rank(x, ties.method = "min")
y = rank(y, ties.method = "min")

plot(x, y)
points(x, mon(x, y)[1, ], col = "red", type = "b", lwd = 2)


#' Optimal monotone function
#'
#' @param x,y Input data.
#' @param l The loss. Defaults to absolute value loss.
#' @return A matrix whose rows are the optimal fs.
mon = function(x, y, l = abs) {

  args = lapply(seq(length(x) - 1), function(x) c(0, 1))

  rep_to_z = function(rep) {

    z = x

    for (i in 2:length(x)) if (rep[i - 1] == 1) z[i] = z[i - 1]

    z

  }

  candidates = apply(do.call(expand.grid, args), 1, rep_to_z)

  result = sapply(seq(ncol(candidates)),
                  function(i) risk(candidates[ , i], y, l = l))

  solutions = t(candidates[, result == max(result)])
  solutions

}

#' The risk
#'
#' @param x,y Input data.
#' @param l The loss. Defaults to absolute value loss.
#' @return The risk.

risk = function(x, y, l = abs) {

  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))

  v = xs[1, ] > xs[2, ]
  u = ys[1, ] > ys[2, ]

  1 - mean(l(u - v)) / mean(l(u - 1))

}
