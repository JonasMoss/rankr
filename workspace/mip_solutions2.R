x = 1:10
#x[6] = 5
#x[2] = 1
#x[3] = 1
x = rank(x, ties.method = "min")
y = rank(c(1, 1, 2, 1, 3, 4, 3, 2, 6, 3), ties.method = "min")
y = rank(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4), ties.method = "min")
#y = rank(c(1, 2, 1, 1, 1, 4, 3, 2, 6, 3), ties.method = "min")
#y = rank(c(1, 2, 1, 1, 1, 6, 6, 6, 5, 6), ties.method = "min")
#y = rank(1:10, ties.method = "min")


risk(isoreg(x, y)$yf, y)
tau_e(x, y)


x = 1:5
y = c(1, 1, 2, 3, 2)


attempt = function(x, y) {


  f = function(i, j) {
    s = 0.5 * (-(i - 2)^2 + 2 * (i - 2) * (n - 1) - (i - 2) + 2 * (n - 1))
    s + (j - i)
  }

  n = length(x)
  ys = combn(y, 2)
  u = -(ys[1, ] < ys[2, ]) + (ys[1, ] == ys[2, ])

  z = rep(0, n - 1)
  v = 1 * (ys[1, ] < ys[2, ])

  indices = 1:(n - 1)

  for (i in indices) {

    w1 = v
    w2 = v
    w1[f(i, (i + 1):n)] = 1
    # Second modifictation.
    w2[f(i, (i + 1):n)] = 0

    if(sum(u * w1) < sum(u * w2)) {
      v = w1
      z[i] = 1
    } else {
      v = w2
      z[i] = 0
    }

  }

  rank(c(0,cumsum(z)), ties.method = "min")

}


#>   sol$solution[zeros]
#[1] 0 0 1 0 0 1 0 0 1




n = 20
x = sort(sample(1:3, n, replace = TRUE))
y = 2 * x + rbinom(n, 10, 0.5)


set.seed(313)
n = 20
x = sort(rnorm(n))
y = rnorm(n) + x

rankr::tau_a(x, y)
tau_e(x, y)
tau_e(y, x)
rankr::tau_d(x, y)
rankr::somers_d(x, y)
risk(minimizer(x, y) , y)
risk(minimizer(sort(y), x[order(y)]) , x[order(y)])

plot(rank(x, ties.method = "min"), rank(y, ties.method = "min"))
points(rank(x, ties.method = "min"), minimizer(x, y), col = "blue", type = "b")

#' The risk
#'
#' Only works if P(Y_1>Y_2) > P(Y_1 == Y_2).
#'
#' @param x,y Input data.
#' @param l The loss. Defaults to the 0-1 loss.
#' @return The risk.

risk = function(x, y, l = function(x) x != 0) {

  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))

  v = (xs[1, ] > xs[2, ]) * -1 +
    (xs[1, ] == xs[2, ]) * 0 +
    (xs[1, ] < xs[2, ]) * 1

  u = (ys[1, ] > ys[2, ]) * -1 +
    (ys[1, ] == ys[2, ]) * 0 +
    (ys[1, ] < ys[2, ]) * 1

  1 - mean(l(u - v)) / mean(l(u - 1))

}

#' Indices of tied values.
#'
#' Vector of indices `i` so that `x[i] = x[i + 1]`.
#' @param x Vector of data.
#' @return Indices of tied data.

ties = function(x) {

  vec = c()

  for (i in 1:(length(x) - 1)) if (x[i] == x[i + 1]) vec = c(vec, i)

  vec

}

#' Calculate the minimizer.
#'
#' Assumes x is sorted.
#'
#' @param x,y Data
#' @return The minimizer.
minimizer = function(x, y) {

  n = length(x)
  m = n * (n - 1) / 2
  tied_data = ties(x)

  g = function(i) {
    s = 0.5 * (-(i - 2)^2 + 2 * (i - 2) * (n - 1) - (i - 2) + 2 * (n - 1))
    s + 1
  }

  # Calculates the index of w_ij
  f = function(i, j) {
    s = 0.5 * (-(i - 2)^2 + 2 * (i - 2) * (n - 1) - (i - 2) + 2 * (n - 1))
    s + (j - i)
  }

  get_constraints = function(n) {
    mat = matrix(rep(0, m * m * 2), ncol = m)
    combns = combn(1:n, 2)

    '%!in%' <- function(x,y)!('%in%'(x,y))

    for (index in seq.int(1, m)) {

      if (index %!in% zeros) {
        i = combns[, index][1]
        j = combns[, index][2]
        indices = g(seq.int(from = i, to = j - 1, by = 1))

        row_one = rep(0, m)
        row_one[indices] = -1
        row_one[f(i, j)] = 1

        row_two = rep(0, m)
        row_two[indices] = 1/(j - i)
        row_two[f(i, j)] = -1

        mat[index, ] = row_one
        mat[m + index, ] = row_two
      }

    }

    mat

  }

  ties_constraints = function(tied_data) {
    mat = matrix(0, nrow = length(tied_data), ncol = m)
    indices = g(tied_data)
    row = 1
    for (i in indices) {
      mat[row, i] = 1
      row = row + 1
    }
    mat
  }

  n = length(x)
  m = n * (n - 1) / 2
  zeros = sapply(seq(n - 1), g)

  ys = combn(y, 2)
  u = (ys[1, ] < ys[2, ]) * -1 + (ys[1, ] == ys[2, ]) * 1

  obj = -(ys[1, ] < ys[2, ]) + (ys[1, ] == ys[2, ])
  mat = rbind(get_constraints(n), ties_constraints(tied_data))
  dir = c(rep("<=", 2 * m), rep("==", length(tied_data)))
  rhs = rep(0, 2 * m + length(tied_data))
  types = rep("B", 2 * m)

  sol = Rglpk::Rglpk_solve_LP(obj, mat, dir, rhs, types = types)
  sol$solution[zeros]
  rank(c(0,cumsum(sol$solution[zeros])), ties.method = "min")

}

#' Tau E
#' @param x,y The data.
#' @return Tau E

tau_e = function(x, y) {
  x_ = sort(x)
  y_ = y[order(x)]
  z = minimizer(x_, y_)
  risk(z, y_)
}

