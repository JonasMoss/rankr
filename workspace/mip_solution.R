x = 1:10
y = rank(c(1, 1, 2, 1, 3, 4, 3, 2, 6, 3), ties.method = "min")
#x = 1:5
#y = c(1, 1, 2, 1, 3)
#mon(x, y)[1, ]

## Comparison.
# set.seed(313)
# n = 20
# x = sort(sample(1:100, n, replace = FALSE))
# x0 = 33
# x1 = 66
# f = function(x) 0 * (x < x0) + (x - x0) * (x >= x0 & x < x1) + (x1) * (x >= x1)
# y = f(x) + rbinom(n, 1, 0.7)


minimizer = function(x, y) {

  n = length(x)
  m = n * (n - 1) / 2

  ys = combn(y, 2)
  u = (ys[1, ] > ys[2, ]) * -1 +
    (ys[1, ] == ys[2, ]) * 0 +
    (ys[1, ] < ys[2, ]) * 1

  obj = (-1 + 2*(u != 1))
  mat = get_constraints(x, y)
  dir = rep("<=", 2 * m)
  rhs = rep(0, 2 * m)
  types = rep("B", 2 * m)

}

get_constraints = function(n) {
  # Calculates the index of w_(i)(i+1)
  g = function(i) {
    s = 0.5 * (-(i - 2)^2 + 2 * (i - 2) * (n - 1) - (i - 2) + 2 * (n - 1))
    s + 1
  }

  # Calculates the index of w_ij
  f = function(i, j) {
    s = 0.5 * (-(i - 2)^2 + 2 * (i - 2) * (n - 1) - (i - 2) + 2 * (n - 1))
    s + (j - i)
  }


  m = n * (n - 1) / 2
  mat = matrix(rep(0, m * m * 2), ncol = m)
  combns = combn(1:n, 2)

  zeros = sapply(seq(n - 1), g)

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

ys = combn(y, 2)
u = (ys[1, ] > ys[2, ]) * -1 + (ys[1, ] == ys[2, ]) * 0 + (ys[1, ] < ys[2, ]) * 1

# objective vector.
obj = (-1 + 2*(u != 1))

##
## Experiment
##

f = function(i, j, n) {
  s = 0.5 * (-(i - 2)^2 + 2 * (i - 2) * (n - 1) - (i - 2) + 2 * (n - 1))
  s + (j - i)
}

## List of elementary pair indices
n = length(x)
g = function(i) {
  s = 0.5 * (-(i - 2)^2 + 2 * (i - 2) * (n - 1) - (i - 2) + 2 * (n - 1))
  s + 1
}

sapply(seq(n - 1), function(i) f(i, i + 1, n))
sapply(seq(n - 1), g)

m = n * (n - 1) / 2

make_rows = function(i, j) {
  indices = g(seq.int(from = i, to = j - 1, by = 1))

  row_one = rep(0, m)
  row_one[indices] = -1
  row_one[f(i, j, n)] = 1

  row_two = rep(0, m)
  row_two[indices] = 1/(j - i)
  row_two[f(i, j, n)] = -1
  rbind(row_one, row_two)
}

mat = matrix(rep(0, m * m * 2), ncol = m)
combns = combn(1:n, 2)

zeros = sapply(seq(n - 1), g)

'%!in%' <- function(x,y)!('%in%'(x,y))

for (index in seq.int(1, m)) {

  if (index %!in% zeros) {
    i = combns[, index][1]
    j = combns[, index][2]
    indices = g(seq.int(from = i, to = j - 1, by = 1))

    row_one = rep(0, m)
    row_one[indices] = -1
    row_one[f(i, j, n)] = 1

    row_two = rep(0, m)
    row_two[indices] = 1/(j - i)
    row_two[f(i, j, n)] = -1

    mat[index, ] = row_one
    mat[m + index, ] = row_two
  }

}


dir = rep("<=", 2 * m)
rhs = rep(0, 2 * m)
types = rep("B", 2 * m)

sol = Rglpk::Rglpk_solve_LP(obj, mat, dir, rhs, types = types)
sol$solution[zeros]
rank(c(0,cumsum(sol$solution[zeros])), ties.method = "min")
mon(x, y)

plot(x, y)
points(x, rank(c(0,cumsum(sol$solution[zeros])), ties.method = "min"), type = "b", col = "blue")
