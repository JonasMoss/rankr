new_tau <- \(x, y)  {
  omega <- \(x) {
    if (x[1] > x[2]) {
      1
    } else if (x[2] > x[1]) {
      -1
    } else {
      0
    }
  }

  pairs <- \(x) {
    xx <- arrangements::combinations(x, 2)
    apply(rbind(xx, arrangements::combinations(rev(x), 2)), 1, omega)
  }

  xx <- pairs(x)
  yy <- pairs(y)

  l <- \(x, y) 1*(!(x == y || y == 0))

  above <- mean(sapply(seq_along(xx), \(i) l(xx[i], yy[i])))
  belowm <- mean(sapply(seq_along(xx), \(i) l(-1,yy[i])))
  #belowp <- mean(sapply(seq_along(xx), \(i) l(1,yy[i])))
  below0 <- mean(sapply(seq_along(xx), \(i) l(0,yy[i])))
  1 - above/belowm
}

new_tau2 <- \(x, y)  {
  omega <- \(x) {
    if (x[1] <= x[2]) {
      1
    } else {
      -1
    }
  }

  pairs <- \(x) {
    xx <- arrangements::combinations(x, 2)
    apply(rbind(xx, arrangements::combinations(rev(x), 2)), 1, omega)
  }

  xx <- pairs(x)
  yy <- pairs(y)

  l <- \(x, y) 1*(!(x == y))

  above <- mean(sapply(seq_along(xx), \(i) l(xx[i], yy[i])))
  belowm <- mean(sapply(seq_along(xx), \(i) l(-1,yy[i])))
  #belowp <- mean(sapply(seq_along(xx), \(i) l(1,yy[i])))
  1 - above/belowm
}

all <- \(x,y) {
  out <- c(new_tau(x, y),
           new_tau2(x,y),
           cor(x, y, method = "kendall"),
           DescTools::SomersDelta(x, y),
           DescTools::SomersDelta(y, x),
           DescTools::GoodmanKruskalGamma(x, y))
  names(out) <- c("new", "new_tau2","tau", "somer_xy", "somer_yx", "gamma")
  out
}


# New, Somers, and Goodman are equal here; are they always equal when x is
# without ties?
x <- sort(rnorm(10))
y <- c(rep(1,3), rep(2, 3), rep(3, 4)) + rbinom(10, 10, 0.1)
all(x,y)

# Now they aren't equal; x ain't monotone no more.
x <- c(rep(1,2), rep(2, 2), runif(4, 3, 4), rep(5, 2))
y <- c(rep(1,3), rep(2, 3), rep(3, 4)) + rbinom(10, 10, 0.1)
all(x,y)

gk_gamma(x,y)
somers_d(x,y)
tau_a(x, y)
tau_b(x, y)
tau_c(x, y)

# Harder non-monotonicity. Here Goodman fails, as the sequence is not monotone,
# but Goodman is still equal to 1. (Probably known in the lit.)
x <- c(rep(1,2), rep(2, 3), c(3, 4, 5, 5, 5))
y <- c(rep(1,3), rep(2, 3), rep(3, 4))
all(x,y)

# So here Goodman fails, as the sequence is not monotone, but Goodman is
# still equal to 1.

# Harder monotonicity.
x <- c(rep(1,3), rep(2, 3), c(3, 4, 5, 5))
y <- c(rep(1,3), rep(2, 3), rep(3, 4))

# Harder monotonicity (2)
x <- c(rep(1,3), rep(2, 3), c(3, 4, 4, 4))
y <- c(rep(1,3), c(2, 3, 3), c(3, 4, 4, 5))

x <- c(rep(1,3), rep(2, 3), c(5, 5, 5, 5))
y <- c(rep(1,3), rep(2, 3), c(3, 4, 5, 6))


plot(x,y)

new_tau(x, y)
cor(x, y, method = "kendall")
DescTools::SomersDelta(x, y)
DescTools::SomersDelta(y, x)
DescTools::GoodmanKruskalGamma(x, y)
