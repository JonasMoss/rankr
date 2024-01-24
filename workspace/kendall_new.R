
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
  #belowm <- mean(sapply(seq_along(xx), \(i) l(-1,yy[i])))
  #belowp <- mean(sapply(seq_along(xx), \(i) l(1,yy[i])))
  #below0 <- mean(sapply(seq_along(xx), \(i) l(0,yy[i])))

  below <- table(yy)[1]
  1 - above/below
}


new_tau2 <- \(x, y)  {
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

  l <- \(x, y) 1*(!(x == y))

  above <- mean(sapply(seq_along(xx), \(i) l(xx[i], yy[i])))
  obj <- tau_data(x, y)
  below <- table(yy)[1]
  1 - above/(obj$N + obj$t_y + obj$t_x)
}


counter <- \(x, y)  {
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
  c(numerator = sum(sapply(seq_along(xx), \(i) l(xx[i], yy[i]))),
    denom = sum(sapply(seq_along(xx), \(i) l(-1,yy[i]))))
}


new_tau3 <- \(x, y)  {
  omega <- \(x) {
    1 - 2 * (x[1] > x[2])
  }

  omega2 <- \(x) {
    if (x[1] > x[2]) {
      1
    } else if (x[2] > x[1]) {
      -1
    } else {
      0
    }
  }

  pairs <- \(x, f) {
    xx <- arrangements::combinations(x, 2)
    apply(rbind(xx, arrangements::combinations(rev(x), 2)), 1, f)
  }

  xx <- pairs(x, omega)
  yy <- pairs(y, omega)

  l <- \(x, y) 1*(!(x == y))
  ## l <- \(x, y) 1*(!(x == y) || x == 0 && y == 1)

  above <- mean(sapply(seq_along(xx), \(i) l(xx[i], yy[i])))
  below <- mean(sapply(seq_along(xx), \(i) l(-1,yy[i])))
  1 - above/below
}

all <- \(x,y) {
  out <- c(tau_prop(x,y),
           tau_prop2(x,y),
    new_tau(x, y),
           new_tau2(x,y),
           cor(x, y, method = "kendall"),
           DescTools::SomersDelta(x, y),
           DescTools::SomersDelta(y, x),
           DescTools::GoodmanKruskalGamma(x, y))
  names(out) <- c("prop","prop","new", "new_tau2","tau", "somer_xy", "somer_yx", "gamma")
  out
}
