set.seed(313)

x1 <- sort(rnorm(10))
y1 <- c(rep(1, 3), rep(2, 3), rep(3, 4)) + rbinom(10, 10, 0.1)

x2 <- c(rep(1, 2), rep(2, 2), runif(4, 3, 4), rep(5, 2))
y2 <- c(rep(1, 3), rep(2, 3), rep(3, 4)) + rbinom(10, 10, 0.1)

x3 <- c(rep(1, 2), rep(2, 3), c(3, 4, 5, 5, 5))
y3 <- c(rep(1, 3), rep(2, 3), rep(3, 4))

x4 <- c(rep(1, 3), rep(2, 3), c(3, 4, 5, 5))
y4 <- c(rep(1, 3), rep(2, 3), rep(3, 4))

x5 <- c(rep(1, 3), rep(2, 3), c(3, 4, 4, 4))
y5 <- c(rep(1, 3), c(2, 3, 3), c(3, 4, 4, 5))

x6 <- c(rep(1, 3), rep(2, 3), c(5, 5, 5, 5))
y6 <- c(rep(1, 3), rep(2, 3), c(3, 4, 5, 6))

x7 <- c(rep(1, 3), rep(2, 3), rep(3, 3))
y7 <- c(1, 2, 3, 3, 2, 1, 2, 1, 3)

plot(x1, y1)
plot(x2, y2)
plot(x3, y3)
plot(x4, y4) # y = f(x) is weakly monotone.
plot(x4, x4**2) # y =
