# New, Somers, and Goodman are equal here; are they always equal when x is
# without ties?
x <- sort(rnorm(10))
y <- -c(rep(1,3), rep(2, 3), rep(3, 4)) + rbinom(10, 10, 0.1)
all(x,y)

tau_prop <- \(x, y) {
  a <- tau(x,y)
  1 - 2*(a[2] + a[3]) / ((a[1] + a[2] + a[3]))
}

tau_prop <- \(x, y) {
  a <- tau(x,y)
  (a[1] - a[2] - a[3]) / ((a[1] + a[2] + a[3]))
}

tau_prop <- \(x, y) {
  a <- tau(x,y)
  1 - (min(2*a[1], 2*a[2], a[1]+a[2])+2*a[3]) / ((a[1] + a[2] + a[3]))
}

set.seed(1)
# Now they aren't equal; x ain't monotone no more.
x <- c(rep(1,2), rep(2, 2), runif(4, 3, 4), rep(5, 2))
y <- c(rep(1,3), rep(2, 3), rep(3, 4)) + rbinom(10, 10, 0.1)

all(x,y)
new_tau(x, y)
tau_prop(x, y)

all(x,y)
n <- length(x)
diff <- tau_a(x,y)*((n*(n-1))/2)
1/(somers_d(x, y) / diff)
a <- tau(x,y)

(a[1] + a[2] + a[3]) / 2

counter(x,y)

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
all(x,y)

# Harder monotonicity (2)
x <- c(rep(1,3), rep(2, 3), c(3, 4, 4, 4))
y <- c(rep(1,3), c(2, 3, 3), c(3, 4, 4, 5))
all(x,y)

x <- c(rep(1,3), rep(2, 3), c(5, 5, 5, 5))
y <- -c(rep(1,3), rep(2, 3), c(3, 4, 5, 6))
all(x,y)

x <- c(rep(1,3), rep(2, 3), rep(3, 3))
y <- c(1,2,3,3,2,1,2,1,3)
all(x, y)

plot(x,y)

new_tau(x, y)
cor(x, y, method = "kendall")
DescTools::SomersDelta(x, y)
DescTools::SomersDelta(y, x)
DescTools::GoodmanKruskalGamma(x, y)
