## Make plot of mpg ~ cyl and cyl ~ mpg.

par(mfrow=c(1,2))

w = round(tau_weak(mtcars$mpg, mtcars$cyl), 2)
s = round(tau_strict(mtcars$mpg, mtcars$cyl), 2)
b = round(tau_b(mtcars$mpg, mtcars$cyl), 2)
main = bquote(tau[w] ~ .("=") ~ .(w) ~ .(";") ~ tau[s] ~ .("=") ~ .(s) ~ tau[b] ~ .("=") ~ .(b))

plot(mtcars$mpg, mtcars$cyl, xlab = "Miles per gallon", ylab = "Cylinders",
     sub = main)
x <- sort(mtcars$mpg)
y <- mtcars$cyl[order(mtcars$mpg)]
z <- c(rep(8,12), rep(6, 9), rep(4, 11))
lines(x, z, "s")

w = round(tau_weak(mtcars$cyl, mtcars$mpg),2)
s = round(tau_strict(mtcars$cyl, mtcars$mpg),2)
b = round(tau_b(mtcars$cyl, mtcars$mpg),2)
main = bquote(tau[w] ~ .("=") ~ .(w) ~ .(";") ~ tau[s] ~ .("=") ~ .(s) ~ tau[b] ~ .("=") ~ .(b))

plot(mtcars$cyl, mtcars$mpg, xlab = "Cyliders", ylab = "Miles per gallon", main = main)


omega <- \(x) {
  if (x[1] > x[2]) {
    1
  } else if (x[2] > x[1]) {
    -1
  } else {
    0
  }
}

x <- sort(mtcars$mpg)
y <- mtcars$cyl[order(mtcars$mpg)]
z <- c(rep(8,12), rep(6, 9), rep(4, 11))
lines(x, z, "h")

pairs <- \(x) {
  xx <- arrangements::combinations(x, 2)
  apply(rbind(xx, arrangements::combinations(rev(x), 2)), 1, omega)
}

yy <- pairs(y)
zz <- pairs(z)

l <- \(x, y) 1*(!(x == y))
l2 <- \(x, y) 1*(!(x == y || y == 0))
above1 <- sum(sapply(seq_along(yy), \(i) l(zz[i], yy[i])))
above2 <- sum(sapply(seq_along(yy), \(i) l2(xx[i], yy[i])))
belowm <- sum(sapply(seq_along(yy), \(i) l(-1,yy[i])))
belowp <- sum(sapply(seq_along(yy), \(i) l(1,yy[i])))
below0 <- sum(sapply(seq_along(yy), \(i) l(0,yy[i])))
below <- 2*table(yy)[1]
1 - above1/below




theor2 <-tau_data(x,y)$n_d + tau_data(x,y)$t_x
theor1 <- tau_data(z,y)$n_d + tau_data(z,y)$t_x + tau_data(z,y)$t_y
