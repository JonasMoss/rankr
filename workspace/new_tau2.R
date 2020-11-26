## Comparison with real tau on strictly monotone data.

n = 25
x = seq(from = 0, to = 1, length.out = n)
y = x + runif(n)
tau4(x, y)
cor(x, y, method = "kendall")
tau5(x, y)

## Comparison.
n = 200
x = seq(from = 0, to = 100, length.out = n)
x0 = 33
x1 = 66
f = function(x) 0 * (x < x0) + (x - x0) * (x >= x0 & x < x1) + (x1) * (x > x1) 
f = function(x) 0 * (x < x0) + x0 * (x >= x0 & x < x1) + x1 * (x > x1) 
y = f(x)
#y[5] = 0.6
#y[15] = 0.3

## Ties in both.
# = c(0, 0, 0, 1, 1, 1, 2, 2, 2)
#y = c(0, 1, 1, 2, 3, 3, 4, 5, 5)



tau4(x, y)
tau4(y, x)
tau5(x, y)
tau5(y, x)

plot(x, y)

plot(x, y, ylim = c(0, 1))
points(y, x, col = "blue")
tau4(x, y) + tau4(y, x)
cor(x, y, method = "kendall")
tau4(y, x)
cor(y, x, method = "kendall")
1/2*(tau4(x, y) + tau4(y, x))
sqrt(tau4(x, y) * tau4(y, x))


n = 100
x = seq(from = 0, to = 1, length.out = n)
x0 = 0.1
x1 = 0.3
f = function(x) 0 * (x < x0) + x0 * (x >= x0 & x < x1) + (x1) * (x > x1) 
y = f(x)
plot(x, y)

tau3(y, x)
tau3(x, y)
tau4(x, y)
tau4(y, x)

z = x
x = y
y = z



plot(y, x)

x = seq(from = 0, to = 1, length.out = n)
y = rep(1, n)


#y = rep(0, 17)
#y[17] = 0.1
plot(x, y)
cor(x, y, method = "kendall")
tau3(x, y)

plot(x, y, ylim = c(0, 1))
points(y, x, col = "blue")

(1 - cor(x, y, method = "kendall")) * (n * (n - 1)) / 4


cor(x, rep(1, n), method = "kendall")

