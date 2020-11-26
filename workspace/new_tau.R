n = 10 ^ 2
x = sort(runif(n, 0, 1))

y1 = x ^ 2
y2 = x ^ 3 + x ^ 2
y3 = x + x ^ 2


x0 = 0.9
f = function(x) 0 * (x < x0) + (x - x0) * (x >= x0)

y = f(x)
cor(x, y, method = "kendall")

plot(x, y)
plot(y, x)

new_tau(x, y, a = 1, b = 1)
new_tau(x, y, a = 1, b = 0)
new_tau(y, x, a = 1, b = 1)
new_tau(y, x, a = 1, b = 0)

new_tau2(x, y, a = 1)
new_tau2(x, y, a = 1)

z = x
x = y
y = z
new_tau2(x, y, a = 1)
new_tau2(x, y, a = 1)

new_tau = function(x, y, a, b) {
  
  omega = function(x, a, b) {
    x1 = x[1]
    x2 = x[2]
    if (x1 == x2) {
      return(b)
    } else {
      a * (x1 > x2) - a * (x1 < x2) 
    }
  }
  
  y_comb = cbind(combn(y, 2), combn(rev(y), 2))
  x_comb = cbind(combn(x, 2), combn(rev(x), 2))
  ys = apply(y_comb , 2, function(x) omega(x, a = 1, b = 0))
  xs = apply(x_comb, 2, function(x) omega(x, a = a, b = b))
  1 - mean(xs != ys) / max(mean(y_comb[1,] == y_comb[2,]), mean(y_comb[1,] < y_comb[2,]))
}


new_tau2 = function(x, y, a) {
  
  omega = function(x) {
    x1 = x[1]
    x2 = x[2]
    if (x1 == x2) {
      10
    } else {
      a * (x1 > x2) - a * (x1 < x2) 
    }
  }
  
  y_comb = cbind(combn(y, 2), combn(rev(y), 2))
  x_comb = cbind(combn(x, 2), combn(rev(x), 2))
  ys = apply(y_comb, 2, function(x) omega(x))
  xs = apply(x_comb, 2, function(x) omega(x))
  1 - mean(xs != ys | !(ys == 5)) / mean(y_comb[1,] <= y_comb[2,])
}