## Comparison.
set.seed(313)
n = 10
x = sort(sample(1:300, n, replace = TRUE))
x0 = 33
x1 = 66
f = function(x) 0 * (x < x0) + (x - x0) * (x >= x0 & x < x1) + (x1) * (x >= x1)
#f = function(x) 0 * (x < x0) + x0 * (x >= x0 & x < x1) + x1 * (x > x1)
y = f(x) + rbinom(n, 1, 0.5)
y = f(x)

d = function(x, y) {
  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))
  l = mean(ys[1, ] > ys[2, ])
  u = mean(ys[1, ] > ys[2, ] & xs[1, ] <= xs[2, ]) + mean(ys[1, ] <= ys[2, ] & xs[1, ] > xs[2, ])
  1 - u / l
}


d = function(x, y) {
  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))
  l = sum(ys[1, ] > ys[2, ])
  u = sum(ys[1, ] > ys[2, ]) + rankr:::sum_prod_xy(x, y)/2 - rankr:::sum_prod_x(x)/2 - 2 * rankr:::inversions(x, y)
  u / l
}


sum(ys[1, ] > ys[2, ] & xs[1, ] == xs[2, ]) == - rankr:::sum_prod_xy(x, y)/2 + rankr:::sum_prod_x(x)/2
2 * rankr:::inversions(x, y) == sum((ys[2, ] < ys[1, ] & xs[2, ] > xs[1, ]))

sum(ys[1, ] > ys[2, ]) + rankr:::sum_prod_xy(x, y)/2 - rankr:::sum_prod_x(x)/2 - 2 * rankr:::inversions(x, y)
sum(ys[1, ] > ys[2, ]) - sum(ys[1, ] > ys[2, ] & xs[1, ] == xs[2, ]) - 2 * rankr:::inversions(x, y)


sum(ys[1, ] > ys[2, ] & xs[1, ] >= xs[2, ])



d = function(x, y) {
  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))
  l = sum(ys[1, ] > ys[2, ])
  u = (n * (n-1) - rankr:::sum_prod_x(x) - rankr:::sum_prod_x(y) + rankr:::sum_prod_xy(x, y) - 4*rankr:::inversions(x, y))/2
  u / l
}


d = function(x, y) {
  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))
  l = sum(ys[1, ] > ys[2, ])
  u = 2 * sum(ys[1, ] > ys[2, ] & xs[1, ] < xs[2, ]) + sum(ys[1, ] > ys[2, ] & xs[1, ] == xs[2, ])
  1 - u / l
  #u = sum(ys[1, ] > ys[2, ] & xs[1, ] > xs[2, ]) - sum(ys[1, ] > ys[2, ] & xs[1, ] < xs[2, ])
  #u / l
}


d(x, y)
d(y, x)
rankr::somers_d(x, y)
rankr::somers_d(y, x)
f(x, y)
f(y, x)
#DescTools::SomersDelta(x, y)

u = 2 * sum(ys[1, ] > ys[2, ] & xs[1, ] < xs[2, ])
1 - u / l


f = function(x, y) {
  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))
  l = sum(ys[1, ] > ys[2, ])
  u = sum(ys[1, ] < ys[2, ] & xs[1, ] >= xs[2, ]) + sum(ys[1, ] >= ys[2, ] & xs[1, ] < xs[2, ])
  1 - u / l
}


sum(ys[1, ] <= ys[2, ] & xs[1, ] >= xs[2, ]) + sum(ys[1, ] > ys[2, ] & xs[1, ] < xs[2, ])
sum(ys[1, ] <= ys[2, ] & xs[1, ] <= xs[2, ]) + sum(ys[1, ] > ys[2, ] & xs[1, ] < xs[2, ])
sum(ys[1, ] < ys[2, ] & xs[1, ] >= xs[2, ]) + sum(ys[1, ] >= ys[2, ] & xs[1, ] < xs[2, ])
sum(ys[1, ] > ys[2, ] & xs[1, ] >= xs[2, ]) + sum(ys[1, ] <= ys[2, ] & xs[1, ] < xs[2, ])


f = function(x, y) {
  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))
  mean(ys[1, ] > ys[2, ]) - mean(ys[1, ] > ys[2, ] & xs[1, ] < xs[2, ])
  mean(ys[1, ] > ys[2, ] & xs[1, ] >= xs[2, ])
}

g = function(x, y) {
  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))
  u = mean(ys[1, ] > ys[2, ]) - mean(ys[1, ] > ys[2, ] & xs[1, ] < xs[2, ])
  l = mean(ys[1, ] > ys[2, ]) * mean(xs[1, ] >= xs[2, ])
  1 - u/l
}

# g(x,y)
# rankr::tau_d(x, y)
# rankr::tau_d(y, x)

d(x, y)
d(y, x)
rankr::somers_d(x, y)
rankr::somers_d(y, x)
DescTools::SomersDelta(x, y)

ys = cbind(combn(y, 2), combn(rev(y), 2))
xs = cbind(combn(x, 2), combn(rev(x), 2))
#ys = combn(y, 2)
#xs = combn(x, 2)
2 * rankr:::count_inversions(y) / (n * (n - 1))
mean((ys[2, ] < ys[1, ] & xs[2, ] > xs[1, ]) | (ys[1, ] < ys[2, ] & xs[1, ] > xs[2, ]))

sum(((ys[2, ] > ys[1, ]) & (xs[2, ] >= xs[1, ])) | ((ys[1, ] >= ys[2, ]) & (xs[1, ] >= xs[2, ])))

n * (n - 1) - sum((ys[1, ] < ys[2, ] & xs[1, ] < xs[2, ]) | (ys[1, ] > ys[2, ] & xs[1, ] > xs[2, ]))

rankr:::count_inversions(y)
rankr:::sum_prod_xy(x, y)
rankr:::sum_prod_x(y)
rankr:::count_inversions(y)
DescTools::ConDisPairs(table(x, y))$D

sum((ys[2, ] < ys[1, ] & xs[2, ] > xs[1, ]))

n = length(x)
(n * (n-1) - rankr:::sum_prod_x(x) - rankr:::sum_prod_x(y) + rankr:::sum_prod_xy(x, y) - 4*rankr:::inversions(x, y))/2

sum(ys[1, ] > ys[2, ]) + rankr:::sum_prod_xy(x, y)/2 - rankr:::sum_prod_x(x)/2 - 2 * rankr:::inversions(x, y))

(DescTools::ConDisPairs(table(x, y))$C - DescTools::ConDisPairs(table(x, y))$D)

### ============================================================================
### Symmetry
### ============================================================================

ys = cbind(combn(y, 2), combn(rev(y), 2))
xs = cbind(combn(x, 2), combn(rev(x), 2))
u = xs[1, ] >= xs[2, ]
v = ys[1, ] >= ys[2, ]

fn = function(par) {
  a = par[1]
  b = par[2]
  mean(abs(u - a - b * v))
}

tau_e = function(x, y, l = abs) {

  ys = cbind(combn(y, 2), combn(rev(y), 2))
  xs = cbind(combn(x, 2), combn(rev(x), 2))

  v = xs[1, ] > xs[2, ]
  u = ys[1, ] > ys[2, ]

  fn = function(par) {
    a = par[1]
    b = par[2]
    mean(l(u - a - b * v))
  }

  f = function(a) mean(l(u - a))

  upper = optim(par = c(1, 1), fn = fn)$value
  lower = optimize(f, lower = 0, upper = 1)$objective

  1 - upper/lower

}

u = mean(ys[1, ] >= ys[2, ] & xs[1, ] < xs[2, ]) + mean(ys[1, ] < ys[2, ] & xs[1, ] >= xs[2, ])
l = mean(xs[1, ] < xs[2, ])

1 - u/l
fn = function(a) mean(abs(u - a))


