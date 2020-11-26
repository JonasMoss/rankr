set.seed(313)
n = 10
x = sort(runif(n, -1, 1))
f = function(x) x
f = function(x) -x^2 
y = f(x)+ rnorm(n)/10 

pred = function(x, y, f) {
  u = apply(combn(y, 2), 2, which.max)
  fx = f(x)
  v = apply(combn(fx, 2), 2, which.max)
  1 - 2 * (1 - mean(u == v))
}

pred2 = function(x, y, m) {
  v1 = apply(combn(x[1:m], 2), 2, which.max)
  v2 = apply(combn(-x[(m+1):n], 2), 2, which.max)
  v = c(v1, v2)
  u1 = apply(combn(y[1:m], 2), 2, which.max)
  u2 = apply(combn(y[(m+1):n], 2), 2, which.max)
  u = c(u1, u2)
  1 - 2 * (1 - mean(u == v))
}


cor(x, y, method = "kendall")
pred(x, y, f)
h(x, y)
pred2(x, y, m = 160)


g2 = function(x, y) {
  n = length(y)
  z = y[order(x)]
  
  # This is the vector of swaps for the vector sorted in increasing order. 
  # Each swap is recorded once, and only for the _smallest-ranked_ element.
  
  right_swaps = bubble_expand(z)
  left_swaps = rep(0, n)
  
  # The Kendall's tau for this z is:
  
  taus = rep(NA, n)
  taus[1] = 1 - 4 * sum(right_swaps) / (n * (n - 1))
  taus[n] = -taus[1]
  
  for(m = seq(2, n - 2)) {
    taus[m] = NA
    bubble_expand(rev(y[1:2]))
    
  }
  
  
  
  
}


bubble_expand = function(y) {
  
  z = rank(y)
  n = length(z)
  orders = order(z)
  swaps = rep(0, length(z))
  swap = 0
  
  for(i in 1:(n - 1)) {
    
    for(j in 1:(n - i)) {
      
      if(z[j] > z[j + 1]) {
        
        temp = z[j]
        z[j] = z[j + 1]
        z[j + 1] = temp
        
        # We must record the swap on the smallest ranked element. 
        if(orders[z[j]] < orders[z[j + 1]]) {
          swaps[orders[z[j]]] = swaps[orders[z[j]]] + 1
        } else {
          swaps[orders[z[j + 1]]] = swaps[orders[z[j + 1]]] + 1
        }
        
        swap = swap + 1
        
      }
      
    }
    
  }
  
  swaps
  
}


# Here y is sorted (decreasing) from 1:k. We will sort it to (k + 1) and record
# the number of swaps needed. Designed to be ported to pass by reference in
# C++.

bubbles = function(y, k) {

  swap = 0
  x = y[k + 1]
  
  for(j in k:1) {
    if(x > y[j]) {
      y[j + 1] = y[j]
      y[j] = x
      swap = swap + 1
    } else break
  }
    
  swap
  
}
