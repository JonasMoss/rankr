library("reticulate")

source_python("count_inversions.py")

tau_py = function(x, y) {
  tau6py(x, y)
}


tau3 = function(x, y) {
  y_ = y[order(x)]
  n = length(y)
  d = count_inversions(y_)
  # Make table; should be much faster option.
  
  tab = table(x, y)
  result = 0
  
  for (k in 1:nrow(tab)) {
    row = tab[k, ]
    if (sum(row == 1) > 1) {
      values = y[row == 1]
      contribution = 0.5 + 0.5 * sum(sapply(table(values), function(i) i / n * (i - 1) / (n - 1)))
      prob = (sum(tab[k, ]) / n) ^ 2
      result = result + contribution * prob
    }

  }

  p = 0.5 - 0.5 * sum(sapply(table(y_), function(i) i / n * (i - 1) / (n - 1)))
  if(p == 0) {
    0
  } else {
    1  - (2 * d / (n * (n - 1)) + result) / p
  }

}

# To calculate p(y1 = y2), find the size of each equivalence class n_1 then
# sum (n_1 / n * (n_1 - 1) / (n - 1)). Then p(y1 < y2) = p(y2 < y1) implies that
# p(y1 < y2) = 1/2 * (1 - p(y1 = y2)).

tau4 = function(x, y) {
  
  n = length(y)
  m = n * (n - 1)
  
  f = function(z) sum(sapply(table(z), function(i) i * (i - 1))) / m
  
  n = length(y)
  tab = table(x, y)
  
  bottom_left   = 1 / 2 * (1 - f(y))
  bottom_right  = 1 / 2 * (1 + f(x))
  
  top_left = 0
  
  for (k in 1:nrow(tab)) {
    row = tab[k, ]
    x_num = sum(row)
    len = x_num * (x_num - 1)
    contribution = len - sum(sapply(row, function(i) i * (i - 1)))
    top_left = top_left + contribution
  }

  top_right = count_inversions(x, y)
  
  1 - (0.5 * top_left + top_right) / (m  * bottom_left * bottom_right)
  
}

tau_py = function(x, y) {
  tau6py(x, y)
}


tau5 = function(x, y) {
  ns = 1:length(x)
  n = length(x)
  combns = cbind(combn(ns, 2), combn(rev(ns), 2))
  
  top = sum(apply(combns, 2, function(i) {
    (y[i[1]] < y[i[2]]) & (x[i[1]] >= x[i[2]])
  }))
  
  bottom_left = sum(apply(combns, 2, function(i) {
    y[i[1]] < y[i[2]]
  }))
  
  bottom_right = sum(apply(combns, 2, function(i) {
    x[i[1]] >= x[i[2]]
  }))
  
  
  1 - top / (bottom_left * bottom_right) * n * (n - 1) 

}

tau_m_bf = function(x, y) {
  ns = 1:length(x)
  n = length(x)
  combns = cbind(combn(ns, 2), combn(rev(ns), 2))
  
  top = sum(apply(combns, 2, function(i) {
    (y[i[1]] <= y[i[2]]) & (x[i[1]] > x[i[2]])
  }))
  
  bottom_left = sum(apply(combns, 2, function(i) {
    y[i[1]] <= y[i[2]]
  }))
  
  bottom_right = sum(apply(combns, 2, function(i) {
    x[i[1]] > x[i[2]]
  }))
  
  
  1 - top / (bottom_left * bottom_right) * n * (n - 1) 
  
}

tau_m = function(x, y) {
  tau_mpy(x, y)
}