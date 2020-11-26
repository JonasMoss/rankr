library("DescTools")


tau2(x, y)
tau2(y, x)
tau(x, y)
tau(y, x)
KendallTauA(x, y) 
cor(x, y, method = "kendall") # tau_b
StuartTauC(x, y)
GoodmanKruskalGamma(x, y)
SomersDelta(x, y)
SomersDelta(y, x)



n = 50
x = sort(runif(n))
x = sample(1:100, n, replace = TRUE)/100
y = x^2 * (x < 0.5) + 0.25 * (x >= 0.5) + runif(n, 0, 0.01)
y = x^2 * (x < 0.5) + 0.25 * (x >= 0.5)

x = mtcars$mpg
y = mtcars$cyl


x = sort(sample(c(0, 1), 50, replace = TRUE, prob = c(0.5, 0.5)))
y = sample(c(0, 1), 50, replace = TRUE, prob = c(0.5, 0.5))[order(x)]


df = mtcars
mat = matrix(nrow = ncol(df), ncol = ncol(df))
for(i in 1:ncol(df)) {
  for (j in 1:ncol(df)) {
    mat[i, j] = tau(df[, i], df[, j])
  }
}

rownames(mat) = colnames(df)
colnames(mat) = colnames(df)
corrplot::corrplot(mat)


## Ties in both: Can maybe to used as counterexamples to Goodman tau and Somers that
## they do not measure deviation from monotonicity.
x = c(0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3)
y = c(0, 1, 1, 1, 0, 2, 3, 3, 4, 5, 5, 2)

## Ties in one: 
y = 1:9
x = c(0,0,0,1,1,1,2,2,2)

## Check: Very strange result for GoodmanKruskall tau
set.seed(313)
x = runif(10)
y = runif(10)
plot(x, y)
GoodmanKruskalTau(x, y)

n = 30000
x = sample(1:10, n, replace = TRUE)
y = sample(1:10, n, replace = TRUE)

x = runif(30000)
y = runif(30000)
microbenchmark::microbenchmark(tau_b(x, y),pcaPP::cor.fk(x, y))

microbenchmark::microbenchmark(tau(x, y), tau2(x, y), count_inversions_cpp(x), 
                               pcaPP::cor.fk(x, y), counter(x))
microbenchmark::microbenchmark(tau(x, y), tau_py(x, y), cor(x, y, method = "kendall"), pcaPP::cor.fk(x, y))
