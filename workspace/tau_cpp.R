Rcpp::sourceCpp("tau.cpp")

tau_d(x, y)
tau_d(y, x)
tau5(x, y)
tau5(y, x)

tau_a(x, y)
DescTools::KendallTauA(y, x)
tau_a(y, y)
DescTools::KendallTauA(y, y)

cor(x, y, method = "kendall")
tau_b(x, y)
tau_b(y, x)

tau_c(x, y)
tau_c(y, x)
DescTools::StuartTauC(x, y)
DescTools::StuartTauC(y, x)

DescTools::SomersDelta(y, x)
somers_d(y, x)
DescTools::SomersDelta(x, y)
somers_d(x, y)

gd_gamma(x, y)
gd_gamma(y, x)
DescTools::GoodmanKruskalGamma(x, y)
DescTools::GoodmanKruskalGamma(y, x)


# tau_py(x, y)
# Rcpp::sourceCpp("count_inversions.cpp")
# count_inversions(y)
reticulate::source_python("count_inversions.py")