test_that("Goodman-Kruskall agrees with DescTools", {
  testthat::expect_equal(gk_gamma(x1, y1), DescTools::GoodmanKruskalGamma(x1, y1))
  testthat::expect_equal(gk_gamma(x2, y2), DescTools::GoodmanKruskalGamma(x2, y2))
  testthat::expect_equal(gk_gamma(-x3, y3), DescTools::GoodmanKruskalGamma(-x3, y3))
  testthat::expect_equal(gk_gamma(x4, y4), DescTools::GoodmanKruskalGamma(x4, y4))
  testthat::expect_equal(gk_gamma(-x5, y5), DescTools::GoodmanKruskalGamma(-x5, y5))
})
