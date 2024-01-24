test_that("tau_a agree with DescTools", {
  testthat::expect_equal(tau_a(x1, y1), DescTools::KendallTauA(x1, y1))
  testthat::expect_equal(tau_a(x2, y2), DescTools::KendallTauA(x2, y2))
  testthat::expect_equal(tau_a(-x3, y3), DescTools::KendallTauA(-x3, y3))
  testthat::expect_equal(tau_a(x4, y4), DescTools::KendallTauA(x4, y4))
  testthat::expect_equal(tau_a(-x5, y5), DescTools::KendallTauA(-x5, y5))
})
