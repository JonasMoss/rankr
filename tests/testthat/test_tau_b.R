test_that("tau_b agrees with DescTools", {
  testthat::expect_equal(tau_b(x1, y1), DescTools::KendallTauB(x1, y1))
  testthat::expect_equal(tau_b(x2, y2), DescTools::KendallTauB(x2, y2))
  testthat::expect_equal(tau_b(-x3, y3), DescTools::KendallTauB(-x3, y3))
  testthat::expect_equal(tau_b(x4, y4), DescTools::KendallTauB(x4, y4))
  testthat::expect_equal(tau_b(-x5, y5), DescTools::KendallTauB(-x5, y5))
})
