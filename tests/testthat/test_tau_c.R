test_that("tau_c agrees with DescTools", {
  testthat::expect_equal(tau_c(x1, y1), DescTools::StuartTauC(x1, y1))
  testthat::expect_equal(tau_c(x2, y2), DescTools::StuartTauC(x2, y2))
  testthat::expect_equal(tau_c(-x3, y3), DescTools::StuartTauC(-x3, y3))
  testthat::expect_equal(tau_c(x4, y4), DescTools::StuartTauC(x4, y4))
  testthat::expect_equal(tau_c(-x5, y5), DescTools::StuartTauC(-x5, y5))
})
