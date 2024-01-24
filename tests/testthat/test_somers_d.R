test_that("Somers' D agree with DescTools", {
  testthat::expect_equal(somers_d(x1, y1), DescTools::SomersDelta(x1, y1))
  testthat::expect_equal(somers_d(x2, y2), DescTools::SomersDelta(x2, y2))
  testthat::expect_equal(somers_d(-x3, y3), DescTools::SomersDelta(-x3, y3))
  testthat::expect_equal(somers_d(x4, y4), DescTools::SomersDelta(x4, y4))
  testthat::expect_equal(somers_d(-x5, y5), DescTools::SomersDelta(-x5, y5))
})
