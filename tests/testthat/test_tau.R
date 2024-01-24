test_that("tau(x,y)=-tau(x,-y)", {
  expect_equal(tau(x1, y1), -tau(x1, -y1))
  expect_equal(tau(x2, y2), -tau(x2, -y2))
  expect_equal(tau(x3, y3), -tau(x3, -y3))
  expect_equal(tau(x4, y4), -tau(x4, -y4))
  expect_equal(tau(x5, y5), -tau(x5, -y5))
  expect_equal(tau(x6, y6), -tau(x6, -y6))
  expect_equal(tau(x7, y7), -tau(x7, -y7))
})

test_that("tau(x,y)=-tau(-x,y)", {
  expect_equal(tau(x1, y1), -tau(-x1, y1))
  expect_equal(tau(x2, y2), -tau(-x2, y2))
  expect_equal(tau(x3, y3), -tau(-x3, y3))
})

test_that("tau(x,y)=0 when x,y independent", {
  expect_equal(tau(x7, y7), 0)
})
