test_that("make_tols defaults to half-rounding intervals", {
  t <- make_tols()
  expect_equal(t$M, 0.005)
  expect_equal(t$s, 0.005)
  expect_equal(t$R, 0.005)
  expect_null(t$alpha)
})

test_that("apply_tol returns symmetric interval", {
  iv <- apply_tol(0.30, 0.005)
  expect_equal(iv$lo, 0.295)
  expect_equal(iv$hi, 0.305)
})

test_that("in_interval is closed on both ends", {
  expect_true(in_interval(0.5, 0.5, 0.5))
  expect_true(in_interval(0.5, 0.0, 1.0))
  expect_false(in_interval(0.5, 0.6, 1.0))
})
