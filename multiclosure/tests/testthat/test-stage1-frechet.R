test_that("frechet_bounds_pair recovers ±1 for identical marginals", {
  x <- c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L)
  y <- x
  b <- frechet_bounds_pair(x, y)
  expect_equal(b$r_max, 1, tolerance = 1e-8)
  expect_equal(b$r_min, -1, tolerance = 1e-8)
})

test_that("Frechet check flags r outside bounds for skewed marginals", {
  skip_if_not_installed("unsum")
  M <- c(4.80, 1.20); s <- c(0.50, 0.50)
  R <- matrix(c(1, 0.95, 0.95, 1), 2, 2)
  out <- check_frechet(M, s, R, K = 5L, N = 80L, tol = make_tols())
  expect_false(all(out$pass))
})
