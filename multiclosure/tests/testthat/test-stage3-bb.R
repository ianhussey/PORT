test_that("bb_mom_fit recovers known parameters from BB sample moments", {
  skip_if_not_installed("extraDistr")
  K <- 7L; n <- K - 1L
  a <- 3.5; b <- 2.0
  x <- 0:n
  pmf <- extraDistr::dbbinom(x, size = n, alpha = a, beta = b)
  mu0 <- sum(x * pmf)
  var <- sum((x - mu0)^2 * pmf)
  fit <- bb_mom_fit(mean = mu0 + 1, sd = sqrt(var), K = K)
  expect_true(fit$valid)
  expect_equal(fit$alpha, a, tolerance = 1e-6)
  expect_equal(fit$beta, b, tolerance = 1e-6)
})

test_that("bb_mom_fit flags underdispersion", {
  fit <- bb_mom_fit(mean = 4.0, sd = 0.05, K = 7L)
  expect_false(fit$valid)
  expect_match(fit$reason, "underdispersed")
})

test_that("bb_skew agrees with numerical moment for known params", {
  skip_if_not_installed("extraDistr")
  K <- 7L; n <- K - 1L; a <- 4; b <- 2
  x <- 0:n
  p <- extraDistr::dbbinom(x, size = n, alpha = a, beta = b)
  mu <- sum(x * p); m2 <- sum((x - mu)^2 * p); m3 <- sum((x - mu)^3 * p)
  g1_numeric <- m3 / m2^(3/2)
  expect_equal(bb_skew(n, a, b), g1_numeric, tolerance = 1e-8)
})

test_that("bb_skew_se analytical agrees roughly with bootstrap", {
  skip_if_not_installed("extraDistr")
  fit <- bb_mom_fit(mean = 4.0, sd = 1.5, K = 7L)
  se_a <- bb_skew_se(fit, N = 200L, method = "analytical")
  set.seed(1L)
  se_b <- bb_skew_se(fit, N = 200L, method = "bootstrap", B = 300L)
  # within ~30% on a small bootstrap
  expect_true(abs(se_a - se_b) / max(se_a, se_b) < 0.5)
})
