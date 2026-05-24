test_that("Bhatia-Davis catches impossible SD", {
  # K=5, M=2.00 -> max SD = sqrt(1*3) ~= 1.732
  out <- check_bhatia_davis(M = 2.00, s = 2.50, K = 5L)
  expect_false(out$pass)
  expect_match(out$reason, "Bhatia-Davis")
})

test_that("Bhatia-Davis accepts feasible SD", {
  out <- check_bhatia_davis(M = 3.00, s = 1.00, K = 5L)
  expect_true(out$pass)
})

test_that("max SD attained at mean = (K+1)/2", {
  K <- 7L; M <- (K + 1) / 2
  sd_max <- sqrt((M - 1) * (K - M))
  out <- check_bhatia_davis(M = M, s = sd_max, K = K)
  expect_true(out$pass)
})
