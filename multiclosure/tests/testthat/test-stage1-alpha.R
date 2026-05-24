test_that("alpha_from_R_s recovers a known value", {
  R <- matrix(0.30, 4L, 4L); diag(R) <- 1
  s <- rep(1.0, 4L)
  # With all r_jk = 0.30, s = 1: V = 4 + 2*6*0.30 = 7.6; sum(s^2)=4
  # alpha = (4/3)*(1 - 4/7.6) ~= 0.6316
  expect_equal(alpha_from_R_s(R, s), 0.6315789, tolerance = 1e-5)
})

test_that("check_alpha flags large mismatch", {
  R <- matrix(0.25, 3L, 3L); diag(R) <- 1
  s <- rep(1.0, 3L)
  tols <- make_tols()
  out <- check_alpha(R, s, alpha_reported = 0.85, N = 100L, tol = tols)
  expect_false(out$pass)
  expect_true(out$alpha_reported > out$alpha_hi)
})

test_that("check_alpha accepts a consistent reported alpha", {
  R <- matrix(0.30, 4L, 4L); diag(R) <- 1
  s <- rep(1.0, 4L)
  tols <- make_tols()
  out <- check_alpha(R, s, alpha_reported = 0.63, N = 100L, tol = tols)
  expect_true(out$pass)
})

test_that("check_alpha returns pass=NA when alpha not reported", {
  R <- matrix(0.30, 3L, 3L); diag(R) <- 1
  out <- check_alpha(R, s = rep(1, 3L), alpha_reported = NULL, N = 50L,
                     tol = make_tols())
  expect_true(is.na(out$pass))
})
