test_that("ground-truth summary passes verification on its own dataset", {
  skip_if_not_installed("GenOrd")
  skip_if_not_installed("unsum")
  skip_if_not_installed("scrutiny")
  skip_if_not_installed("extraDistr")

  set.seed(2L)
  N <- 100L; K <- 5L; p <- 3L
  ab <- c(3.0, 3.0); n <- K - 1L
  R0 <- matrix(0.30, p, p); diag(R0) <- 1
  L <- chol(R0)
  Z <- matrix(rnorm(N * p), N, p) %*% L
  U <- pnorm(Z)
  pmf <- extraDistr::dbbinom(0:n, size = n, alpha = ab[1], beta = ab[2])
  cdf <- cumsum(pmf)
  X <- matrix(NA_integer_, N, p)
  for (j in seq_len(p))
    X[, j] <- as.integer(findInterval(U[, j], cdf) + 1L)
  X[X > K] <- K; X[X < 1L] <- 1L

  M <- round(colMeans(X), 2L)
  s <- round(apply(X, 2L, stats::sd), 2L)
  R <- round(stats::cor(X), 2L); diag(R) <- 1

  out <- multi_closure(N = N, K = K, M = M, s = s, R = R,
                       max_candidates = 20L,
                       stage4 = list(strategy = "bb_closest",
                                     max_combos = 100L,
                                     samples_per_combo = 1L),
                       verbose = FALSE, seed = 1L)

  # Strong: ground truth passes verification on its own reported summary
  gt_d <- summary_distance(X, M, s, R)
  expect_lte(gt_d$dM, 0.005 + 1e-12)
  expect_lte(gt_d$ds, 0.005 + 1e-12)
  expect_lte(gt_d$dR, 0.005 + 1e-12)

  # Weaker: Stage 1 should pass; expect feasible or indeterminate, not FALSE
  expect_true(isTRUE(out$feasibility$passed) || is.na(out$feasibility$passed))
})
