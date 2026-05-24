test_that("canonicalise_candidate is row-permutation invariant", {
  set.seed(1L)
  X <- matrix(sample(1:5, 30, replace = TRUE), 10, 3)
  perm <- X[sample(nrow(X)), ]
  expect_equal(canonicalise_candidate(X), canonicalise_candidate(perm))
})

test_that("verify_summaries flags perturbed candidates", {
  set.seed(1L)
  X <- matrix(sample(1:5, 30, replace = TRUE), 10, 3)
  M <- colMeans(X); s <- apply(X, 2L, sd); R <- cor(X)
  out <- verify_summaries(list(X), M, s, R, make_tols())
  expect_equal(out$n_pass, 1L)

  # Perturb one cell — should still pass at 0.005 tol? Probably yes for
  # a single cell in N=10. Use a coarser perturbation:
  Y <- X; Y[1, 1] <- if (Y[1, 1] < 5) Y[1, 1] + 1 else Y[1, 1] - 1
  out2 <- verify_summaries(list(Y), M, s, R, make_tols())
  expect_equal(out2$n_pass, 0L)
})

test_that("summary_distance reports per-element distances", {
  X <- matrix(c(1, 2, 3, 4, 5, 1), 3, 2)
  M <- c(2, 10/3); s <- c(stats::sd(X[,1]), stats::sd(X[,2]))
  R <- stats::cor(X)
  d <- summary_distance(X, M, s, R)
  expect_equal(d$dM, 0, tolerance = 1e-10)
})
