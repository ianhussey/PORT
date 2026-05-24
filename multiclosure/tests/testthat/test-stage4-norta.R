test_that("norta_reconstruct returns a feasible candidate for a simple case", {
  skip_if_not_installed("GenOrd")
  K <- 5L
  pmf <- c(0.05, 0.20, 0.50, 0.20, 0.05)
  cdf <- cumsum(pmf)
  R <- matrix(c(1, 0.30, 0.30, 1), 2, 2)
  out <- norta_reconstruct(list(cdf, cdf), R_target = R, N = 100L, K = K)
  expect_true(out$feasible)
  expect_length(out$candidates, 1L)
  expect_equal(dim(out$candidates[[1]]), c(100L, 2L))
})

test_that("choose_marginal_combos respects max_combos", {
  skip_if_not_installed("unsum")
  marg <- list(
    tibble::tibble(candidate_id = 1:5, skew = c(-0.1, 0, 0.1, 0.2, 0.3)),
    tibble::tibble(candidate_id = 1:5, skew = c(-0.2, -0.1, 0, 0.1, 0.2))
  )
  bb_fits <- list(list(valid = TRUE, skew_bb = 0),
                  list(valid = TRUE, skew_bb = 0))
  R <- diag(2)
  combos <- choose_marginal_combos(marg, R, bb_fits,
                                   strategy = "bb_closest",
                                   max_combos = 6L, seed = 1L)
  expect_lte(nrow(combos), 6L)
  expect_named(combos, c("combo_id", "item_1", "item_2"))
})
