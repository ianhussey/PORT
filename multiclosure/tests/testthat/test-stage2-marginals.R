test_that("enumerate_marginals returns valid per-item tibbles", {
  skip_if_not_installed("unsum")
  out <- enumerate_marginals(M = c(3.20, 2.85),
                             s = c(0.95, 1.10),
                             N = 50L, K = 5L)
  expect_length(out, 2L)
  for (j in seq_along(out)) {
    expect_s3_class(out[[j]], "tbl_df")
    if (nrow(out[[j]]) > 0L) {
      expect_named(out[[j]], c("candidate_id", "counts", "pmf", "cdf",
                               "mean", "sd", "skew"))
      # cdf is monotone non-decreasing and ends at 1
      cdf <- out[[j]]$cdf[[1]]
      expect_true(all(diff(cdf) >= -1e-12))
      expect_equal(tail(cdf, 1L), 1, tolerance = 1e-12)
    }
  }
})

test_that("marginal_cdf is monotone and ends at 1", {
  cdf <- marginal_cdf(c(2L, 3L, 5L, 0L, 0L), K = 5L)
  expect_true(all(diff(cdf) >= 0))
  expect_equal(tail(cdf, 1L), 1)
})
