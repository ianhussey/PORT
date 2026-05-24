test_that("check_grim_grimmer wraps scrutiny without erroring", {
  skip_if_not_installed("scrutiny")
  out <- check_grim_grimmer(M = c(4.00, 3.50), s = c(1.00, 0.90),
                            N = 100L, K = 7L, dp = 2L)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("item", "mean_reported", "sd_reported",
                      "grim_pass", "grimmer_pass", "reason"))
})

test_that("GRIM flags impossible mean for given N", {
  skip_if_not_installed("scrutiny")
  out <- check_grim_grimmer(M = 3.71, s = 1.20, N = 100L, K = 7L)
  # M=3.71 at N=100 means sum=371 -> integer; GRIM should pass.
  # Try non-integer: M=3.715 at N=100 -> sum=371.5 not integer.
  out2 <- check_grim_grimmer(M = 3.715, s = 1.20, N = 100L, K = 7L)
  expect_false(any(isTRUE(out2$grim_pass)))
})
