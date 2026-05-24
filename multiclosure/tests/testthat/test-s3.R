test_that("print and accessors work on a stage-1-failing object", {
  out <- multi_closure(
    N = 80L, K = 5L,
    M = c(2.00, 3.00, 3.50),
    s = c(2.50, 1.00, 0.90),
    R = matrix(c(1, .3, .2, .3, 1, .25, .2, .25, 1), 3, 3),
    verbose = FALSE)
  expect_s3_class(out, "multiClosure")
  expect_false(isTRUE(out$feasibility$passed))
  expect_output(print(out), "INFEASIBLE")
  expect_type(feasibility(out), "logical")
  expect_true(is.list(stage1_report(out)))
})
