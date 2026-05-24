## Validation grid: simulate ground truth, round summaries to 2dp, run the
## pipeline, record outcomes. Offline driver — not run in CI.

library(multiclosure)
library(dplyr)
library(tibble)

set.seed(20260524L)

grid <- expand.grid(
  N = c(50L, 100L, 200L, 300L),
  p = c(3L, 5L, 10L),
  K = c(5L, 7L),
  skew_profile = c("symmetric", "right_skew", "ceiling"),
  rho = c(0.10, 0.30, 0.50),
  rep = seq_len(20L),
  stringsAsFactors = FALSE
)

bb_params_for_profile <- function(profile) {
  switch(profile,
         "symmetric"  = c(alpha = 3.0, beta = 3.0),
         "right_skew" = c(alpha = 2.0, beta = 5.0),
         "ceiling"    = c(alpha = 6.0, beta = 1.5))
}

simulate_one <- function(N, p, K, profile, rho) {
  R0 <- matrix(rho, p, p); diag(R0) <- 1
  L <- chol(R0)
  Z <- matrix(rnorm(N * p), N, p) %*% L
  U <- pnorm(Z)
  ab <- bb_params_for_profile(profile)
  n <- K - 1L
  pmf <- extraDistr::dbbinom(0:n, size = n, alpha = ab[1], beta = ab[2])
  cdf <- cumsum(pmf)
  X <- matrix(NA_integer_, N, p)
  for (j in seq_len(p))
    X[, j] <- as.integer(findInterval(U[, j], cdf) + 1L)
  X[X > K] <- K; X[X < 1L] <- 1L
  list(X = X,
       M = round(colMeans(X), 2L),
       s = round(apply(X, 2L, sd), 2L),
       R = round(cor(X), 2L))
}

results <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  row <- grid[i, ]
  sim <- simulate_one(row$N, row$p, row$K, row$skew_profile, row$rho)
  t0 <- Sys.time()
  out <- tryCatch(
    multi_closure(N = row$N, K = row$K, M = sim$M, s = sim$s, R = sim$R,
                  max_candidates = 100L,
                  stage4 = list(strategy = "bb_closest",
                                max_combos = 200L,
                                samples_per_combo = 1L),
                  verbose = FALSE),
    error = function(e) NULL
  )
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (is.null(out)) {
    results[[i]] <- cbind(row, feasible = NA, n_candidates = 0L,
                          norta_failures = NA, runtime_s = dt,
                          ground_truth_pass = NA)
  } else {
    gt_d <- summary_distance(sim$X, sim$M, sim$s, sim$R)
    gt_pass <- gt_d$dM <= 0.005 && gt_d$ds <= 0.005 && gt_d$dR <= 0.005
    results[[i]] <- cbind(
      row,
      feasible = as.character(out$feasibility$passed),
      n_candidates = out$candidates$n_returned,
      norta_failures = out$diagnostics$norta_failures,
      runtime_s = dt,
      ground_truth_pass = gt_pass)
  }
  if (i %% 20L == 0L) cat("done", i, "/", nrow(grid), "\n")
}
res_df <- bind_rows(lapply(results, as_tibble))
write.csv(res_df, file = "validation_grid_results.csv", row.names = FALSE)
