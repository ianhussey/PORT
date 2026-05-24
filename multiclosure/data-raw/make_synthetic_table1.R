## Generate a synthetic but realistic psychology Table 1 and save to
## inst/extdata/synthetic_table1.rds.
##
## Scenario: a hypothetical 6-item "ResilienceX" Likert (1-7) scale,
## N = 217. Rounded to 2dp as one would find in a published Table 1.

set.seed(20260524L)

N <- 217L
K <- 7L
p <- 6L

## Draw latent MVN, transform each column to a chosen BB marginal.
target_R <- matrix(0.40, p, p)
diag(target_R) <- 1

## Pick BB shape parameters per item with mild skew variation.
n_trials <- K - 1L
bb_params <- list(
  c(alpha = 4.0, beta = 2.0),
  c(alpha = 3.5, beta = 2.2),
  c(alpha = 4.2, beta = 1.8),
  c(alpha = 3.0, beta = 3.0),
  c(alpha = 3.8, beta = 2.4),
  c(alpha = 3.3, beta = 2.6)
)

## Draw correlated uniform via Gaussian copula.
L <- chol(target_R)
Z <- matrix(rnorm(N * p), N, p) %*% L
U <- pnorm(Z)

## Map to BB quantiles per column.
X <- matrix(NA_integer_, N, p)
for (j in seq_len(p)) {
  a <- bb_params[[j]]["alpha"]; b <- bb_params[[j]]["beta"]
  pmf <- extraDistr::dbbinom(0:n_trials, size = n_trials, alpha = a, beta = b)
  cdf <- cumsum(pmf)
  X[, j] <- as.integer(findInterval(U[, j], cdf) + 1L)
}
X[X > K] <- K
X[X < 1L] <- 1L

M <- round(colMeans(X), 2L)
s <- round(apply(X, 2L, sd), 2L)
R <- round(cor(X), 2L)
diag(R) <- 1

## Cronbach alpha
alpha_implied <- (p / (p - 1)) * (1 - sum(s^2) / sum(R * tcrossprod(s)))
alpha <- round(alpha_implied, 2L)

cat("Synthetic Table 1:\n")
print(M); print(s); print(R); cat("alpha:", alpha, "\n")

dat <- list(N = N, K = K, M = M, s = s, R = R, alpha = alpha,
            generator = list(target_R = target_R, bb_params = bb_params))

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
saveRDS(dat, "inst/extdata/synthetic_table1.rds")
