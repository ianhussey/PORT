#' Method-of-moments fit of a beta-binomial to reported (mean, SD)
#'
#' Shifts the scale `{1..K}` to `{0..K-1}` and fits a beta-binomial with
#' `n = K - 1` trials by matching the first two moments. Parameterisation:
#' \eqn{p = \alpha / (\alpha + \beta)}, \eqn{\rho = 1 / (\alpha + \beta + 1)},
#' so that \eqn{E[X] = n p} and \eqn{\mathrm{Var}[X] = n p (1-p) (1 + (n-1)\rho)}.
#'
#' The closed-form MoM solution:
#' \deqn{\hat p = \mu_0 / n,\quad
#'       \hat \rho = (v - n \hat p (1-\hat p)) / (n (n-1) \hat p (1-\hat p))}
#' \deqn{\hat\alpha = \hat p (1/\hat\rho - 1),\quad
#'       \hat\beta = (1-\hat p)(1/\hat\rho - 1)}
#'
#' Validity requires \eqn{\hat p \in (0,1)} and \eqn{\hat \rho \in (0,1)}. If
#' the reported variance is below the binomial variance (underdispersion) no
#' positive-α,β solution exists; the function returns `valid = FALSE` and
#' downstream filtering is disabled for that item (the diagnostic is still
#' recorded).
#'
#' @param mean numeric, reported mean (on the {1..K} scale)
#' @param sd numeric, reported SD
#' @param K integer scale length
#' @param N integer sample size, used for SE of sample skewness
#' @return list with `valid`, `reason`, `alpha`, `beta`, `n`, `p_hat`,
#'   `rho_hat`, `mean_shifted`, `var`, `skew_bb`
#' @export
bb_mom_fit <- function(mean, sd, K, N = NA_integer_) {
  n <- K - 1L
  mu0 <- mean - 1
  v <- sd^2
  p_hat <- mu0 / n
  if (!(p_hat > 0 && p_hat < 1)) {
    return(list(valid = FALSE,
                reason = "p_hat out of (0, 1) — mean at scale boundary",
                alpha = NA_real_, beta = NA_real_, n = n,
                p_hat = p_hat, rho_hat = NA_real_,
                mean_shifted = mu0, var = v, skew_bb = NA_real_))
  }
  bin_var <- n * p_hat * (1 - p_hat)
  if (v <= bin_var) {
    return(list(valid = FALSE,
                reason = "underdispersed vs binomial — no positive BB MoM",
                alpha = NA_real_, beta = NA_real_, n = n,
                p_hat = p_hat, rho_hat = NA_real_,
                mean_shifted = mu0, var = v, skew_bb = NA_real_))
  }
  rho_hat <- (v - bin_var) / (n * (n - 1) * p_hat * (1 - p_hat))
  if (!(rho_hat > 0 && rho_hat < 1)) {
    return(list(valid = FALSE,
                reason = "rho_hat out of (0, 1) — SD exceeds BB ceiling",
                alpha = NA_real_, beta = NA_real_, n = n,
                p_hat = p_hat, rho_hat = rho_hat,
                mean_shifted = mu0, var = v, skew_bb = NA_real_))
  }
  ab <- 1 / rho_hat - 1
  alpha <- p_hat * ab
  beta <- (1 - p_hat) * ab
  list(valid = TRUE, reason = "",
       alpha = alpha, beta = beta, n = n,
       p_hat = p_hat, rho_hat = rho_hat,
       mean_shifted = mu0, var = v,
       skew_bb = bb_skew(n, alpha, beta))
}

#' Closed-form skewness of beta-binomial
#'
#' \deqn{\gamma_1 = \frac{(\alpha + \beta + 2n)(\beta - \alpha)}{\alpha + \beta + 2}
#'       \sqrt{\frac{1 + \alpha + \beta}{n \alpha \beta (n + \alpha + \beta)}}}
#'
#' (Johnson, Kemp & Kotz, *Univariate Discrete Distributions*, 3rd ed.)
#'
#' @export
bb_skew <- function(n, alpha, beta) {
  if (alpha <= 0 || beta <= 0 || n <= 0) return(NA_real_)
  num <- (alpha + beta + 2 * n) * (beta - alpha)
  den <- alpha + beta + 2
  rad <- (1 + alpha + beta) / (n * alpha * beta * (n + alpha + beta))
  (num / den) * sqrt(rad)
}

#' Sampling SE of sample skewness g1 under the fitted BB distribution
#'
#' * `method = "analytical"` (default): compute central moments
#'   \eqn{\mu_3, \mu_4, \mu_5, \mu_6} numerically from the BB pmf over
#'   `0:(K-1)` via `extraDistr::dbbinom()`, then apply the delta-method
#'   approximation
#'   \deqn{\mathrm{Var}(g_1) \approx
#'         \frac{\mu_6 - 6\mu_4 \sigma^2 - 4\mu_3^2 + 9\sigma^6}{N \sigma^6}}
#'   (Stuart & Ord, *Kendall's Advanced Theory*, §10.13).
#' * `method = "bootstrap"`: draw `B` parametric samples of size `N` from the
#'   fitted BB; return sd of their sample skewness.
#'
#' @export
bb_skew_se <- function(fit, N, method = c("analytical", "bootstrap"), B = 500L) {
  method <- match.arg(method)
  if (!isTRUE(fit$valid)) return(NA_real_)
  n <- fit$n; alpha <- fit$alpha; beta <- fit$beta
  if (method == "analytical") {
    x <- 0:n
    p <- extraDistr::dbbinom(x, size = n, alpha = alpha, beta = beta)
    mu <- sum(x * p)
    mu2 <- sum((x - mu)^2 * p)
    mu3 <- sum((x - mu)^3 * p)
    mu4 <- sum((x - mu)^4 * p)
    mu6 <- sum((x - mu)^6 * p)
    sigma2 <- mu2
    if (sigma2 <= 0) return(NA_real_)
    var_g1 <- (mu6 - 6 * mu4 * sigma2 - 4 * mu3^2 + 9 * sigma2^3) /
              (N * sigma2^3)
    return(sqrt(max(var_g1, .Machine$double.eps)))
  }
  # bootstrap
  skews <- replicate(B, {
    smp <- extraDistr::rbbinom(N, size = n, alpha = alpha, beta = beta)
    sample_skew(smp)
  })
  stats::sd(skews, na.rm = TRUE)
}

#' Stage 3: filter per-item CLOSURE candidates by BB-implied population skew
#'
#' Retain candidates whose sample skewness lies within `band_mult` SEs of the
#' BB-implied skew. If the BB fit is invalid for an item (underdispersed,
#' boundary mean), no filtering is applied and `retention = 1`.
#'
#' @param marginals_j tibble from [enumerate_marginals()] (one item)
#' @param bb_fit_j list from [bb_mom_fit()]
#' @param N integer sample size
#' @param se_method `"analytical"` or `"bootstrap"`
#' @param band_mult numeric multiplier on the SE for the retention band
#' @return list with `kept` (filtered tibble), `retention`, `band` (lo, hi)
#' @export
bb_skew_filter <- function(marginals_j, bb_fit_j, N,
                           se_method = "analytical", band_mult = 2) {
  n_in <- nrow(marginals_j)
  if (n_in == 0L) {
    return(list(kept = marginals_j, retention = 0,
                band = c(NA_real_, NA_real_)))
  }
  if (!isTRUE(bb_fit_j$valid)) {
    return(list(kept = marginals_j, retention = 1,
                band = c(NA_real_, NA_real_)))
  }
  se <- bb_skew_se(bb_fit_j, N, method = se_method)
  if (!is.finite(se)) {
    return(list(kept = marginals_j, retention = 1,
                band = c(NA_real_, NA_real_)))
  }
  lo <- bb_fit_j$skew_bb - band_mult * se
  hi <- bb_fit_j$skew_bb + band_mult * se
  keep_idx <- which(marginals_j$skew >= lo & marginals_j$skew <= hi)
  list(kept = marginals_j[keep_idx, , drop = FALSE],
       retention = length(keep_idx) / n_in,
       band = c(lo, hi))
}
