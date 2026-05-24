#' Round half to even (banker's rounding)
#'
#' R's `round()` already uses IEC 60559 / banker's rounding on most platforms.
#' This wrapper exists so the rounding rule is explicit at call sites where
#' it matters (ground-truth simulation in `data-raw/`, validation harness).
#'
#' @param x numeric
#' @param dp integer, decimal places
#' @noRd
round_half_even <- function(x, dp = 2L) {
  round(x, dp)
}

#' Round half up
#'
#' Many statistical software packages (SPSS, Excel) use round-half-up rather
#' than round-half-to-even. Used in ground-truth simulation to test that the
#' pipeline is robust to either rule given the ±0.005 tolerance.
#' @noRd
round_half_up <- function(x, dp = 2L) {
  z <- abs(x) * 10^dp
  sign(x) * floor(z + 0.5) / 10^dp
}

#' Compute sample skewness (Fisher's g1, identical to e1071::skewness type=1).
#' @noRd
sample_skew <- function(x) {
  n <- length(x)
  if (n < 3L) return(NA_real_)
  m <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  if (m2 <= 0) return(NA_real_)
  m3 / m2^(3/2)
}

#' Sign-aware corner products of two intervals.
#'
#' Given `[a_lo, a_hi]` and `[b_lo, b_hi]`, returns the min and max of `a*b`
#' over the rectangle. Used in interval propagation of the Cronbach α formula.
#' @noRd
interval_product <- function(a_lo, a_hi, b_lo, b_hi) {
  corners <- c(a_lo * b_lo, a_lo * b_hi, a_hi * b_lo, a_hi * b_hi)
  list(lo = min(corners), hi = max(corners))
}

#' Validate inputs to `multi_closure()`.
#'
#' Raises with informative messages; does not return anything useful.
#' @noRd
validate_inputs <- function(N, K, M, s, R, alpha = NULL) {
  if (!is.numeric(N) || length(N) != 1L || N != as.integer(N) || N < 2L)
    stop("`N` must be a positive integer >= 2.")
  if (!is.numeric(K) || length(K) != 1L || K != as.integer(K) || K < 2L)
    stop("`K` must be a positive integer >= 2 (number of response options).")
  if (!is.numeric(M) || !is.numeric(s))
    stop("`M` and `s` must be numeric vectors.")
  if (length(M) != length(s))
    stop("`M` and `s` must have the same length (one entry per item).")
  p <- length(M)
  if (p < 1L)
    stop("Need at least one item.")
  if (any(M < 1 | M > K))
    stop("Each entry of `M` must lie within [1, K].")
  if (any(s < 0))
    stop("Each entry of `s` must be non-negative.")
  if (!is.matrix(R) && !is.data.frame(R))
    stop("`R` must be a matrix.")
  R <- as.matrix(R)
  if (nrow(R) != p || ncol(R) != p)
    stop("`R` must be a p x p matrix matching the length of `M`.")
  if (!isTRUE(all.equal(R, t(R), tolerance = 1e-8)))
    stop("`R` must be symmetric.")
  if (any(abs(R) > 1 + 1e-8))
    stop("All entries of `R` must lie in [-1, 1].")
  if (any(abs(diag(R) - 1) > 1e-8))
    stop("`R` must have ones on the diagonal.")
  if (!is.null(alpha)) {
    if (!is.numeric(alpha) || length(alpha) != 1L)
      stop("`alpha` must be a single numeric value or NULL.")
  }
  invisible(TRUE)
}
