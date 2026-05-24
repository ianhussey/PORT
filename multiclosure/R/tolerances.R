#' Construct a tolerance specification
#'
#' All checks in the pipeline are interval-based — equality is never used,
#' because reported summaries are rounded and tight equality would spuriously
#' flag valid papers as infeasible.
#'
#' At 2dp the default half-rounding tolerance is ±0.005 per scalar. The
#' Cronbach-α tolerance is generally tighter than the per-scalar SD and
#' correlation tolerances; if `tol_alpha` is left `NULL` it is derived inside
#' [check_alpha()] by interval propagation through the α formula.
#'
#' @param tol_M numeric, half-width for item means (default 0.005)
#' @param tol_s numeric, half-width for item SDs (default 0.005)
#' @param tol_R numeric, half-width for correlation entries (default 0.005)
#' @param tol_alpha numeric or NULL; if NULL, derived by propagation
#' @param dp integer, reported decimal places (informational)
#' @param rounding character, one of `"half_even"` (default) or `"half_up"`
#' @return list with named entries `M`, `s`, `R`, `alpha`, `dp`, `rounding`
#' @export
make_tols <- function(tol_M = 0.005, tol_s = 0.005, tol_R = 0.005,
                      tol_alpha = NULL, dp = 2L,
                      rounding = c("half_even", "half_up")) {
  rounding <- match.arg(rounding)
  list(M = tol_M, s = tol_s, R = tol_R, alpha = tol_alpha,
       dp = as.integer(dp), rounding = rounding)
}

#' Symmetric interval around a value
#' @param x numeric
#' @param tol non-negative numeric (scalar or same length as x)
#' @return list with `lo` and `hi` (both same shape as `x`)
#' @export
apply_tol <- function(x, tol) {
  list(lo = x - tol, hi = x + tol)
}

#' Test whether each value lies within `[lo, hi]`
#' @param value numeric
#' @param lo numeric, same length as `value`
#' @param hi numeric, same length as `value`
#' @return logical, same length as `value`
#' @export
in_interval <- function(value, lo, hi) {
  value >= lo & value <= hi
}
