#' Compute Cronbach's alpha from a correlation matrix and SD vector
#'
#' Uses the unstandardised formula
#' \deqn{\alpha = \frac{p}{p-1} \left(1 - \frac{\sum_j s_j^2}{V}\right)}
#' where \eqn{V = \sum_j s_j^2 + 2 \sum_{j<k} r_{jk} s_j s_k} is the total
#' variance of the summed score implied by the reported R and s.
#'
#' @param R correlation matrix
#' @param s vector of item SDs
#' @return scalar alpha
#' @export
alpha_from_R_s <- function(R, s) {
  p <- length(s)
  if (p < 2L) return(NA_real_)
  ss <- s^2
  cov_mat <- R * tcrossprod(s)
  V <- sum(cov_mat)
  (p / (p - 1)) * (1 - sum(ss) / V)
}

#' Stage 1: Cronbach's alpha consistency check
#'
#' Compute the α implied by the reported `R` and `s`, then check whether the
#' reported `alpha_reported` lies within the implied α interval. The interval
#' bounds are obtained by sign-aware interval propagation of the reporting
#' tolerances through the α formula:
#'
#' * Each \eqn{s_j^2} lies in `[(s_j - tol_s)^2, (s_j + tol_s)^2]`.
#' * Each off-diagonal covariance \eqn{r_{jk} s_j s_k} lies in the (sign-aware)
#'   product interval implied by `r_jk ± tol_R` and `s_j, s_k ± tol_s`.
#' * The α expression is monotone in \eqn{\sum s_j^2} (decreasing) and in
#'   \eqn{V} (increasing), so the α bounds follow directly.
#'
#' Mismatch is a forensic signal: it indicates `R` and `s` were not computed
#' from the same data, or that `alpha_reported` was computed from a different
#' covariance / scoring rule (e.g. with imputed items).
#'
#' @param R correlation matrix (p x p)
#' @param s vector of item SDs (length p)
#' @param alpha_reported scalar reported α (may be NULL — function returns
#'   pass = NA in that case)
#' @param N integer sample size (currently unused, kept for signature symmetry)
#' @param tol tolerance list from [make_tols()]
#' @return list with `alpha_implied`, `alpha_lo`, `alpha_hi`, `alpha_reported`,
#'   `pass`, `reason`
#' @export
check_alpha <- function(R, s, alpha_reported, N, tol) {
  if (is.null(alpha_reported)) {
    return(list(
      alpha_implied = alpha_from_R_s(R, s),
      alpha_lo = NA_real_, alpha_hi = NA_real_,
      alpha_reported = NA_real_,
      pass = NA, reason = "no alpha reported; check skipped"
    ))
  }

  p <- length(s)
  alpha_implied <- alpha_from_R_s(R, s)

  s_lo <- pmax(0, s - tol$s)
  s_hi <- s + tol$s
  ss_lo <- s_lo^2
  ss_hi <- s_hi^2

  # Total variance V = sum_j s_j^2 + 2 sum_{j<k} r_jk s_j s_k.
  # Interval on each r_jk s_j s_k via sign-aware corner products.
  V_lo <- sum(ss_lo)
  V_hi <- sum(ss_hi)
  for (j in seq_len(p - 1L)) {
    for (k in (j + 1L):p) {
      r_lo <- R[j, k] - tol$R
      r_hi <- R[j, k] + tol$R
      sjsk_lo <- s_lo[j] * s_lo[k]
      sjsk_hi <- s_hi[j] * s_hi[k]
      ip <- interval_product(r_lo, r_hi, sjsk_lo, sjsk_hi)
      V_lo <- V_lo + 2 * ip$lo
      V_hi <- V_hi + 2 * ip$hi
    }
  }

  if (V_lo <= 0 || V_hi <= 0) {
    return(list(
      alpha_implied = alpha_implied,
      alpha_lo = NA_real_, alpha_hi = NA_real_,
      alpha_reported = alpha_reported,
      pass = FALSE,
      reason = "alpha interval undefined (total variance interval crosses 0)"
    ))
  }

  alpha_lo <- (p / (p - 1)) * (1 - sum(ss_hi) / V_lo)
  alpha_hi <- (p / (p - 1)) * (1 - sum(ss_lo) / V_hi)
  if (alpha_lo > alpha_hi) {
    tmp <- alpha_lo; alpha_lo <- alpha_hi; alpha_hi <- tmp
  }

  pass <- isTRUE(alpha_reported >= alpha_lo && alpha_reported <= alpha_hi)
  reason <- if (pass) "" else
    sprintf("alpha mismatch: reported %.3f outside implied interval [%.3f, %.3f]",
            alpha_reported, alpha_lo, alpha_hi)

  list(
    alpha_implied = alpha_implied,
    alpha_lo = alpha_lo, alpha_hi = alpha_hi,
    alpha_reported = alpha_reported,
    pass = pass, reason = reason
  )
}
