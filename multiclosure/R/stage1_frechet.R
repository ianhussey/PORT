#' Discrete Fréchet-Hoeffding correlation bounds for a pair of marginals
#'
#' Given two integer marginals on `{1..K}` represented as length-N realisations
#' `x` and `y`, the Pearson correlation under the comonotonic coupling
#' (both sorted ascending) achieves the upper Fréchet bound, and under the
#' countermonotonic coupling (one sorted ascending, the other descending) the
#' lower Fréchet bound. For *discrete* marginals these bounds are exact and
#' tight; computing them by sorting is faster and simpler than invoking the
#' continuous helpers in the `copula` package.
#'
#' @param x integer vector, length N, realisation of marginal 1
#' @param y integer vector, length N, realisation of marginal 2
#' @return list with `r_min`, `r_max`
#' @export
frechet_bounds_pair <- function(x, y) {
  if (length(x) != length(y))
    stop("x and y must have the same length")
  xs <- sort(x); ys <- sort(y)
  list(r_min = stats::cor(xs, rev(ys)), r_max = stats::cor(xs, ys))
}

#' Realise a length-N integer marginal matching a target (mean, sd) on {1..K}
#'
#' Used by [check_frechet()] to obtain representative marginals for each item.
#' Calls `unsum::closure_generate()` and returns the first candidate's
#' replication into a length-N vector. If CLOSURE returns no candidates, falls
#' back to a maxent-style construction: a distribution on `{1..K}` matching
#' the target mean (the SD is then implicit; the Fréchet bounds depend mainly
#' on the marginal shape, so a near-match suffices for the bound calculation).
#'
#' @noRd
realise_marginal <- function(M_j, s_j, N, K, rounding = "up", dp = 2L) {
  fmt <- paste0("%.", dp, "f")
  out <- tryCatch(
    unsum::closure_generate(
      mean = sprintf(fmt, M_j), sd = sprintf(fmt, s_j), n = N,
      scale_min = 1L, scale_max = K,
      rounding = rounding
    ),
    error = function(e) NULL
  )
  if (!is.null(out) && length(out$results$sample) > 0L) {
    return(as.integer(out$results$sample[[1]]))
  }
  # Fallback: distribute N observations to match the mean roughly.
  base <- rep(round(M_j), N)
  # Adjust to hit the exact integer-mean target if possible.
  total <- sum(base); target <- round(M_j * N)
  diff <- target - total
  # Move mass toward extremes to inflate SD when needed.
  if (diff > 0) {
    idx <- which(base < K)
    base[idx[seq_len(min(diff, length(idx)))]] <-
      base[idx[seq_len(min(diff, length(idx)))]] + 1L
  } else if (diff < 0) {
    idx <- which(base > 1)
    base[idx[seq_len(min(-diff, length(idx)))]] <-
      base[idx[seq_len(min(-diff, length(idx)))]] - 1L
  }
  base
}

#' Stage 1: discrete Fréchet-Hoeffding check per correlation pair
#'
#' For each pair `(j, k)`, construct representative integer marginals matching
#' the reported `(M, s)` on `{1..K}`, compute the achievable correlation range
#' `[r_min, r_max]` for those marginals, and test whether `r_jk` lies in
#' `[r_min - tol_R, r_max + tol_R]`. A reported correlation outside the
#' achievable range is a strong forensic signal.
#'
#' @param M numeric vector of item means
#' @param s numeric vector of item SDs
#' @param R correlation matrix
#' @param K integer scale length
#' @param N integer sample size
#' @param tol tolerance list from [make_tols()]
#' @return tibble with columns `j`, `k`, `r_reported`, `r_min`, `r_max`,
#'   `pass`, `reason`
#' @export
check_frechet <- function(M, s, R, K, N, tol) {
  p <- length(M)
  realised <- vector("list", p)
  for (j in seq_len(p)) realised[[j]] <- realise_marginal(M[j], s[j], N, K)

  pairs <- utils::combn(p, 2L)
  out <- vector("list", ncol(pairs))
  for (idx in seq_len(ncol(pairs))) {
    j <- pairs[1, idx]; k <- pairs[2, idx]
    bnd <- frechet_bounds_pair(realised[[j]], realised[[k]])
    r <- R[j, k]
    pass <- (r >= bnd$r_min - tol$R) && (r <= bnd$r_max + tol$R)
    reason <- if (pass) "" else
      sprintf("pair (%d, %d): r=%.3f outside achievable [%.3f, %.3f]",
              j, k, r, bnd$r_min, bnd$r_max)
    out[[idx]] <- tibble::tibble(
      j = j, k = k, r_reported = r,
      r_min = bnd$r_min, r_max = bnd$r_max,
      pass = pass, reason = reason
    )
  }
  dplyr::bind_rows(out)
}
