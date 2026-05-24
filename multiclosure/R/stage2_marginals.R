#' Convert a count vector to a length-K CDF
#'
#' @param counts integer vector of length K, summing to N
#' @param K integer scale length
#' @return numeric vector length K, cumulative probabilities
#' @export
marginal_cdf <- function(counts, K) {
  cumsum(counts) / sum(counts)
}

#' Stage 2: per-item marginal enumeration via CLOSURE
#'
#' For each item, call `unsum::closure_generate()` to enumerate the set of
#' integer samples on `{1..K}` of length `N` with the reported mean and SD.
#' Each candidate's length-N integer sample is converted to a count vector,
#' pmf, and cdf; sample mean, SD, and skew are also recorded.
#'
#' Note: `unsum::closure_generate()` requires `mean` and `sd` as **character**
#' strings (to preserve trailing zeros and infer the reported granularity).
#' This wrapper formats `M` and `s` accordingly via [sprintf()] with `dp`
#' decimal places.
#'
#' @inheritParams multi_closure
#' @param dp integer, decimal places of the reported summaries
#' @param rounding character, passed to `unsum::closure_generate()`
#' @return list of length p; each element is a tibble with columns
#'   `candidate_id`, `counts` (list-column of length-K integer vectors), `pmf`,
#'   `cdf`, `mean`, `sd`, `skew`.
#' @export
enumerate_marginals <- function(M, s, N, K, dp = 2L, rounding = "up") {
  p <- length(M)
  out <- vector("list", p)
  fmt <- paste0("%.", dp, "f")

  for (j in seq_len(p)) {
    res <- tryCatch(
      unsum::closure_generate(
        mean = sprintf(fmt, M[j]),
        sd = sprintf(fmt, s[j]),
        n = N,
        scale_min = 1L, scale_max = K,
        rounding = rounding
      ),
      error = function(e) {
        warning(sprintf("closure_generate failed for item %d: %s",
                        j, conditionMessage(e)))
        NULL
      }
    )

    samples <- if (is.null(res)) list() else res$results$sample
    if (length(samples) == 0L) {
      out[[j]] <- tibble::tibble(
        candidate_id = integer(0),
        counts = list(), pmf = list(), cdf = list(),
        mean = numeric(0), sd = numeric(0), skew = numeric(0)
      )
      next
    }

    rows <- lapply(seq_along(samples), function(i) {
      x <- as.integer(samples[[i]])
      counts <- as.integer(tabulate(x, nbins = K))
      pmf <- counts / sum(counts)
      cdf <- cumsum(pmf)
      tibble::tibble(
        candidate_id = i,
        counts = list(counts),
        pmf = list(pmf),
        cdf = list(cdf),
        mean = mean(x),
        sd = stats::sd(x),
        skew = sample_skew(x)
      )
    })
    out[[j]] <- dplyr::bind_rows(rows)
  }
  out
}
