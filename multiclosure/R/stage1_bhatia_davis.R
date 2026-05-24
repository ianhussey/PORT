#' Stage 1: Bhatia-Davis bound per item
#'
#' On the scale `{1, ..., K}` with mean `M`, the maximum possible SD is
#' `sqrt((M - 1) * (K - M))`. Any reported SD exceeding this is impossible.
#' (Population SD; sample SD with N-1 denominator can be slightly larger when
#' N is small, but the bound is treated leniently by the ±tol_s tolerance
#' downstream.)
#'
#' @param M numeric vector of item means (length p)
#' @param s numeric vector of item SDs (length p)
#' @param K integer scale length
#' @return tibble with columns `item`, `sd_max`, `sd_reported`, `pass`,
#'   `reason`
#' @export
check_bhatia_davis <- function(M, s, K) {
  sd_max <- sqrt(pmax(0, (M - 1) * (K - M)))
  pass <- s <= sd_max + 1e-8
  tibble::tibble(
    item = seq_along(M),
    sd_max = sd_max,
    sd_reported = s,
    pass = pass,
    reason = ifelse(pass, "",
                    sprintf("item %d: SD %.3f exceeds Bhatia-Davis max %.3f",
                            seq_along(M), s, sd_max))
  )
}
