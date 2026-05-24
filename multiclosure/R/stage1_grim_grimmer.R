#' Stage 1: GRIM and GRIMMER checks
#'
#' GRIM checks granularity consistency of a reported mean given N. GRIMMER
#' extends this to the (mean, SD) pair. Both are computed per item via the
#' `scrutiny` package.
#'
#' The reported values are passed to scrutiny as character strings preserving
#' the number of reported decimals — this is required by `scrutiny::grim_map`
#' and `scrutiny::grimmer_map` so they can infer the rounding granularity.
#'
#' @param M numeric vector of reported item means (length p)
#' @param s numeric vector of reported item SDs (length p)
#' @param N integer sample size
#' @param K integer scale length (1..K). Not used by GRIM/GRIMMER directly but
#'   kept in the signature for symmetry with other Stage-1 checks.
#' @param dp integer, decimal places of the reported summaries
#' @return tibble with one row per item: columns `item`, `mean_reported`,
#'   `sd_reported`, `grim_pass`, `grimmer_pass`, `reason`
#' @export
check_grim_grimmer <- function(M, s, N, K, dp = 2L) {
  p <- length(M)
  fmt <- paste0("%.", dp, "f")
  M_chr <- sprintf(fmt, M)
  s_chr <- sprintf(fmt, s)

  grim_df <- tryCatch(
    scrutiny::grim_map(
      tibble::tibble(x = M_chr, n = rep(N, p)),
      items = 1L
    ),
    error = function(e) NULL
  )
  grimmer_df <- tryCatch(
    scrutiny::grimmer_map(
      tibble::tibble(x = M_chr, sd = s_chr, n = rep(N, p)),
      items = 1L
    ),
    error = function(e) NULL
  )

  grim_pass <- if (is.null(grim_df)) rep(NA, p) else as.logical(grim_df$consistency)
  grimmer_pass <- if (is.null(grimmer_df)) rep(NA, p) else as.logical(grimmer_df$consistency)

  reason <- vapply(seq_len(p), function(j) {
    bad_grim <- isTRUE(!grim_pass[j])
    bad_grimmer <- isTRUE(!grimmer_pass[j])
    if (bad_grim && bad_grimmer)
      sprintf("item %d: GRIM and GRIMMER violated", j)
    else if (bad_grim)
      sprintf("item %d: GRIM violated", j)
    else if (bad_grimmer)
      sprintf("item %d: GRIMMER violated", j)
    else ""
  }, character(1))

  tibble::tibble(
    item = seq_len(p),
    mean_reported = M,
    sd_reported = s,
    grim_pass = grim_pass,
    grimmer_pass = grimmer_pass,
    reason = reason
  )
}
