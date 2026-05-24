#' Run all Stage 1 checks
#'
#' Calls [check_grim_grimmer()], [check_bhatia_davis()], [check_alpha()] (if
#' `alpha` is supplied), and [check_frechet()]. Returns an aggregate verdict.
#'
#' @inheritParams multi_closure
#' @param tols tolerance list from [make_tols()]
#' @return list with `passed` (logical), `reasons` (character), `details`
#'   (list with the four sub-results)
#' @export
run_stage1 <- function(N, K, M, s, R, alpha = NULL, tols) {
  gg <- check_grim_grimmer(M, s, N, K, dp = tols$dp)
  bd <- check_bhatia_davis(M, s, K)
  al <- check_alpha(R, s, alpha, N, tols)
  fr <- check_frechet(M, s, R, K, N, tols)

  reasons <- character(0)
  if (any(isFALSE(all(gg$grim_pass, na.rm = TRUE))))
    reasons <- c(reasons, gg$reason[!is.na(gg$grim_pass) & !gg$grim_pass])
  if (any(isFALSE(all(gg$grimmer_pass, na.rm = TRUE))))
    reasons <- c(reasons, gg$reason[!is.na(gg$grimmer_pass) & !gg$grimmer_pass])
  if (any(!bd$pass))
    reasons <- c(reasons, bd$reason[!bd$pass])
  if (!is.na(al$pass) && isFALSE(al$pass))
    reasons <- c(reasons, al$reason)
  if (any(!fr$pass))
    reasons <- c(reasons, fr$reason[!fr$pass])
  reasons <- reasons[nzchar(reasons)]

  passed <- length(reasons) == 0L

  list(
    passed = passed,
    reasons = reasons,
    details = list(grim_grimmer = gg, bhatia_davis = bd,
                   alpha = al, frechet = fr)
  )
}
