#' Lex-sort the rows of a candidate matrix
#'
#' Sample mean, SD, and Pearson correlation are invariant to row permutations.
#' Sorting rows lex-ascending gives a canonical representative of the
#' row-permutation equivalence class; equal canonical forms are equal
#' datasets in any forensic sense.
#'
#' @param X integer matrix (N x p)
#' @return integer matrix with rows sorted lex ascending
#' @export
canonicalise_candidate <- function(X) {
  X[do.call(order, as.data.frame(X)), , drop = FALSE]
}

#' Element-wise and summary distances between a candidate's summary and the
#' reported summary.
#'
#' @param X integer matrix
#' @param M numeric vector of reported means
#' @param s numeric vector of reported SDs
#' @param R reported correlation matrix
#' @return named list with `dM_each`, `ds_each`, `dR_upper`, and the maxima
#'   `dM`, `ds`, `dR`
#' @export
summary_distance <- function(X, M, s, R) {
  M_hat <- colMeans(X)
  s_hat <- apply(X, 2L, stats::sd)
  R_hat <- stats::cor(X)
  ut <- upper.tri(R)
  list(
    dM_each = M_hat - M,
    ds_each = s_hat - s,
    dR_upper = R_hat[ut] - R[ut],
    dM = max(abs(M_hat - M)),
    ds = max(abs(s_hat - s)),
    dR = max(abs(R_hat[ut] - R[ut]))
  )
}

#' Stage 5: verify each candidate against the full reported summary
#'
#' Retains candidates whose recomputed (M, s, R) all fall within the
#' respective reporting tolerances. NORTA-generated candidates match the
#' marginals tightly but the joint correlation matrix can drift due to
#' discretisation; expect substantial filtering here.
#'
#' @param candidates list of N x p integer matrices
#' @param M reported means
#' @param s reported SDs
#' @param R reported correlation matrix
#' @param tols tolerance list
#' @return list with `distances` (tibble), `pass_candidates` (list of
#'   matrices), `n_in`, `n_pass`
#' @export
verify_summaries <- function(candidates, M, s, R, tols) {
  if (length(candidates) == 0L)
    return(list(distances = tibble::tibble(), pass_candidates = list(),
                n_in = 0L, n_pass = 0L))
  rows <- lapply(seq_along(candidates), function(i) {
    d <- summary_distance(candidates[[i]], M, s, R)
    pass <- d$dM <= tols$M && d$ds <= tols$s && d$dR <= tols$R
    tibble::tibble(cand_id = i, pass = pass,
                   dM = d$dM, ds = d$ds, dR = d$dR)
  })
  distances <- dplyr::bind_rows(rows)
  pass_idx <- distances$cand_id[distances$pass]
  list(distances = distances,
       pass_candidates = candidates[pass_idx],
       n_in = length(candidates),
       n_pass = length(pass_idx))
}
