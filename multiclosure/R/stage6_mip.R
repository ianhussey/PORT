#' Stage 6: MIP fallback for NORTA-infeasible cases (v2 stub)
#'
#' In v1 this function is a stub. When Stage 1 Fréchet bounds pass but Stage 4
#' NORTA returns no candidates, the reported summary is achievable in
#' principle by *some* non-Gaussian copula on bounded discrete marginals; a
#' Mixed Integer Quadratically Constrained Program with integer variables
#' \eqn{X_{ij} \in \{1,...,K\}} and interval constraints on the means, SDs,
#' and correlations can search that space directly.
#'
#' Implementation deferred to v2 — requires either Gurobi (commercial / free
#' academic) or SCIP via `ROI.plugin.scip` (open-source). See `NEWS.md`.
#'
#' @inheritParams multi_closure
#' @param ... reserved
#' @return list with `feasible = NA`, `candidates = list()`,
#'   `reason = "stage6_not_implemented_in_v1"`
#' @export
mip_reconstruct <- function(N, K, M, s, R, alpha = NULL, tols, ...) {
  list(
    feasible = NA,
    candidates = list(),
    reason = "stage6_not_implemented_in_v1",
    suggested = paste(
      "Install Gurobi (free for academic use) or SCIP via",
      "ROI.plugin.scip, and use multiclosure v2 (planned)."
    )
  )
}
