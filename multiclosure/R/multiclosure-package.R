#' multiclosure: Multivariate CLOSURE for Forensic Reconstruction
#'
#' Reconstruct or sample integer datasets compatible with a reported
#' multivariate summary (means, SDs, correlation matrix, Cronbach's alpha)
#' on a bounded Likert-style scale.
#'
#' The user-facing entry point is [multi_closure()]. Individual stages can
#' also be invoked à la carte (see [run_stage1()], [enumerate_marginals()],
#' [bb_skew_filter()], [stage4_norta()], [verify_summaries()]).
#'
#' @keywords internal
"_PACKAGE"
