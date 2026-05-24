#' Multivariate CLOSURE: forensic reconstruction of bounded-integer datasets
#'
#' Given a reported multivariate summary on a Likert-style scale, produce a
#' `multiClosure` object describing whether the summary is feasible, and (if
#' so) a sample of integer datasets compatible with it.
#'
#' The pipeline runs in six stages, cheap impossibility checks first:
#'
#' 1. Stage 1: GRIM / GRIMMER, Bhatia-Davis, Cronbach α consistency,
#'    Fréchet-Hoeffding correlation bounds.
#' 2. Stage 2: per-item CLOSURE enumeration via `unsum::closure_generate()`.
#' 3. Stage 3: beta-binomial population-skew plausibility filter.
#' 4. Stage 4: NORTA joint reconstruction via `GenOrd::ordcont` / `ordsample`.
#' 5. Stage 5: verification against full reported summary, canonicalisation,
#'    deduplication.
#' 6. Stage 6: MIP fallback — *deferred to v2*. When Stage 4 fails but Stage 1
#'    Fréchet passes, `feasibility$passed` is set to `NA` (not `FALSE`).
#'
#' @param N integer sample size
#' @param K integer scale length (number of integer response options)
#' @param M numeric vector of reported item means (length p)
#' @param s numeric vector of reported item SDs (length p)
#' @param R reported p x p correlation matrix
#' @param alpha numeric or NULL, reported Cronbach's α
#' @param tol_M,tol_s,tol_R reporting tolerances (default 0.005 for 2dp)
#' @param tol_alpha optional explicit α tolerance; if NULL, derived
#' @param dp integer, decimal places of reported values
#' @param rounding `"half_even"` or `"half_up"`
#' @param max_candidates integer cap on returned candidate matrices
#' @param stage3 list of options for Stage 3
#' @param stage4 list of options for Stage 4
#' @param stage6 list of options for Stage 6 (currently `enable = FALSE`)
#' @param dedup logical, whether to lex-canonicalise + dedup candidates
#' @param verbose logical
#' @param seed integer or NULL
#' @return a `multiClosure` S3 object
#' @export
multi_closure <- function(N, K, M, s, R,
                          alpha = NULL,
                          tol_M = 0.005, tol_s = 0.005, tol_R = 0.005,
                          tol_alpha = NULL,
                          dp = 2L,
                          rounding = c("half_even", "half_up"),
                          max_candidates = 1000L,
                          stage3 = list(se_method = "analytical",
                                        band_mult = 2),
                          stage4 = list(strategy = "bb_closest",
                                        max_combos = 1000L,
                                        samples_per_combo = 1L),
                          stage6 = list(enable = FALSE),
                          dedup = TRUE,
                          verbose = TRUE,
                          seed = NULL) {
  call_ <- match.call()
  rounding <- match.arg(rounding)
  validate_inputs(N, K, M, s, R, alpha)
  R <- as.matrix(R); p <- length(M)
  if (!is.null(seed)) set.seed(seed)
  tols <- make_tols(tol_M, tol_s, tol_R, tol_alpha, dp = dp,
                    rounding = rounding)

  if (verbose) message("Stage 1: cheap impossibility certificates")
  s1 <- run_stage1(N, K, M, s, R, alpha, tols)

  if (!s1$passed) {
    return(new_multiClosure(
      inputs = list(N = N, K = K, M = M, s = s, R = R, alpha = alpha,
                    tols = tols, dp = dp, rounding = rounding),
      feasibility = list(passed = FALSE, reason = s1$reasons),
      stage1 = s1$details,
      marginals = list(), bb_plausibility = list(),
      candidates = list(matrices = list(), source = NA_character_,
                        n_returned = 0L, capped = FALSE, deduped = dedup),
      summary_distance = tibble::tibble(),
      diagnostics = list(combos_tried = 0L, norta_failures = 0L,
                         stage6_recommended = FALSE, seed = seed),
      call = call_))
  }

  if (verbose) message("Stage 2: per-item CLOSURE enumeration")
  closure_rounding <- switch(rounding, half_up = "up", half_even = "even", "up")
  marginals <- enumerate_marginals(M, s, N, K, dp = dp,
                                   rounding = closure_rounding)
  empty_items <- vapply(marginals, function(x) nrow(x) == 0L, logical(1))
  if (any(empty_items)) {
    reasons <- sprintf("item %d: CLOSURE returned no candidates",
                       which(empty_items))
    return(new_multiClosure(
      inputs = list(N = N, K = K, M = M, s = s, R = R, alpha = alpha,
                    tols = tols, dp = dp, rounding = rounding),
      feasibility = list(passed = FALSE, reason = reasons),
      stage1 = s1$details, marginals = marginals,
      bb_plausibility = list(),
      candidates = list(matrices = list(), source = NA_character_,
                        n_returned = 0L, capped = FALSE, deduped = dedup),
      summary_distance = tibble::tibble(),
      diagnostics = list(combos_tried = 0L, norta_failures = 0L,
                         stage6_recommended = FALSE, seed = seed),
      call = call_))
  }

  if (verbose) message("Stage 3: beta-binomial plausibility filter")
  bb_fits <- lapply(seq_len(p), function(j) bb_mom_fit(M[j], s[j], K, N))
  filtered_list <- lapply(seq_len(p), function(j) {
    bb_skew_filter(marginals[[j]], bb_fits[[j]], N,
                   se_method = stage3$se_method,
                   band_mult = stage3$band_mult)
  })
  filtered_marginals <- lapply(filtered_list, function(x) x$kept)
  bb_plausibility <- lapply(seq_len(p), function(j) {
    list(fit = bb_fits[[j]],
         retention = filtered_list[[j]]$retention,
         band = filtered_list[[j]]$band,
         kept_ids = filtered_marginals[[j]]$candidate_id)
  })

  if (verbose) message("Stage 4: NORTA joint reconstruction")
  s4 <- stage4_norta(filtered_marginals, R, N, K, bb_fits, tols,
                     strategy = stage4$strategy,
                     max_combos = stage4$max_combos,
                     samples_per_combo = stage4$samples_per_combo,
                     max_candidates = max_candidates,
                     seed = seed)

  if (verbose) message("Stage 5: verify against full reported summary")
  v5 <- verify_summaries(s4$candidates, M, s, R, tols)
  pass_candidates <- v5$pass_candidates

  if (dedup && length(pass_candidates) > 1L) {
    canon <- lapply(pass_candidates, canonicalise_candidate)
    keys <- vapply(canon, function(X) paste(as.integer(X), collapse = ","),
                   character(1))
    keep_idx <- !duplicated(keys)
    pass_candidates <- canon[keep_idx]
  } else if (dedup && length(pass_candidates) == 1L) {
    pass_candidates <- list(canonicalise_candidate(pass_candidates[[1]]))
  }

  capped <- length(pass_candidates) > max_candidates
  if (capped) pass_candidates <- pass_candidates[seq_len(max_candidates)]

  stage6_recommended <- length(pass_candidates) == 0L && s1$passed
  feasibility <- if (length(pass_candidates) > 0L) {
    list(passed = TRUE, reason = character(0))
  } else if (stage6_recommended) {
    list(passed = NA,
         reason = "stage4_failed_consider_mip_v2 — Stage 1 passed but no NORTA candidate verified")
  } else {
    list(passed = FALSE,
         reason = "no candidate verified, Stage 1 should have caught this")
  }

  new_multiClosure(
    inputs = list(N = N, K = K, M = M, s = s, R = R, alpha = alpha,
                  tols = tols, dp = dp, rounding = rounding),
    feasibility = feasibility,
    stage1 = s1$details,
    marginals = marginals,
    bb_plausibility = bb_plausibility,
    candidates = list(matrices = pass_candidates,
                      source = if (length(pass_candidates) > 0L) "norta" else NA_character_,
                      n_returned = length(pass_candidates),
                      capped = capped, deduped = dedup),
    summary_distance = v5$distances,
    diagnostics = list(combos_tried = s4$combos_tried,
                       norta_failures = s4$norta_failures,
                       stage6_recommended = stage6_recommended,
                       seed = seed,
                       latent_R_examples = s4$latent_R_examples),
    call = call_)
}
