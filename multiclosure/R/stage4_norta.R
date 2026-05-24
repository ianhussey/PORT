#' Choose marginal combinations to feed into NORTA
#'
#' The cross-product of per-item filtered candidates can be astronomical. This
#' helper picks at most `max_combos` combinations using one of:
#'
#' * `"bb_closest"` (default): for each item, take the top
#'   `k_j = max(1, floor(max_combos^(1/p)))` candidates ranked by
#'   `|skew - skew_bb|`. Form the cross-product; shuffle.
#' * `"random"`: sample uniformly at random from the per-item filtered
#'   candidate pools (without replacement) up to `max_combos`.
#' * `"stratified"`: stratify each item's candidates into `n_strata` skew
#'   quantile bins and Latin-hypercube-sample across items.
#' * `"full"`: enumerate the Cartesian product; error if size > `max_combos`.
#'
#' At `p` around 20, `"bb_closest"` collapses to `k_j = 1`, giving a single
#' combination (one marginal per item). The resulting forensic conclusion is
#' a *lower bound* on feasibility: failure means even the most BB-consistent
#' marginals could not reproduce the reported R.
#'
#' @param filtered_marginals list of length p of tibbles from
#'   [bb_skew_filter()]
#' @param R correlation matrix
#' @param bb_fits list of length p from [bb_mom_fit()]
#' @param strategy character
#' @param max_combos integer cap
#' @param n_strata integer, used by `"stratified"`
#' @param seed integer or NULL, for shuffling
#' @return tibble with columns `combo_id`, `item_1`, ..., `item_p` (each
#'   referring to a `candidate_id` within that item's tibble)
#' @export
choose_marginal_combos <- function(filtered_marginals, R, bb_fits,
                                   strategy = c("bb_closest", "random",
                                                "stratified", "full"),
                                   max_combos = 1000L,
                                   n_strata = 5L,
                                   seed = NULL) {
  strategy <- match.arg(strategy)
  if (!is.null(seed)) set.seed(seed)
  p <- length(filtered_marginals)
  card <- vapply(filtered_marginals, nrow, integer(1))
  if (any(card == 0L))
    return(tibble::tibble())

  if (strategy == "bb_closest") {
    k_j <- max(1L, as.integer(floor(max_combos^(1 / p))))
    per_item <- lapply(seq_len(p), function(j) {
      tbl <- filtered_marginals[[j]]
      target <- if (isTRUE(bb_fits[[j]]$valid)) bb_fits[[j]]$skew_bb else 0
      ord <- order(abs(tbl$skew - target))
      tbl$candidate_id[ord][seq_len(min(k_j, nrow(tbl)))]
    })
    grid <- as.matrix(do.call(expand.grid, c(per_item, list(KEEP.OUT.ATTRS = FALSE))))
    if (nrow(grid) > max_combos) grid <- grid[sample.int(nrow(grid), max_combos), , drop = FALSE]
    grid <- grid[sample.int(nrow(grid)), , drop = FALSE]
  } else if (strategy == "random") {
    grid <- matrix(NA_integer_, nrow = max_combos, ncol = p)
    seen <- character(0)
    n_filled <- 0L
    attempts <- 0L
    while (n_filled < max_combos && attempts < max_combos * 10L) {
      attempts <- attempts + 1L
      pick <- vapply(seq_len(p), function(j) {
        sample(filtered_marginals[[j]]$candidate_id, 1L)
      }, integer(1))
      key <- paste(pick, collapse = "_")
      if (!key %in% seen) {
        seen <- c(seen, key)
        n_filled <- n_filled + 1L
        grid[n_filled, ] <- pick
      }
    }
    grid <- grid[seq_len(n_filled), , drop = FALSE]
  } else if (strategy == "stratified") {
    per_item <- lapply(seq_len(p), function(j) {
      tbl <- filtered_marginals[[j]]
      sk <- tbl$skew
      bins <- if (length(unique(sk)) >= n_strata)
        as.integer(cut(sk, n_strata, labels = FALSE)) else
        rep(1L, length(sk))
      split(tbl$candidate_id, bins)
    })
    grid <- matrix(NA_integer_, nrow = max_combos, ncol = p)
    for (i in seq_len(max_combos)) {
      for (j in seq_len(p)) {
        strata <- per_item[[j]]
        s <- sample(seq_along(strata), 1L)
        grid[i, j] <- sample(strata[[s]], 1L)
      }
    }
  } else { # full
    if (prod(as.numeric(card)) > max_combos)
      stop(sprintf("Full cross-product has %g combos > max_combos=%d",
                   prod(as.numeric(card)), max_combos))
    per_item <- lapply(filtered_marginals, function(tbl) tbl$candidate_id)
    grid <- as.matrix(do.call(expand.grid, c(per_item, list(KEEP.OUT.ATTRS = FALSE))))
  }

  colnames(grid) <- paste0("item_", seq_len(p))
  tibble::as_tibble(grid) |> dplyr::mutate(combo_id = dplyr::row_number()) |>
    dplyr::relocate(combo_id)
}

#' Run NORTA for a single combination of marginals
#'
#' Computes the latent Gaussian correlation matrix that produces the target
#' ordinal correlation matrix `R_target` under the given marginals, via
#' `GenOrd::ordcont()`. If `ordcont()` cannot find a feasible latent matrix
#' (target correlation is unrealisable by a Gaussian copula on these
#' marginals), returns `feasible = FALSE` — itself a forensic signal.
#'
#' @param marginal_cdfs list of length p; each a length-K CDF vector
#' @param R_target correlation matrix
#' @param N integer sample size
#' @param K integer scale length
#' @param samples_per_combo integer, how many draws of `ordsample()` to take
#' @return list with `feasible`, `candidates` (list of N x p integer
#'   matrices), `latent_R`, `reason`
#' @export
norta_reconstruct <- function(marginal_cdfs, R_target, N, K,
                              samples_per_combo = 1L) {
  p <- length(marginal_cdfs)
  marginal_trunc <- lapply(marginal_cdfs, function(cdf) cdf[-length(cdf)])
  support <- rep(list(seq_len(K)), p)

  Lz <- tryCatch(
    GenOrd::ordcont(marginal = marginal_trunc, Sigma = R_target,
                    support = support, Spearman = FALSE),
    error = function(e) list(error = conditionMessage(e))
  )
  if (!is.null(Lz$error)) {
    return(list(feasible = FALSE, candidates = list(),
                latent_R = NULL,
                reason = paste("ordcont:", Lz$error)))
  }
  # GenOrd::ordcont returns the intermediate Gaussian correlation matrix
  # in $SigmaC (older versions) or $Sigma (some forks). Prefer SigmaC.
  latent_R <- if (!is.null(Lz$SigmaC)) Lz$SigmaC else Lz$Sigma
  if (is.null(latent_R)) {
    return(list(feasible = FALSE, candidates = list(),
                latent_R = NULL,
                reason = "ordcont returned no latent correlation matrix"))
  }
  cands <- vector("list", samples_per_combo)
  for (b in seq_len(samples_per_combo)) {
    X <- tryCatch(
      GenOrd::ordsample(n = N, marginal = marginal_trunc,
                        Sigma = latent_R, support = support,
                        Spearman = FALSE),
      error = function(e) NULL
    )
    if (is.null(X)) {
      return(list(feasible = FALSE, candidates = list(),
                  latent_R = latent_R,
                  reason = "ordsample failed"))
    }
    cands[[b]] <- matrix(as.integer(X), nrow = N, ncol = p)
  }
  list(feasible = TRUE, candidates = cands,
       latent_R = latent_R, reason = "")
}

#' Stage 4: drive NORTA over chosen marginal combinations
#'
#' Iterates [norta_reconstruct()] over the combos produced by
#' [choose_marginal_combos()]. Stops early when `length(verified)` reaches
#' `max_candidates` (after Stage 5).
#'
#' @inheritParams choose_marginal_combos
#' @param N integer sample size
#' @param K integer scale length
#' @param tols tolerance list
#' @param samples_per_combo integer
#' @param max_candidates integer cap on verified candidates
#' @return list with `candidates` (list of N x p integer matrices),
#'   `norta_failures`, `combos_tried`, `latent_R_examples`
#' @export
stage4_norta <- function(filtered_marginals, R, N, K, bb_fits, tols,
                         strategy = "bb_closest", max_combos = 1000L,
                         samples_per_combo = 1L,
                         max_candidates = 1000L,
                         seed = NULL) {
  combos <- choose_marginal_combos(filtered_marginals, R, bb_fits,
                                   strategy = strategy,
                                   max_combos = max_combos, seed = seed)
  if (nrow(combos) == 0L)
    return(list(candidates = list(), norta_failures = 0L,
                combos_tried = 0L, latent_R_examples = list()))

  p <- length(filtered_marginals)
  all_cands <- list()
  failures <- 0L
  latent_examples <- list()

  for (i in seq_len(nrow(combos))) {
    cdfs <- lapply(seq_len(p), function(j) {
      cid <- combos[[paste0("item_", j)]][i]
      row <- filtered_marginals[[j]][filtered_marginals[[j]]$candidate_id == cid, ]
      row$cdf[[1]]
    })
    out <- norta_reconstruct(cdfs, R, N, K, samples_per_combo)
    if (!out$feasible) {
      failures <- failures + 1L
      next
    }
    all_cands <- c(all_cands, out$candidates)
    if (length(latent_examples) < 3L)
      latent_examples <- c(latent_examples, list(out$latent_R))
    if (length(all_cands) >= max_candidates * 2L) break
  }

  list(candidates = all_cands,
       norta_failures = failures,
       combos_tried = nrow(combos),
       latent_R_examples = latent_examples)
}
