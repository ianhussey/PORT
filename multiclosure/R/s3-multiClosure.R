#' Construct a `multiClosure` object
#'
#' Internal constructor used by [multi_closure()] and the early-exit branches.
#' @noRd
new_multiClosure <- function(inputs, feasibility, stage1, marginals,
                             bb_plausibility, candidates, summary_distance,
                             diagnostics, call) {
  structure(
    list(
      inputs = inputs,
      feasibility = feasibility,
      stage1 = stage1,
      marginals = marginals,
      bb_plausibility = bb_plausibility,
      candidates = candidates,
      summary_distance = summary_distance,
      diagnostics = diagnostics,
      call = call
    ),
    class = c("multiClosure", "list")
  )
}

#' @export
print.multiClosure <- function(x, ...) {
  cat("multiClosure object\n")
  cat("Call:\n  "); print(x$call)
  inp <- x$inputs
  cat(sprintf("Inputs: N=%d, K=%d, p=%d, alpha=%s\n",
              inp$N, inp$K, length(inp$M),
              if (is.null(inp$alpha)) "NA" else format(inp$alpha)))

  verdict <- x$feasibility$passed
  verdict_str <- if (isTRUE(verdict)) "FEASIBLE"
                 else if (isFALSE(verdict)) "INFEASIBLE"
                 else "INDETERMINATE (Stage 6 not implemented in v1)"
  cat(sprintf("\nFeasibility: %s\n", verdict_str))
  if (length(x$feasibility$reason) > 0L) {
    cat("Reasons:\n")
    for (r in x$feasibility$reason) cat("  - ", r, "\n", sep = "")
  }

  if (length(x$stage1) > 0L) {
    cat("\nStage 1 pass/fail:\n")
    s1 <- x$stage1
    cat(sprintf("  GRIM:        %s\n",
                summarise_pass(s1$grim_grimmer$grim_pass)))
    cat(sprintf("  GRIMMER:     %s\n",
                summarise_pass(s1$grim_grimmer$grimmer_pass)))
    cat(sprintf("  Bhatia-Davis:%s\n",
                summarise_pass(s1$bhatia_davis$pass)))
    if (!is.na(s1$alpha$pass))
      cat(sprintf("  Alpha:       %s (implied %.3f, reported %.3f, interval [%.3f, %.3f])\n",
                  if (isTRUE(s1$alpha$pass)) "pass" else "FAIL",
                  s1$alpha$alpha_implied, s1$alpha$alpha_reported,
                  s1$alpha$alpha_lo, s1$alpha$alpha_hi))
    cat(sprintf("  Frechet:     %s\n",
                summarise_pass(s1$frechet$pass)))
  }

  if (length(x$bb_plausibility) > 0L) {
    cat("\nPer-item retention (BB skew filter):\n")
    for (j in seq_along(x$bb_plausibility)) {
      bb <- x$bb_plausibility[[j]]
      r <- bb$retention
      flag <- if (is.finite(r) && r < 0.10) " *** low ***" else ""
      cat(sprintf("  item %d: %.3f%s\n", j, r, flag))
    }
  }

  if (!is.null(x$candidates)) {
    cat(sprintf("\nCandidates returned: %d (source = %s, deduped = %s, capped = %s)\n",
                x$candidates$n_returned,
                if (is.na(x$candidates$source)) "none" else x$candidates$source,
                x$candidates$deduped, x$candidates$capped))
  }

  if (nrow(x$summary_distance) > 0L) {
    d <- x$summary_distance
    cat("Max summary distances across candidates:\n")
    cat(sprintf("  dM <= %.4f, ds <= %.4f, dR <= %.4f\n",
                max(d$dM), max(d$ds), max(d$dR)))
  }

  diag <- x$diagnostics
  if (!is.null(diag$combos_tried)) {
    cat(sprintf("\nNORTA: %d combos tried, %d failed",
                diag$combos_tried, diag$norta_failures))
    if (isTRUE(diag$stage6_recommended))
      cat(" -- consider MIP fallback (v2)")
    cat("\n")
  }
  invisible(x)
}

#' @noRd
summarise_pass <- function(x) {
  if (length(x) == 0L) return("(empty)")
  n_pass <- sum(x, na.rm = TRUE); n <- sum(!is.na(x))
  if (n == 0L) return("(skipped)")
  sprintf("%d / %d pass%s", n_pass, n,
          if (n_pass < n) " *** FAIL ***" else "")
}

#' @export
summary.multiClosure <- function(object, ...) {
  list(
    inputs = object$inputs,
    feasibility = object$feasibility,
    stage1 = object$stage1,
    bb_retention = vapply(object$bb_plausibility,
                          function(b) b$retention, numeric(1)),
    candidates = list(n = object$candidates$n_returned,
                      source = object$candidates$source),
    summary_distance_max = if (nrow(object$summary_distance) > 0L)
      list(dM = max(object$summary_distance$dM),
           ds = max(object$summary_distance$ds),
           dR = max(object$summary_distance$dR))
      else NULL,
    diagnostics = object$diagnostics
  )
}

#' @export
plot.multiClosure <- function(x, what = c("retention", "skew_bb",
                                          "distances", "frechet"), ...) {
  what <- match.arg(what)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plot.multiClosure; install it.")

  if (what == "retention") {
    df <- tibble::tibble(
      item = seq_along(x$bb_plausibility),
      retention = vapply(x$bb_plausibility,
                         function(b) b$retention, numeric(1))
    )
    ggplot2::ggplot(df, ggplot2::aes(x = factor(item), y = retention)) +
      ggplot2::geom_col() +
      ggplot2::geom_hline(yintercept = 0.10, linetype = "dashed",
                          colour = "red") +
      ggplot2::labs(x = "Item", y = "Retention fraction",
                    title = "Stage 3 — BB-skew filter retention",
                    subtitle = "Retention < 0.10 (red line) suggests reported (M, SD) atypical for BB")
  } else if (what == "skew_bb") {
    df <- dplyr::bind_rows(lapply(seq_along(x$marginals), function(j) {
      m <- x$marginals[[j]]
      if (nrow(m) == 0L) return(NULL)
      tibble::tibble(item = j, skew = m$skew,
                     skew_bb = if (isTRUE(x$bb_plausibility[[j]]$fit$valid))
                       x$bb_plausibility[[j]]$fit$skew_bb else NA_real_,
                     band_lo = x$bb_plausibility[[j]]$band[1],
                     band_hi = x$bb_plausibility[[j]]$band[2])
    }))
    ggplot2::ggplot(df, ggplot2::aes(x = factor(item), y = skew)) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.3) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = band_lo, ymax = band_hi),
                             colour = "red", width = 0.3) +
      ggplot2::geom_point(ggplot2::aes(y = skew_bb), colour = "red",
                          size = 2) +
      ggplot2::labs(x = "Item", y = "Sample skewness g1",
                    title = "BB-implied skew vs CLOSURE candidate skews")
  } else if (what == "distances") {
    d <- x$summary_distance
    if (nrow(d) == 0L) stop("No candidates to plot distances for.")
    df <- tidyr_pivot_longer(d)
    ggplot2::ggplot(df, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::facet_wrap(~ metric, scales = "free") +
      ggplot2::labs(title = "Summary distances across candidates")
  } else {
    fr <- x$stage1$frechet
    if (is.null(fr)) stop("No Frechet info available.")
    ggplot2::ggplot(fr, ggplot2::aes(x = j, y = k, fill = pass)) +
      ggplot2::geom_tile() +
      ggplot2::labs(title = "Stage 1 — Frechet bounds per pair")
  }
}

#' @noRd
tidyr_pivot_longer <- function(d) {
  tibble::tibble(
    metric = rep(c("dM", "ds", "dR"), each = nrow(d)),
    value = c(d$dM, d$ds, d$dR)
  )
}

#' @export
`[.multiClosure` <- function(x, i) {
  x$candidates$matrices <- x$candidates$matrices[i]
  x$candidates$n_returned <- length(x$candidates$matrices)
  if (nrow(x$summary_distance) > 0L)
    x$summary_distance <- x$summary_distance[i, , drop = FALSE]
  x
}

#' @export
as.data.frame.multiClosure <- function(x, row.names = NULL, optional = FALSE,
                                       ...) {
  if (length(x$candidates$matrices) == 0L)
    return(data.frame())
  rows <- lapply(seq_along(x$candidates$matrices), function(i) {
    X <- x$candidates$matrices[[i]]
    df <- as.data.frame(X)
    names(df) <- paste0("item_", seq_len(ncol(X)))
    df$cand_id <- i
    df$row_id <- seq_len(nrow(X))
    df
  })
  do.call(rbind, rows)
}

#' Accessor: list of candidate matrices
#' @export
candidates <- function(x) UseMethod("candidates")
#' @export
candidates.multiClosure <- function(x) x$candidates$matrices

#' Accessor: feasibility verdict (logical with attribute "reason")
#' @export
feasibility <- function(x) UseMethod("feasibility")
#' @export
feasibility.multiClosure <- function(x) {
  out <- x$feasibility$passed
  attr(out, "reason") <- x$feasibility$reason
  out
}

#' Accessor: per-item BB retention
#' @export
bb_retention <- function(x) UseMethod("bb_retention")
#' @export
bb_retention.multiClosure <- function(x) {
  vapply(x$bb_plausibility, function(b) b$retention, numeric(1))
}

#' Accessor: Stage 1 detailed report (bound tibble of all four sub-checks)
#' @export
stage1_report <- function(x) UseMethod("stage1_report")
#' @export
stage1_report.multiClosure <- function(x) {
  list(grim_grimmer = x$stage1$grim_grimmer,
       bhatia_davis = x$stage1$bhatia_davis,
       alpha = x$stage1$alpha,
       frechet = x$stage1$frechet)
}
