# multiclosure

Multivariate extension of CLOSURE for forensic reconstruction of bounded-integer
datasets.

Given the summary statistics typically reported in a psychology paper for
Likert-style data — sample size `N`, scale length `K`, per-item means `M`, SDs
`s`, correlation matrix `R`, optionally Cronbach's α — `multi_closure()`
enumerates or samples integer datasets compatible with those summaries on the
stated bounded scale.

The tool is designed for **forensic** use:

* Detect papers whose reported summaries are *infeasible* (no integer dataset
  on the stated scale could produce them).
* Detect *internal inconsistency* (e.g. α incompatible with `R` and `s`).
* Test whether claimed inferences can be reproduced on *any* dataset consistent
  with the published summary.

## Pipeline

1. **Cheap impossibility certificates** — GRIM, GRIMMER (`scrutiny`),
   Bhatia-Davis per item, Cronbach's α consistency, discrete Fréchet-Hoeffding
   bounds per correlation pair.
2. **Per-item CLOSURE enumeration** via `unsum::closure_generate()`.
3. **Beta-binomial population-plausibility filter.** For each item, fit a
   beta-binomial by method of moments to the reported (mean, SD); retain
   CLOSURE candidates whose sample skew lies within a sampling-variability
   band around the BB-implied population skew.
4. **NORTA joint reconstruction** via `GenOrd::ordcont()` (latent Gaussian
   correlation) and `GenOrd::ordsample()`.
5. **Verification** against the full reported summary, with lexicographic
   canonicalisation and deduplication of candidates.
6. **MIP fallback** — *deferred to v2.*

All comparisons are interval-based using reporting-precision tolerances
(default ±0.005 for 2dp inputs). Equality is never used.

## Status

Prototype v0.1.0. Target operating range: `p ≤ 20` items, `N ≤ 300`. Stages 1–5
implemented; Stage 6 is a stub. See `NEWS.md`.

## Installation

```r
# from a local clone
devtools::install("multiclosure")
```

Dependencies: `unsum`, `scrutiny`, `GenOrd`, `extraDistr`, `tibble`, `dplyr`,
`purrr`. The `copula` package is Suggested (not currently required by the
implementation — the discrete Fréchet bounds are computed by direct sort-based
coupling).

## Quickstart

```r
library(multiclosure)

out <- multi_closure(
  N = 217, K = 7,
  M = c(5.12, 4.88, 5.30, 4.50, 5.05, 4.72),
  s = c(1.10, 1.25, 0.95, 1.40, 1.05, 1.30),
  R = readRDS(system.file("extdata", "synthetic_table1.rds",
                          package = "multiclosure"))$R,
  alpha = 0.82
)
print(out)
plot(out, what = "retention")
candidates(out)[[1]]
```

See `vignettes/multiclosure-walkthrough.Rmd` for a full walk-through, including
adversarial variants that fail at each stage.
