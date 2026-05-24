# multiclosure 0.1.0

Initial release. Ships Stages 1–5 of the multivariate-CLOSURE pipeline:

* **Stage 1** — cheap impossibility certificates: GRIM, GRIMMER, Bhatia-Davis,
  Cronbach's alpha consistency, and discrete Fréchet-Hoeffding correlation
  bounds.
* **Stage 2** — per-item marginal enumeration via `unsum::closure_generate()`.
* **Stage 3** — beta-binomial method-of-moments fit per item; population-skew
  plausibility filter on each item's CLOSURE candidate set.
* **Stage 4** — NORTA joint reconstruction via `GenOrd::ordcont()` and
  `GenOrd::ordsample()`.
* **Stage 5** — verification against the full reported summary; lexicographic
  canonicalisation and deduplication of candidate matrices.

## Known limitations

* **Stage 6 (MIP fallback) is deferred to v2.** When Stage 4 NORTA fails but
  Stage 1 Fréchet passes, the orchestrator sets
  `feasibility$passed = NA` with reason `stage4_failed_consider_mip_v2`. It
  does *not* declare infeasibility — a non-Gaussian-copula reconstruction may
  still exist.
* No Bayesian fallback (`cmdstanr` / `nimble`) — planned beyond v2.
