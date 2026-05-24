## Adversarial cases — each should be caught at the earliest possible stage,
## with the correct reason string.

library(multiclosure)

cases <- list()

## A1 GRIM violation
cases$grim <- list(
  N = 100L, K = 7L,
  M = c(3.71, 4.00, 5.00),
  s = c(1.20, 1.30, 1.10),
  R = diag(3)
)

## A3 Bhatia-Davis violation: K=5, M=2.00, s=2.50, max is sqrt(1*3)=1.73
cases$bhatia <- list(
  N = 80L, K = 5L,
  M = c(2.00, 3.00, 3.50),
  s = c(2.50, 1.00, 0.90),
  R = matrix(c(1, .3, .2, .3, 1, .25, .2, .25, 1), 3, 3)
)

## A4 Frechet violation: highly skewed marginals with r=0.95
cases$frechet <- list(
  N = 80L, K = 5L,
  M = c(4.80, 1.20, 3.00),
  s = c(0.50, 0.50, 1.20),
  R = matrix(c(1, 0.95, 0.30,
               0.95, 1, 0.20,
               0.30, 0.20, 1), 3, 3)
)

## A5 Alpha inconsistency
make_alpha_case <- function() {
  R <- matrix(c(1, .25, .25, .25, 1, .25, .25, .25, 1), 3, 3)
  s <- c(1.00, 1.00, 1.00)
  list(N = 100L, K = 7L,
       M = c(4.00, 4.00, 4.00),
       s = s, R = R,
       alpha = 0.85)  # implied alpha is much lower
}
cases$alpha <- make_alpha_case()

for (nm in names(cases)) {
  cat(sprintf("\n=== Adversarial case: %s ===\n", nm))
  args <- cases[[nm]]
  out <- do.call(multi_closure, c(args,
                                  list(max_candidates = 10L,
                                       verbose = FALSE)))
  cat(sprintf("Verdict: %s\n",
              switch(as.character(out$feasibility$passed),
                     "TRUE" = "FEASIBLE",
                     "FALSE" = "INFEASIBLE",
                     "NA" = "INDETERMINATE")))
  for (r in out$feasibility$reason) cat("  reason: ", r, "\n", sep = "")
}
