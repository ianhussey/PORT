## Minimal end-to-end example: p=3 items, K=5, small N.
##
## Run from the package root with:
##   devtools::load_all("multiclosure")
##   source("multiclosure/examples/01_minimal.R")

library(multiclosure)

set.seed(1)

N <- 80
K <- 5
M <- c(3.20, 2.85, 3.60)
s <- c(0.95, 1.10, 0.80)
R <- matrix(c(1.00, 0.42, 0.35,
              0.42, 1.00, 0.28,
              0.35, 0.28, 1.00), 3, 3)

out <- multi_closure(
  N = N, K = K, M = M, s = s, R = R,
  alpha = NULL,
  max_candidates = 50L,
  stage4 = list(strategy = "bb_closest", max_combos = 200L,
                samples_per_combo = 1L),
  seed = 1L
)

print(out)
cat("\nFirst candidate matrix (head):\n")
if (length(candidates(out)) > 0L) print(head(candidates(out)[[1]]))
