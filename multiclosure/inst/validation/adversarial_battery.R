## Adversarial battery — each case should produce a specific verdict and
## reason. Run as `Rscript adversarial_battery.R`; non-zero exit on any
## mismatch.

library(multiclosure)

results <- list()
record <- function(name, ok, detail) {
  results[[name]] <<- list(ok = ok, detail = detail)
}

contains <- function(reasons, pattern) {
  any(grepl(pattern, reasons, fixed = FALSE))
}

# A1 GRIM violation
out <- multi_closure(N = 100L, K = 7L,
                     M = c(3.71, 4.00, 5.00),
                     s = c(1.20, 1.30, 1.10),
                     R = diag(3), verbose = FALSE)
record("A1_grim",
       isFALSE(out$feasibility$passed) && contains(out$feasibility$reason, "GRIM"),
       paste(out$feasibility$reason, collapse = "; "))

# A3 Bhatia-Davis violation
out <- multi_closure(N = 80L, K = 5L,
                     M = c(2.00, 3.00, 3.50),
                     s = c(2.50, 1.00, 0.90),
                     R = matrix(c(1, .3, .2, .3, 1, .25, .2, .25, 1), 3, 3),
                     verbose = FALSE)
record("A3_bhatia_davis",
       isFALSE(out$feasibility$passed) && contains(out$feasibility$reason, "Bhatia"),
       paste(out$feasibility$reason, collapse = "; "))

# A4 Frechet violation
out <- multi_closure(N = 80L, K = 5L,
                     M = c(4.80, 1.20, 3.00),
                     s = c(0.50, 0.50, 1.20),
                     R = matrix(c(1, 0.95, 0.30,
                                  0.95, 1, 0.20,
                                  0.30, 0.20, 1), 3, 3),
                     verbose = FALSE)
record("A4_frechet",
       isFALSE(out$feasibility$passed) && contains(out$feasibility$reason, "(achievable|Frechet|pair)"),
       paste(out$feasibility$reason, collapse = "; "))

# A5 Alpha mismatch
R <- matrix(c(1, .25, .25, .25, 1, .25, .25, .25, 1), 3, 3)
out <- multi_closure(N = 100L, K = 7L,
                     M = c(4.00, 4.00, 4.00),
                     s = c(1.00, 1.00, 1.00),
                     R = R, alpha = 0.85, verbose = FALSE)
record("A5_alpha",
       isFALSE(out$feasibility$passed) && contains(out$feasibility$reason, "alpha"),
       paste(out$feasibility$reason, collapse = "; "))

ok <- vapply(results, function(r) isTRUE(r$ok), logical(1))
cat("\n--- Adversarial battery summary ---\n")
for (nm in names(results)) {
  cat(sprintf("%-20s %s\n  %s\n", nm,
              if (results[[nm]]$ok) "PASS" else "FAIL",
              results[[nm]]$detail))
}
if (!all(ok)) quit(status = 1L)
