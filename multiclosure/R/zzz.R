.onLoad <- function(libname, pkgname) {
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "multiclosure v0.1.0 — multivariate forensic CLOSURE.\n",
    "Stages 1-5 implemented. Stage 6 (MIP fallback) is a stub; see NEWS."
  )
}
