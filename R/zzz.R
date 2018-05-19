# Pkg startup message
.onAttach <- function(libname, pkgname) {
  msg <- "dpm is in-development software. You should consider cross-referencing
results with xtdpdml for Stata if possible. Go to
https://www3.nd.edu/~rwilliam/dynamic/ to learn about xtdpdml and the
underlying method."
  packageStartupMessage(msg)
}
