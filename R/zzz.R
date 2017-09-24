# Pkg startup message
.onAttach <- function(libname, pkgname) {
  msg <- "This is ALPHA software. Expect bugs and missing functionality.
Cross-reference all results with xtdpdml for Stata. Go to
https://www3.nd.edu/~rwilliam/dynamic/ to learn about xtdpdml and the
underlying method."
  packageStartupMessage(msg)
}
