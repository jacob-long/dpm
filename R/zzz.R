# Pkg startup message
.onAttach <- function(libname, pkgname) {
  msg <- "This is BETA software. Expect bugs and missing functionality.
You should cross-reference results with xtdpdml for Stata if unsure. Go to
https://www3.nd.edu/~rwilliam/dynamic/ to learn about xtdpdml and the
underlying method."
  packageStartupMessage(msg)
}
