parallel_ns <- asNamespace("parallel")
if (bindingIsLocked("detectCores", parallel_ns)) {
  unlockBinding("detectCores", parallel_ns)
}
assign("detectCores", function(all.tests = FALSE, logical = FALSE) 1L, envir = parallel_ns)
lockBinding("detectCores", parallel_ns)
