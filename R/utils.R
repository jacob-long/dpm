sanitize_lavaan_parallel <- function() {
  if (!is.na(suppressWarnings(parallel::detectCores()))) {
    return(invisible(NULL))
  }

  lavaan_ns <- asNamespace("lavaan")
  lavaan_cache <- get("lavaan_cache_env", envir = lavaan_ns)

  if (!exists("opt.default", envir = lavaan_cache, inherits = FALSE) ||
      !exists("opt.check", envir = lavaan_cache, inherits = FALSE)) {
    suppressWarnings(get("lav_options_default", envir = lavaan_ns)())
  }

  if (exists("opt.default", envir = lavaan_cache, inherits = FALSE)) {
    opt.default <- get("opt.default", envir = lavaan_cache)
    opt.default$ncpus <- 1L
    assign("opt.default", opt.default, envir = lavaan_cache)
  }

  if (exists("opt.check", envir = lavaan_cache, inherits = FALSE)) {
    opt.check <- get("opt.check", envir = lavaan_cache)
    if (!is.null(opt.check$ncpus$nm$bounds)) {
      opt.check$ncpus$nm$bounds <- c(1L, 1L)
    }
    assign("opt.check", opt.check, envir = lavaan_cache)
  }

  invisible(NULL)
}

make_ci_labs <- function(ci.width) {

  alpha <- (1 - ci.width) / 2

  lci_lab <- 0 + alpha
  lci_lab <- paste(round(lci_lab * 100, 1), "%", sep = "")

  uci_lab <- 1 - alpha
  uci_lab <- paste(round(uci_lab * 100, 1), "%", sep = "")

  list(lci = lci_lab, uci = uci_lab)

}

concat <- function(input) {

  input <- unlist(input)

  if (length(input) > 1) {

    for (i in 2:length(input)) {

      input[i] <- paste(input[i - 1], input[i], sep = "\n")

    }

  }

  return(input[length(input)])

}

concat2 <- function(input) {

  input <- unlist(input)

  if (length(input) > 1) {

    for (i in 2:length(input)) {

      input[i] <- paste(input[i - 1], input[i], sep = "\n\n")

    }

  }

  return(input[length(input)])

}

make_names <- function(names, int = FALSE) {
  # Ensure valid leading character
  names <- sub('^[^A-Za-z\\.]+', '.', names)
  # See where dots were originally
  dots <- lapply(names, function(x) which(strsplit(x, NULL)[[1]] == "."))
  if (int == TRUE) {
    names <- gsub(":|\\*", "_by_", names)
  }
  # Use make.names
  names <- make.names(names, allow_ = TRUE)
  # substitute _ for .
  names <- gsub( '\\.', '_', names )
  # Now add the original periods back in
  mapply(names, dots, FUN = function(x, y) {
    x <- strsplit(x, NULL)[[1]]
    x[y] <- "."
    paste0(x, collapse = "")
  })
}

bt <- function(x) {
  if (!is.null(x)) {
    btv <- paste0("`", x, "`")
    btv <- gsub("``", "`", btv, fixed = TRUE)
    btv <- btv %not% c("", "`")
  } else btv <- NULL
  return(btv)
}

un_bt <- function(x) {
  gsub("`", "", x)
}
