
### Taken from jtools
round_df_char <- function(df, digits, pad = " ", na_vals = NA) {

  nas <- is.na(df)
  if (!is.data.frame(df)) {
    # Fixes a sneaky error
    df <- as.data.frame.matrix(df, stringsAsFactors = FALSE)

  }

  rn <- rownames(df)
  cn <- colnames(df)

  df <- as.data.frame(lapply(df, function(col) {
    if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
      as.numeric(as.character(col))
    } else {
      col
    }
  }), stringsAsFactors = FALSE)

  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  # Using a format function here to force trailing zeroes to be printed
  # "formatC" allows signed zeros (e.g., "-0.00")
  df <- as.data.frame(lapply(df, num_print, digits = digits),
                      stringsAsFactors = FALSE)

  # Convert missings to blank character
  if (any(nas)) {
    df[nas] <- ""
  }

  # Here's where we align the the decimals, thanks to Noah for the magic.
  for (i in which(nums)) {
    if (any(grepl(".", df[[i]], fixed = TRUE))) {

      s <- strsplit(df[[i]], ".", fixed = TRUE)
      lengths <- lengths(s)
      digits.r.of.. <- sapply(seq_along(s), function(x) {

        if (lengths[x] > 1) {
          nchar(s[[x]][lengths[x]])
        } else {
          0
        }
      })

      df[[i]] <- sapply(seq_along(df[[i]]), function(x) {
        if (df[[i]][x] == "") {
          ""
        } else if (lengths[x] <= 1) {
          paste0(c(df[[i]][x],
                   rep(".", pad == 0),
                   rep(pad, max(digits.r.of..) -
                         digits.r.of..[x] + as.numeric(pad != 0))),
                 collapse = "")
        } else {
          paste0(c(df[[i]][x], rep(pad, max(digits.r.of..) - digits.r.of..[x])),
                 collapse = "")
        }
      })
    }
  }

  if (length(rn) > 0) rownames(df) <- rn
  if (length(cn) > 0) names(df) <- cn

  # Insert NA placeholders
  df[df == ""] <- na_vals

  return(df)
}

num_print <- function(x, digits = getOption("dpm-digits", 3), format = "f") {
  formatC(x, digits = digits, format = "f")
}

make_ci_labs <- function(ci.width) {

  alpha <- (1 - ci.width) / 2

  lci_lab <- 0 + alpha
  lci_lab <- paste(round(lci_lab * 100, 1), "%", sep = "")

  uci_lab <- 1 - alpha
  uci_lab <- paste(round(uci_lab * 100, 1), "%", sep = "")

  list(lci = lci_lab, uci = uci_lab)

}

`%nin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))

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
