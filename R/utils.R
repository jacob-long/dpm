
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
