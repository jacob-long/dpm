cl_formula_parser <- function(formula) {

  dv <- as.character(formula[[2]])

  formula <- as.character(formula)[[3]]

  conds <- sum(attr(gregexpr("\\|", formula)[[1]], "match.length"))

  if (conds == -1L) {conds <- 0}

  if (conds == 0) {

    constants <- NULL

    varying <- stringr::str_split(formula, "\\+")
    varying <- stringr::str_split(unlist(varying), "\\*")
    varying <- stringr::str_split(unlist(varying), "\\:")
    varying <- unlist(lapply(varying, trimws))

    if (any(grepl(".*(?=pre\\()", varying, perl = T))) {

      end_terms <- which(grepl(".*(?=pre\\()", varying, perl = T))
      end_terms <- varying[end_terms]

      exogs <- varying[!(varying %in% end_terms)]

      end_matches <- regexec(text = varying, pattern = "(?<=pre\\().*(?=\\))",
                             perl = T)

      end_matches <- end_matches[!(end_matches == -1)]

      endogs <- unlist(regmatches(m = end_matches, x = end_terms))

      varying <- c(endogs, exogs)

    } else {

      endogs <- NULL
      exogs <- varying

    }

    allvars <- c(dv, varying)

  } else if (conds == 1) {

    splitted <- stringr::str_split(formula, "\\|")

    varying <- stringr::str_split(splitted[[1]][1], "\\+")
    varying <- stringr::str_split(unlist(varying), "\\*")
    varying <- stringr::str_split(unlist(varying), "\\:")
    varying <- unlist(lapply(varying, trimws))

    if (any(grepl(".*(?=pre\\()", varying, perl = T))) {

      end_terms <- which(grepl(".*(?=pre\\()", varying, perl = T))
      end_terms <- varying[end_terms]

      exogs <- varying[!(varying %in% end_terms)]

      end_matches <- regexec(text = varying, pattern = "(?<=pre\\().*(?=\\))", perl = T)
      end_matches <- end_matches[!(end_matches == -1)]

      endogs <- unlist(regmatches(m = end_matches, x = end_terms))

      varying <- c(endogs, exogs)

    } else {

      endogs <- NULL
      exogs <- varying

    }

    constants <- stringr::str_split(splitted[[1]][2], "\\+")
    constants <- stringr::str_split(unlist(constants), "\\*")
    constants <- stringr::str_split(unlist(constants), "\\:")
    constants <- unlist(lapply(constants, trimws))

    allvars <- c(dv, varying, constants)

  } else {

    warning("Formula should only have two parts on the right-hand side. Note: Interactions are not supported for the cross-lagged fixed effects model.")

  }

  out <- list(allvars = allvars, varying = varying, exogs = exogs,
              endogs = endogs, constants = constants, dv = dv)
  return(out)

}
