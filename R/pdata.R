panel_model_frame <- function(vars, data) {

  # Grabbing lag terms, storing their original names ("lag(var)")
  og_terms <- which(grepl(".*(?=lag\\()", vars, perl = T))
  og_terms <- vars[og_terms]

  # Grabbing the regex, will be useful for pulling out args alter
  lags_matches <- regexec(text = vars, pattern = "(?<=lag\\().*(?=\\))", perl = T)
  lags_matches <- lags_matches[!(lags_matches == -1)]

  if (length(lags_matches) > 0) { # If matches, build the list

    out <- lag_frame(data, lags_matches, og_terms, vars)
    return(out)

  } else { # Create an empty list to return at the end

    return(data)

  }


}

lag_frame <- function(data, lags_matches, og_terms, vars) {

  id <- attr(data, "id")
  if (is.null(id)) {
    id <- names(data)[1]
  }

  wave <- attr(data, "wave")
  if (is.null(wave)) {
    wave <- names(data)[2]
  }

  lags_vars <- regmatches(m = lags_matches, x = og_terms)
  lags_vars <- unlist(lags_vars)

  lags_vars_names <- list()

  if (any(grepl(",", lags_vars))) { # If any commas, assume args follow
    # Set extra_args to T
    extra_args <- TRUE
    # Save list of args
    lags_vars_args <- list()

    # Now go through whichever vars match
    for (i in 1:length(lags_vars)) {

      if (i %in% which(grepl(",", lags_vars))) { # If it has extra arg

        # Split the string by commas
        temp_vars <- strsplit(lags_vars[i], ",")
        # The first part is the variable name
        lags_vars_names[[i]] <- temp_vars[[1]][1]
        # The rest will be arguments, for now just lags
        lags_vars_args[[i]] <- temp_vars[[1]][2:length(temp_vars[[1]])]

      } else { # Build the lists

        # If no args, then lags_vars has the name
        lags_vars_names[[i]] <- lags_vars[i]
        # And there are no args
        lags_vars_args[[i]] <- 1

      }
    }

  } else { # No extra args in any terms

    extra_args <- FALSE

    # Were there even any lag vars?
    if (length(lags_matches) > 0) {

      for (i in 1:length(lags_vars)) { # Make sure we keep the lag vars in a list

        lags_vars_names[[i]] <- lags_vars[i]

      }
    }

  }

  # Separate list of non-lagged vars, needs code in succeeding loop to actually
  # drop the non-lagged vars
  no_lags_vars <- vars

  # All vars vector with all original names
  all_vars <- c(id, wave, vars)
  new_names <- all_vars

  # Keep track of lags for all vars for cross_lag_fe
  vars_lags <- rep(0, length(all_vars))

  # Now we drop lagged vars from the var list
  drops <- list()
  # Create list of indices to drop
  for (i in 1:length(og_terms)) {

    drops[[i]] <- which(og_terms[i] == no_lags_vars)

  }

  # Creating logical vector to help keep the non-lagged vars
  keeps <- 1:length(no_lags_vars) %in% unlist(drops)
  keeps <- !keeps

  # Now we drop the lagged vars
  no_lags_vars <- no_lags_vars[keeps]

  for (i in 1:length(lags_vars)) { # Time to actually lag the data

    if (extra_args) { # If lag arg, provide it

      lags <- as.numeric(lags_vars_args[[i]]) # For clarity

    } else {

      lags <- 1

    }

    vars_lags[which(all_vars == og_terms[i])] <- lags

  }

  names(lags_vars_names) <- og_terms
  for (o in og_terms) {

    new_names[which(new_names == o)] <- lags_vars_names[o]

  }
  names(vars_lags) <- new_names
  names(new_names) <- all_vars

  out <- list(data = data, og_terms = og_terms,
              vars_lags = unlist(vars_lags),
              new_names = new_names)
  return(out)

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



