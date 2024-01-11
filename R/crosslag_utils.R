#' @importFrom stats terms
#' @import stringr
#' @import dplyr
#' @importFrom panelr are_varying
formula_parser <- function(formula, dv, data) {
  # See how many parts the formula has
  conds <- length(formula)[2]

  # Deal with non-numeric variables
  if (any(!sapply(all.vars(formula), function(x) is.numeric(data[[x]])))) {
    # Find the time-varying non-numeric vars
    vars <-
      names(sapply(all.vars(formula),
                   function(x) is.numeric(data[[x]])) %just% FALSE)
    # Expand these factors into 0/1 variables in the data
    data <- expand_factors(vars, data)
    # Now create a formula that does the same
    for (var in vars) {
      forms <- expand_formula(formula, var, data)
      forms <- paste(sapply(forms, to_char), collapse = "|")
      new_form <- Formula::Formula(as.formula(paste("~", forms)))
      attr(formula, "rhs") <- attr(new_form, "rhs")
    }
  }

  # Save varying variables
  varying <- sapply(get_term_labels(formula), function(x) {
    # If non-syntactic names are inside functions, retain backticks
    if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
  })

  evarying <- unique(c(varying,
               str_replace(varying, "(.*)(pre\\()(.*)(\\))(.*)", "\\1\\3\\5")))

  if (conds == 1) {
    # There are no constants
    constants <- NULL
  } else if (conds > 1) {
    # Save constants
    constants <- sapply(get_term_labels(formula, which = 2), function(x) {
      # If non-syntactic names are inside functions, retain backticks
      if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
    })
  }

  # Try to check for non-varying variables in varying part of formula
  for (var in varying %just% names(data)) {
    if (!(are_varying(data, !! sym(var)))) {
      varying <- varying %not% var
      constants <- c(constants, var)
      msg_wrap(var, " was included in the time-varying part of the formula
               but does not vary over time. It is being treated as a constant
               instead.")
    }
  }

  # Retain list of all variables to go into final model
  allvars <- c(dv, varying, constants)

  if (conds >= 3) { # Deal with interactions et al. part of formula
    # Grab all the variables mentioned in this part of the formula
    int_vars <- sapply(get_term_labels(formula, which = 3), function(x) {
      # If non-syntactic names are inside functions, retain backticks
      if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
    })

    if (any(stringr::str_detect(int_vars, "\\|"))) {
      barred <- int_vars[stringr::str_detect(int_vars, "\\|")]
      int_vars <- int_vars[!stringr::str_detect(int_vars, "\\|")]
      split <- stringr::str_trim(unlist(stringr::str_split(barred, "\\|")))
      int_vars <- c(int_vars, split)
    }

    # Add them onto the allvars vector as long as they aren't redundant
    allvars <- unique(c(allvars, int_vars))
  } else int_vars <- NULL

  # Now I want to expand all interactions into their constituent terms in
  # a conventional, one-part formula so I can deal with complex interactions
  # that may involve redundant variable pairings across formula parts.
  if (any_interaction(formula)) {
    formula <- expand_interactions(formula)
  }

  # Within by within interactions
  if (any_interaction(formula)) {
    wint_labs <- sapply(get_interactions(formula), function(x) {
      if (all(x %in% evarying)) paste(x, collapse = "*") else NULL
    })
  } else {wint_labs <- NULL}
  if (!is.null(wint_labs)) {
    wint_labs <- wint_labs[!sapply(wint_labs, is.null)]
  }

  # Between by between interactions
  if (any_interaction(formula)) {
    bint_labs <- sapply(get_interactions(formula), function(x) {
      if (all(x %nin% varying)) paste(x, collapse = "*") else NULL
    })
  } else {bint_labs <- NULL}
  if (!is.null(bint_labs)) {
    bint_labs <- bint_labs[!sapply(bint_labs, is.null)]
  }

  # Cross-level interactions
  if (any_interaction(formula)) {
    cint_labs <- sapply(get_interactions(formula), function(x) {
      # Looking for mix of within and between vars in an interaction
      if (any(x %in% evarying) & any(x %nin% evarying)) {
        paste(x, collapse = "*")
      } else NULL
    })
  } else {cint_labs <- NULL}
  if (!is.null(cint_labs)) {
    cint_labs <- cint_labs[!sapply(cint_labs, is.null)]
  }

  v_info <- tibble::tibble(term = varying, root = varying, lag = NA,
                           endog = FALSE, max_lag = NA)

  # If there's a lag function call, set lag to 1 and 0 otherwise. We'll go
  # back and look for the n argument in a second
  v_info$lag <- as.numeric(str_detect(varying, "(?<=lag\\().*(?=\\))"))


  # Figure out where pre() is called
  v_info$endog <- str_detect(varying, "(?<=pre\\().*(?=\\))")
  # Remove the pre tag
  v_info$term <- str_replace(v_info$term, "(.*)(pre\\()(.*)(\\))(.*)",
                             "\\1\\3\\5")
  v_info$root <- str_replace(v_info$root, "(.*)(pre\\()(.*)(\\))(.*)",
                             "\\1\\3\\5")

  # If there are lagged vars, we need the original varname for taking
  # the mean later
  if (any(v_info$lag > 0)) {
    for (i in which(v_info$lag > 0)) {
      # Convert to call to match the arguments and unambiguously get the n =
      the_call <-
        match.call(dplyr::lag, call = parse(text = v_info$term[i]))
      v_info$lag[i] <- if (!is.null(the_call$n)) the_call$n else 1
      v_info$root[i] <- to_char(the_call$x)
    }
  }

  # If there's a lead function call, set lag to -1 and 0 otherwise. We'll go
  # back and look for the n argument in a second
  leads <- str_detect(varying, "(?<=lead\\().*(?=\\))")

  # If there are lagged vars, we need the original varname for taking
  # the mean later
  if (any(leads)) {
    for (i in which(leads)) {
      # Convert to call to match the arguments and unambiguously get the n =
      the_call <-
        match.call(dplyr::lead, call = parse(text = v_info$term[i]))
      v_info$lag[i] <- if (!is.null(the_call$n)) the_call$n * -1 else -1
      v_info$root[i] <- as.character(the_call$x)
    }
  }


  # Set max_lag before going through interactions
  v_info$max_lag <- v_info$lag
  # Deal with time-varying by time-varying interactions
  if (length(wint_labs) > 0) {
    for (wint in wint_labs) {
      endog <- str_detect(wint, "(?<=pre\\().*(?=\\))")
      while(stringr::str_detect(wint, "(?<=pre\\().*(?=\\))")) {
        wint <- str_replace(wint, "(.*)(pre\\()(.*)(\\))(.*)", "\\1\\3\\5")
      }

      my_row <- nrow(v_info) + 1
      v_info[my_row,] <- list(wint, wint, 0, endog,
                           as.numeric(str_detect(wint, "(?<=lag\\().*(?=\\))")))
      # potential cross-lag ints are complicated so now I try to deal with them
      if (str_detect(wint, "(?<=lag\\().*(?=\\))")) {
        parts <- str_split(wint, ":|\\*")[[1]]
        lags <- NULL
        parts <- str_subset(parts, "(?<=lag\\().*(?=\\))")
        for (part in parts) {
          # Convert to call to match the arguments and unambiguously get the n =
          the_call <- match.call(dplyr::lag, call = parse(text = part))
          lags <- c(lags, if (!is.null(the_call$n)) the_call$n else 1)
        }
        v_info[my_row, "max_lag"] <- max(lags)
      }
    }
    while (any(stringr::str_detect(wint_labs, "(?<=pre\\().*(?=\\))"))) {
      wint_labs <- str_replace(wint_labs, "(.*)(pre\\()(.*)(\\))(.*)",
                               "\\1\\3\\5")
    }
  }

  # Deal with time-varying by time-varying by constant interactions
  if (length(cint_labs) > 0) {
    for (cint in cint_labs) {
      endog <- str_detect(cint, "(?<=pre\\().*(?=\\))")
      while (stringr::str_detect(cint, "(?<=pre\\().*(?=\\))")) {
        cint <- str_replace(cint, "(.*)(pre\\()(.*)(\\))(.*)", "\\1\\3\\5")
      }

      my_row <- nrow(v_info) + 1
      v_info[my_row,] <- list(cint, cint, 0, endog,
                           as.numeric(str_detect(cint, "(?<=lag\\().*(?=\\))")))
      # potential cross-lag ints are complicated so now I try to deal with them
      if (str_detect(cint, "(?<=lag\\().*(?=\\))")) {
        parts <- str_split(cint, ":|\\*")[[1]]
        lags <- NULL
        parts <- str_subset(parts, "(?<=lag\\().*(?=\\))")
        for (part in parts) {
          # Convert to call to match the arguments and unambiguously get the n =
          the_call <- match.call(dplyr::lag, call = parse(text = part))
          lags <- c(lags, if (!is.null(the_call$n)) the_call$n else 1)
        }
        v_info[my_row, "max_lag"] <- max(lags)
      }
    }
    while (any(stringr::str_detect(cint_labs, "(?<=pre\\().*(?=\\))"))) {
      cint_labs <- str_replace(cint_labs, "(.*)(pre\\()(.*)(\\))(.*)",
                               "\\1\\3\\5")
    }
  }

  constants <- c(constants, bint_labs)
  allvars <- unique(c(dv, v_info$root, constants, int_vars))
  allvars <- stringr::str_replace_all(allvars, "(.*)(pre\\()(.*)(\\))(.*)",
                                  "\\1\\3\\5")

  v_info$root[is.na(v_info$root)] <- v_info$term[is.na(v_info$root)]

  v_info$new_name <- make_names(v_info$root, int = TRUE)

  out <- list(conds = conds, allvars = allvars,
              endogs = v_info$term[v_info$endog == TRUE],
              exogs = v_info$term[v_info$endog == FALSE],
              constants = constants, v_info = v_info,
              data = data, wint_labs = wint_labs, cint_labs = cint_labs,
              bint_labs = bint_labs)
  return(out)

}
