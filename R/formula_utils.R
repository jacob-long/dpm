# Accessor function for Formula objects that have multiple parts
get_rhs <- function(x, which = 1, to.formula = FALSE) {
  # Coercing to formula can be useful, otherwise it's a call object
  if (to.formula == TRUE) {
    as.formula(paste("~", deparse(attr(x, "rhs")[[which]])))
  } else {
    attr(x, "rhs")[[which]]
  }
}

# automate tedious call to character conversion
to_char <- function(x) {
  paste(deparse(x), collapse = "")
}

# Get individual terms from formula
get_term_labels <- function(x, which = 1, omit.ints = TRUE) {
  # Formula provides an unusual access method but lets me get one part at a time
  labs <- attr(terms(get_rhs(x, which = which, to.formula = TRUE)),
               "term.labels")
  # I can choose to get only first-order variables
  if (omit.ints == TRUE) {
    labs <- labs[
      which(attr(
        terms(get_rhs(x, which = which, to.formula = TRUE)),
        "order") == 1)
      ]
  }
  # Detect random effects specifications
  if (any(stringr::str_detect(labs, "\\|"))) {
    labs[stringr::str_detect(labs, "\\|")] <-
      paste0("(", labs[stringr::str_detect(labs, "\\|")], ")")
  }
  return(labs)
}

# Generate the labels for factors that R normally does already
expand_labels <- function(data, variable) {
  paste0(variable, unique(
    data[[variable]][complete.cases(data[[variable]])] %not%
      base_level(data[[variable]]))
  )
}

# Make all the labels for terms of all orders for (especially) factors
make_labels <- function(formula, variable, data) {
  term_labels <- labels(terms(formula))[which_terms(formula, variable)]
  labs <- c()
  expanded <- expand_labels(data, variable)
  for (lab in term_labels) {
    for (val in expanded) {
      labs <- c(labs, sub(variable, bt(val), lab, fixed = TRUE))
    }
  }
  labs
}

# Retrieve the base level of factors or other non-numeric variables
base_level <- function(x) {
  if (is.factor(x)) {
    return(levels(x)[1])
  } else if (!is.logical(x)) {
    return(levels(factor(x))[1])
  } else {
    return(FALSE)
  }
}

# Get the indices of terms in terms object involving a given variable
which_terms <- function(formula, variable) {
  # Get the factors matrix from the terms object
  facs <- attr(terms(formula), "factors")
  # Get the term names
  raw_names <- rownames(facs)
  # Use some jiu-jitsu to get the bare variable names for those terms
  bare_vars <- all.vars(
    as.formula(paste("~", paste(raw_names, collapse = "+"))), unique = FALSE)
  # If there's more than one term involving variable, need to handle differently
  if (length(which(bare_vars == variable)) > 1) {
    which(colSums(facs[which(bare_vars == variable),]) > 0)
  } else {
    which(facs[which(bare_vars == variable),] > 0)
  }
}

# Create a formula with an expanded factor variable (i.e., with dummies)
expand_formula <- function(formula, variable, data) {
  out <- list()
  for (i in 1:length(formula)[2]) {
    tmp_form <- get_rhs(formula, which = i, to.formula = TRUE)
    if (!all(deparse(tmp_form) == "~1") && variable %in% all.vars(tmp_form)) {
      # Get terms that don't have anything to do with variable
      num_matches <- length(which_terms(tmp_form, variable))
      if (num_matches > 0 && num_matches != length(labels(terms(tmp_form)))) {
        o_terms <- labels(
          drop.terms(terms(tmp_form), which_terms(tmp_form, variable))
        )
        # Get vector of term labels for all terms that involve variable
        labs <- make_labels(tmp_form, variable, data)
      } else if (num_matches == 0) {
        o_terms <- labels(terms(tmp_form))
        labs <- NULL
      } else {
        o_terms <- NULL
        labs <- make_labels(tmp_form, variable, data)
      }
      # Use base R's reformulate function to make a new formula using these
      # character objects
      out <- c(out, reformulate(c(o_terms, labs)))
    } else if (all(deparse(tmp_form) == "~1")) {
      out <- c(out, reformulate("1"))
    } else {
      out <- c(out, reformulate(labels(terms(tmp_form))))
    }
  }
  return(lapply(out, function(x) x[[2]]))
}

# TODO: consider more robust support of non-treatment contrasts
# This adds new columns to the data frame for the levels of factors
expand_factors <- function(variables, data) {
  # Loop through the variables
  for (var in variables) {
    # Get values of variable
    vals <- unique(data[[var]] %not% base_level(data[[var]]))
    # Loop through values
    for (val in vals) {
      # Create new column of 0/1 for whether variable equals this value
      data[[paste0(var, val)]] <- as.numeric(data[[var]] == val)
    }
  }
  return(data)
}

# Taken from interactions pkg

any_interaction <- function(formula) {
  any(attr(terms(formula), "order") > 1)
}

get_interactions <- function(formula) {
  if (any_interaction(formula)) {
    ts <- terms(formula, keep.order = TRUE)
    labs <- paste("~", attr(ts, "term.labels"))
    forms <- lapply(labs, as.formula)
    forms <- forms[which(attr(ts, "order") > 1)]
    ints <- lapply(forms, function(x) {
      rownames(attr(terms(x), "factors"))
    })
    names(ints) <- attr(ts, "term.labels")[which(attr(ts, "order") > 1)]
    return(ints)
  } else {
    NULL
  }
}

expand_interactions <- function(x) {
  ranefs <- if (length(x)[2] >= 3) {
    fb(get_rhs(x, which = 3, to.formula = FALSE))
  } else NULL
  if (!is.null(ranefs)) {
    ranefs <- if (is.list(ranefs)) sapply(ranefs, to_char) else to_char(ranefs)
    ranefs <- stringr::str_replace_all(ranefs, "~", "")
  }
  if (length(attr(x, "rhs")) >= 3) {
    # Remove ranefs from formula for now
    attr(x, "rhs")[[3]] <- lme4::nobars(attr(x, "rhs")[[3]])
  }

  for (i in seq_along(attr(x, "rhs"))) {
    if (to_char(get_rhs(x, i, TRUE)) != "~ 1") {
      attr(x, "rhs")[[i]] <-
        reformulate(unique(attr(terms(get_rhs(x, i, TRUE), keep.order = TRUE),
                                "term.labels")))[[2]]
    }
  }

  # I handle factors in the random effects terms separately
  if (!is.null(ranefs)) {
    ranef_forms <- lapply(ranefs, function(y) {
      splitted <- stringr::str_split(y, "\\| | \\|\\|")
      lhs <- splitted[[1]][[1]]
      rhs <- splitted[[1]][[2]]
      lhs_form <- Formula::Formula(as.formula(paste("~", lhs)))
      if (to_char(lhs_form) != "~1") {
        lhs_form <- reformulate(unique(attr(terms(lhs_form, keep.order = TRUE),
                                            "term.labels")))
        lhs <- to_char(lhs_form[[2]])
      }
      paste0("(", lhs, ifelse(stringr::str_detect(y, "\\|\\|"),
                              yes = "||", no = "|"), rhs, ")")
    })
    ranef_forms <- paste(ranef_forms, collapse = " + ")
    new_3 <-
      paste("~", to_char(get_rhs(x, which = 3)), "+", ranef_forms)
    attr(x, "rhs")[[3]] <- as.formula(new_3)[[2]]
  }
  x
}

fb <- function(term) {
  if (is.name(term) || !is.language(term))
    return(NULL)
  if (term[[1]] == as.name("("))
    return(fb(term[[2]]))
  stopifnot(is.call(term))
  if (term[[1]] == as.name("|") | term[[1]] == as.name("||"))
    return(term)
  if (length(term) == 2)
    return(fb(term[[2]]))
  c(fb(term[[2]]), fb(term[[3]]))
}
