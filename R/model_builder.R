model_builder <- function(mf, dv, endogs, exogs, constants, id, wave,
                          err.inv, const.inv, alpha.free, y.lag, y.free,
                          fixed.effects) {


##### Get lag info ############################################################

  if (is_panel(mf)) {

    d <- mf
    mf["vars_lags"] <- 0
    mf["og_terms"] <- NULL

  } else {

    d <- mf$data

  }

  # Endogenous predictor lag structure
  ## Saving info about where in the respective lists the endogenous variables
  ## and their lag numbers are
  if (!is.null(endogs)) {

    if (is_panel(mf)) {
      indices <- NULL
    } else if (length(mf["og_terms"]) > 0) {
      indices <- which(names(mf$new_names) %in% endogs)
    } else {
      indices <- NULL
    }

    ## Now going through and saving the number of lags to list for use later
    endogs_lags <- rep(0, length(endogs))
    names(endogs_lags) <- endogs
    if (length(indices) > 0) {
      endogs <- mf$new_names[indices]
      endogs_lags <- mf$vars_lags[indices]
      names(endogs_lags) <- endogs
      endogs <- unname(endogs)
    }

    # endogs <- unlist(endogs)
    endogs <- unique(endogs)
    names(endogs) <- endogs
    endogs_lags <- as.list(endogs_lags)
    if (any(duplicated(names(endogs_lags)))) {
      dupes <- names(endogs_lags)[duplicated(names(endogs_lags))]
      for (dupe in dupes) {
        values <- endogs_lags[which(names(endogs_lags) == dupe)]
        endogs_lags <- endogs_lags[names(endogs_lags) %nin% dupe]
        endogs_lags[[dupe]] <- unname(unlist(values))
      }
    }
    # endogs <- endogs[!duplicated(endogs)]

  } else {

    endogs <- NULL
    endogs_lags <- NULL

  }

  if (!is.null(exogs)) {
    # Exogenous predictor lag structure
    ## Saving info about where in the respective lists the exogenous variables
    ## and their lag numbers are

    if (is_panel(mf)) {
      indices <- NULL
    } else if (length(mf["og_terms"]) > 0) {
      indices <- which(names(mf$new_names) %in% exogs)
    } else {
      indices <- NULL
    }

    ## Now going through and saving the number of lags to list for use later
    exogs_lags <- rep(0, length(exogs))
    names(exogs_lags) <- exogs
    if (length(indices) > 0) {
      exogs <- mf$new_names[indices]
      exogs_lags <- mf$vars_lags[indices]
      names(exogs_lags) <- exogs
      exogs <- unname(exogs)
    }

    exogs <- unique(exogs)
    names(exogs) <- exogs
    exogs_lags <- as.list(exogs_lags)
    if (any(duplicated(names(exogs_lags)))) {
      dupes <- names(exogs_lags)[duplicated(names(exogs_lags))]
      for (dupe in dupes) {
        values <- exogs_lags[which(names(exogs_lags) == dupe)]
        exogs_lags <- exogs_lags[names(exogs_lags) %nin% dupe]
        exogs_lags[[dupe]] <- unname(unlist(values))
      }
    }

  } else {

    exogs <- NULL
    exogs_lags <- NULL

  }

  # Saving a vector of all variables that are time varying
  varying <- c(unlist(endogs), dv, unlist(exogs))
#
#   en_frame <- data.frame(var = endogs, lag = endogs_lags, pre = TRUE)
#   ex_frame <- data.frame(var = exogs, lag = exogs_lags, pre = FALSE)

###### Widen data #############################################################

  # If data is already in wide format, for now I just convert to long and back
  # if (wide_data == TRUE) {
  #
  #   d <- lengthen_panel(d)
  #
  # }

  # Now I need to have the data in wide format
  wframe <- widen_panel_in(d, varying = varying, constants = constants)

  # Save info about complete observations
  complete_obs <- attr(wframe, "complete_obs")


###### Model build prep #######################################################

  # Save list of waves
  waves <- sort(unique(d[[wave]]))
  min_wave <- min(d[[wave]])
  max_wave <- max(d[[wave]])
  # Helper function for later
  ch <- function(x, ...) {as.character(x, ...)}
  # wavesc <- ch(waves)

  # Creating list of time-varying variables in a list for constructing lavaan
  # model string
  vbywave <- rep(list(NA), times = length(waves))
  names(vbywave) <- ch(waves)
  for (w in waves) {
    varnames <- sapply(varying, paste, "_", w, sep = "")
    vbywave[[ch(w)]] <- varnames
    names(vbywave[[ch(w)]]) <- varying # This allows indexing by the bare name
  }

  # start variable is used to determine which wave to begin with
  start <- max(c(mf$vars_lags, y.lag)) + 1 # Depends on how many lags we have
  if (start < (min_wave + max(y.lag))) {start <- min_wave + max(y.lag)} # But can't have lagged DV at wave 1 either
  end <- max_wave
  # if (start == 0) {
  #   start <- 1
  # }

  vbywave <- lapply(vbywave, function(x) {
    x <- x[!duplicated(x)]
  })

####### Alpha latent var definition ###########################################


  # Let alpha vary across time if user requests
  if (alpha.free == TRUE) {

    a_f <- ""

  } else { # Otherwise fixed at one

    a_f <- "1 * "

  }

  the_dvs <- sapply(vbywave[ch(start:end)], function(x) {x[dv]})
  alpha_eq <- paste("alpha =~ ", paste(paste0(a_f, the_dvs), collapse = " + "),
                    sep = "")


####### Main dv equations #####################################################

  main_eqs <- NULL
  var_coefs <- data.frame(var = NA, coef = NA, lag = NA)
  # iterating over each wave for which we will predict the value of DV
  for (w in start:end) {

    reg_vars_en <- NULL
    reg_vars_ex <- NULL
    reg_vars_cons <- NULL

    ## Loop through endogenous variables, putting all in a vector
    if (!is.null(endogs)) {

      for (var in endogs) {

        en_lags_l <- unlist(endogs_lags)
        names(en_lags_l) <- rep(names(endogs_lags),
                                times = sapply(endogs_lags, length))
        index <- which(names(en_lags_l) == var) # For numbering the fixed coefs
        for (i in index) {

          reg_vars_en <- c(
            reg_vars_en, paste0("en", i, " * ",
                                vbywave[[ch(w - en_lags_l[i])]][var])
          )
          var_coefs[nrow(var_coefs) + 1,] <-
            list(var, paste0("en", i), en_lags_l[i])

        }

      }

    }

    ## If there are exogenous time varying vars, loop through those
    if (!is.null(exogs)) {

      for (var in exogs) {

        ex_lags_l <- unlist(exogs_lags)
        names(ex_lags_l) <- rep(names(exogs_lags),
                                times = sapply(exogs_lags, length))
        index <- which(names(ex_lags_l) == var) # For numbering the fixed coefs
        for (i in index) {
          reg_vars_ex <- c(
            reg_vars_ex, paste0("ex", i, " * ",
                                vbywave[[ch(w - ex_lags_l[i])]][var])
          )
          var_coefs[nrow(var_coefs) + 1,] <-
            list(var, paste0("ex", i), ex_lags_l[i])
        }

      }

    }

    ## If there are constants, loop through them
    if (!is.null(constants)) {

      for (var in constants) {

        index <- which(constants == var)
        reg_vars_cons <- c(
          reg_vars_cons, paste0("c", index, " * ", constants[index],
                     sep = "")
        )
        var_coefs[nrow(var_coefs) + 1,] <- list(var, paste0("c", index), 0)
      }

    }

    reg_vars_en <- reg_vars_en[!duplicated(reg_vars_en)]
    reg_vars_ex <- reg_vars_ex[!duplicated(reg_vars_ex)]
    reg_vars_cons <- reg_vars_cons[!duplicated(reg_vars_cons)]

    reg <- paste(vbywave[[ch(w)]][dv], "~",
      paste(c(reg_vars_en, reg_vars_ex,  reg_vars_cons), collapse = " + ")
    )

    ## Lastly, add prior wave of DV
    for (lag.y in y.lag) {
      if (lag.y == 0) {next}
      lag_term <- if (y.free == TRUE | lag.y %in% y.free) {NULL} else {
        paste0("p", lag.y, " * ")
      }
      reg <- paste(reg, " + ", lag_term, vbywave[[ch(w - lag.y)]][dv], sep = "")
      var_coefs[nrow(var_coefs) + 1,] <-  list(dv, paste0("p", lag.y), lag.y)
    }

    ## Save finished equation to list (okay, technically a vector)
    main_eqs <- c(main_eqs, reg)

  }
  reg <- NULL
  var_coefs <- var_coefs[complete.cases(var_coefs),]
  var_coefs <- unique(var_coefs)



####### Alpha covariances #####################################################

  alpha_reg <- "alpha ~~" # Beginning of equation

  # if (!is.null(exogs)) {
  #   exogsc <- exogs[!duplicated(exogs)]
  #   exogs_lagsc <- sort(exogs_lags, decreasing = FALSE)
  #   exogs_lagsc <- exogs_lagsc[unique(names(exogs_lagsc))]
  # } else {
  #   exogsc <- NULL
  #   exogs_lagsc <- NULL
  # }

  varying_vars <- unique(unlist(list(endogs, exogs), recursive = FALSE))
  varying_lags <- unlist(list(endogs_lags, exogs_lags), recursive = FALSE)


  if (any(duplicated(names(varying_lags)))) {
    dupes <- names(varying_lags)[duplicated(names(varying_lags))]
    for (dupe in dupes) {
      values <- varying_lags[which(names(varying_lags) == dupe)]
      varying_lags <- varying_lags[names(varying_lags) %nin% dupe]
      varying_lags[[dupe]] <- unname(unlist(values))
    }
  }

  alpha_vars <- NULL
  for (var in varying_vars) {

    w_begin <- start - max(varying_lags[[var]])
    w_end <- end - min(varying_lags[[var]])
    ws <- w_begin:w_end

    alpha_vars <- c(alpha_vars,
                    sapply(vbywave[ch(ws)], function(x) {x[var]})
                    )

  }

  for (w in 1:(which(names(vbywave) == start) - 1)) {
    # Add waves of DV prior to first time it is used as regression DV
    if (0 %nin% y.lag) { # Don't want this for non-dynamic specification
      alpha_vars <- c(alpha_vars, vbywave[[w]][dv])
    }
  }

  if (fixed.effects == TRUE) {re <- NULL} else {re <- "0 *"}
  alpha_reg <- paste(alpha_reg, paste(paste(re, alpha_vars), collapse = " + "))


######## Endogenous IV covariances ############################################

  # Creating empty object to which we will append the equations
  endogs_covs <- c()

  # The part of the equation with time-varying exogenous predictors is the same
  # no matter what, so making it once and will add each time later
  exogsreg <- NULL
  if (!is.null(exogs)) {

    # exogsc <- exogs[!duplicated(exogs)]
    # exogs_lagsc <- sort(exogs_lags, decreasing = TRUE)
    # exogs_lagsc <- exogs_lagsc[unique(names(exogs_lagsc))]
    for (var in exogs) {

      exogsreg <- c(exogsreg, vbywave[[ch(start - max(exogs_lags[[var]]))]][var])

      for (w in (start - max(exogs_lags[[var]])):(end - min(exogs_lags[[var]]))) {
        if (w > end) {next}
        exogsreg <- c(exogsreg, vbywave[[ch(w)]][var])

      }

    }

  }

  # Creating the constants part of the equations, which obviously doesn't change
  creg <- NULL
  if (!is.null(constants)) {

    for (var in constants) {

      index <- which(constants == var)

      creg <- c(creg, constants[index])

    }

  }

  creg <- creg[!duplicated(creg)]
  exogsreg <- exogsreg[!duplicated(exogsreg)]

  # Creating endogenous IV covariances
  endogs_covs <- NULL # in case there are no endogenous
  mod_frame <- data.frame(dv = NA, iv = NA)
  if (!is.null(endogs)) { # Checking if there are any endogenous variables
    # Iterating through endogenous variables
    endogs_covs <- tv_cov_eqs(var = var, start = start, end = end,
                             endogs_lags = endogs_lags,
                             vbywave = vbywave, endogs = endogs,
                             exogsreg = exogsreg, creg = creg, dv = dv,
                             y.lag = y.lag,
                             mod_frame = mod_frame, min_wave = min_wave,
                             max_wave = max_wave)
    mod_frame <- attr(endogs_covs, "mod_frame")
  }

###### Exogenous tv covariances ###############################################

  exogs_covs <- NULL
  if (!is.null(exogs)) {

    # Iterating through endogenous variables
    exogs_covs <- tv_cov_eqs(var = var, start = start, end = end,
                             endogs_lags = exogs_lags,
                             vbywave = vbywave, endogs = exogs,
                             exogsreg = NULL, creg = creg, dv = dv,
                             y.lag = y.lag,
                             mod_frame = mod_frame, min_wave = min_wave,
                             max_wave = max_wave)
  }

######## Constants covariances ################################################

  constants_covs <- c()
  if (!is.null(constants)) {

    for (c in constants) {

      if (0 %nin% y.lag) {
        ## Covary with all values of DV prior to first regression
        reg <- paste(c, "~~", vbywave[[ch(min_wave)]][dv])
        if (start > min_wave + 1) { # This is needed if there is > 1 lag

          for (wp in (min_wave + 1):(start - 1)) {

            reg <- paste(reg, "+", vbywave[[ch(wp)]][dv])

          }

        }
      } else if (which(constants == c) < length(constants)) {
        reg <- paste(c, "~~")
      } else {
        reg <- NULL
      }

      if (which(constants == c) < length(constants)) {

        ocs <- constants[(which(constants == c) + 1):length(constants)]
        for (oc in ocs) {

          reg <- paste(reg, "+", oc)

        }

      }

      constants_covs <- c(constants_covs, reg)

    }

  }
  reg <- NULL

###### Correlated future errors equations #####################################

  endogs_errs <- c()
  reg <- NULL
  if (!is.null(endogs)) {

    # Using this to control whether final wave of endogenous pred is used
    for (w in start:(end - 1)) {

      if (w == min_wave && start != min_wave) {next}
      # if ()

      reg <- paste(vbywave[[ch(w)]][dv], " ~~", sep = "")
      reg_vars <- NULL

      for (var in endogs) {

        w2 <- w + 1

        while (w2 <= end - min(endogs_lags[[var]])) {
          if (w2 > end) {next}

          reg_vars <- c(reg_vars, vbywave[[ch(w2)]][var])

          w2 <- w2 + 1

        }

      }

      if (is.null(reg_vars)) {

        # Do nothing because it's an empty equation

      } else {

        reg <- paste(reg, paste(reg_vars, collapse = " + "))
        endogs_errs <- c(reg, endogs_errs)

      }
      reg <- NULL

    }

  }

####### DV error variance equations (optional) #################################


  # If user wants DV error variances for each wave to be held constant, do it
  if (err.inv == TRUE) {

    err_var_eqs_ann <- "## Holding DV error variance constant for each wave"
    var_co <- "v*"

  } else {

    err_var_eqs_ann <- "## DV error variance free to vary across waves"
    var_co <- NULL

  }

  err_var_eqs <- NULL
  for (w in start:end) {

    eq <- paste(vbywave[[ch(w)]][dv], " ~~ ", var_co, vbywave[[ch(w)]][dv], sep = "")
    err_var_eqs <- c(err_var_eqs, eq)

  }


###### DV variance constraints #######################################

  dv_vars <- c()
  dv_vars_ann <- NULL

  if (const.inv == TRUE) {

    dv_vars_ann <- "## Hold DV variance constant each wave (optional)"
    var_co <- "var*"

  } else {

    dv_vars_ann <- "## Let DV variance vary across waves"
    var_co <- NULL

  }

  for (w in start:end) {

    reg <- paste(vbywave[[ch(w)]][dv], " ~ ", var_co, "1", sep = "")
    # Add finished equation to list
    dv_vars <- c(dv_vars, reg)
    reg <- NULL

  }
  reg <- NULL


###### DV multiple lags covariances ###########################################

  lag_covs <- NULL
  if (any(y.lag > 1)) {

    lag <- max(y.lag)
    lag <- paste0(dv, "_", min_wave + lag)
    ylags <- sapply(vbywave[ch(max_wave:min_wave)], function(x) {x[dv]})
    ylags <- ylags[(which(ylags == lag) + 1):length(ylags)]
    lag_reg <- paste(
      ylags[1], "~~",  paste(ylags[-1], collapse = " + ")
    )
    lag_covs <- c(lag_covs, lag_reg)
    ylags <- ylags[-1]

    while (length(ylags) > 1) {
      lag_reg <- paste(
        ylags[1], "~~",  paste(ylags[-1], collapse = " + ")
      )
      lag_covs <- c(lag_covs, lag_reg)
      ylags <- ylags[-1]
    }

  }

###### Defining annotations to be used in final model string ##################

  alpha_eq_ann <- "## Alpha latent variable (random intercept)"
  alpha_reg_ann <- if (fixed.effects == TRUE) {
    "## Alpha free to covary with observed variables (fixed effects)"
  } else {
    "## Alpha cannot covary with observed variables (random effects)"
  }
  main_eqs_ann <- "## Main regressions"
  endogs_errs_ann <- if (!is.null(endogs)) {
    "## Correlating DV errors with future values of predetermined predictors"
  } else {NULL}
  endogs_covs_ann <- if (!is.null(endogs)) {
    "## Predetermined predictors covariances"
  } else {NULL}
  exogs_covs_ann <- "## Exogenous (time varying and invariant) predictors covariances"

  # Now save all parts of the lavaan model to one object
  all_eqs <- list(main_eqs_ann, main_eqs, alpha_eq_ann, alpha_eq,
                  alpha_reg_ann, alpha_reg, endogs_errs_ann, endogs_errs,
                  endogs_covs_ann, endogs_covs,
                  exogs_covs_ann, exogs_covs, constants_covs, lag_covs,
                  err_var_eqs_ann, err_var_eqs, dv_vars_ann, dv_vars)


  # Adding line breaks between the equations
  out <- lapply(all_eqs, concat) # Concat is internal function defined in pdata.R

  # Adding extra line breaks between each set of equations
  out <- concat2(out) # Concat 2 is internal function defined pdata.R

  # if (print == TRUE) {
  #   cat(out, "\n")
  # }

  ret_obj <- list(model = out, data = wframe, complete_obs = complete_obs,
                  start = start, end = end, var_coefs = var_coefs)
  if (!is.null(endogs)) {
    ret_obj$endogs_lags <- endogs_lags
    ret_obj$endogs <- endogs
  }
  if (!is.null(exogs)) {
    ret_obj$exogs_lags <- exogs_lags
    ret_obj$exogs <- exogs
  }
  return(ret_obj)

}
