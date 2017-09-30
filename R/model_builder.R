
model_builder <- function(mf, dv, endogs, exogs, constants, id, wave, last.wave,
                          err.inv, const.inv, alpha.free, print = FALSE) {


##### Get lag info ############################################################

  if ("panel_data" %in% class(mf)) {

    d <- mf
    mf$vars_lags <- 0

  } else {

    d <- mf$data

  }

  # Endogenous predictor lag structure
  ## Saving info about where in the respective lists the endogenous variables
  ## and their lag numbers are
  if (!is.null(endogs)) {

    if (length(mf$og_terms) > 0) {
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

    endogs <- unlist(endogs)
    names(endogs) <- endogs

  } else {

    endogs <- NULL
    endogs_lags <- NULL

  }

  if (!is.null(exogs)) {
    # Exogenous predictor lag structure
    ## Saving info about where in the respective lists the exogenous variables
    ## and their lag numbers are

    if (length(mf$og_terms) > 0) {
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

    exogs <- unlist(exogs)
    names(exogs) <- exogs

  } else {

    exogs <- NULL
    exogs_lags <- NULL

  }

  # Saving a vector of all variables that are time varying
  varying <- c(endogs, dv, exogs)

###### Widen data #############################################################

  # If data is already in wide format, for now I just convert to long and back
  # if (wide_data == TRUE) {
  #
  #   d <- lengthen_panel(d)
  #
  # }

  # Now I need to have the data in wide format
  wframe <- widen_panel(d, varying = varying, constants = constants)

  # Save info about complete observations
  complete_obs <- attr(wframe, "complete_obs")


###### Model build prep #######################################################

  # Save list of waves
  waves <- unique(d[,wave])

  # Creating list of time-varying variables in a list for constructing lavaan
  # model string
  vbywave <- rep(list(NA), times = length(waves))
  for (w in waves) {
    varnames <- sapply(varying, paste, "_", w, sep = "")
    vbywave[[w]] <- varnames
    names(vbywave[[w]]) <- varying # This allows indexing by the original name
  }

  # start variable is used to determine which wave to begin with
  start <- max(mf$vars_lags) + 1 # Depends on how many lags we have
  if (start < 2) {start <- 2} # But can't have lagged DV at wave 1 either
  end <- length(waves)
  # if (start == 0) {
  #   start <- 1
  # }


####### Alpha latent var definition ###########################################


  alpha_eq <- paste("alpha =~ ", "1*", vbywave[[start]][dv],
                    sep = "")

  if (length(waves) > 2) {

    # Let alpha vary across time if user requests
    if (alpha.free == TRUE) {

      a_f <- ""

    } else { # Otherwise fixed at one

      a_f <- 1

    }

    for (w in waves[3:length(waves)]) {

      alpha_eq <- paste(alpha_eq, " + ", a_f, "*", vbywave[[w]][dv], sep = "")

    }

  }


####### Main dv equations #####################################################

  main_eqs <- c()
  # iterating over each wave for which we will predict the value of DV
  for (w in waves[start:length(waves)]) {

    # Reg object has beginning of equation, then will be added onto
    ## Starts with first endogenous variable
    if (!is.null(endogs)) {

      reg <- paste(vbywave[[w]][dv], " ~ ", "en1*",
                   vbywave[[w - endogs_lags[1]]][endogs[1]], sep = "")
      # The subtraction of endogs_lags[1] is how the lagging happens
      # and doesn't require the programmer's foreknowledge of what the
      # lag structure is. If no lags, then 0 is subtracted from current wave

      ## If more endogenous variables, loop through them
      if (length(endogs) > 1) {

        for (var in endogs[2:length(endogs)]) {

          index <- which(endogs == var) # For numbering the fixed coefficients
          reg <- paste(reg, " + ", "en", index, "*",
                       vbywave[[w - endogs_lags[index]]][var], sep = "")

        }

      }

    } else {# If no endogenous vars, then create beginning of equation here

      reg <- paste(vbywave[[w]][dv], "~")

    }

    ## If there are exogenous time varying vars, loop through those
    if (!is.null(exogs)) {

      for (var in exogs) {

        index <- which(exogs == var)
        reg <- paste(reg, " + ", "ex", index, "*",
                     vbywave[[w - exogs_lags[index]]][var], sep = "")

      }

    }

    ## If there are constants, loop through them
    if (!is.null(constants)) {

      for (var in constants) {

        index <- which(constants == var)
        reg <- paste(reg, " + ", "c", index, "*", constants[index],
                     sep = "")

      }

    }

    ## Lastly, add prior wave of DV
    reg <- paste(reg, " + ", "p*", vbywave[[w - 1]][dv], sep = "")

    ## Save finished equation to list (okay, technically a vector)
    main_eqs <- c(main_eqs, reg)

  }
  reg <- NULL



####### Alpha covariances #####################################################

  alpha_reg <- "alpha ~~" # Beginning of equation
  varying_vars <- c(endogs, exogs)
  varying_lags <- c(endogs_lags, exogs_lags)

  for (var in varying_vars) {

    w <- start - varying_lags[var]

    if (which(varying_vars == var) == 1) {
      # Avoid extraneous + sign
      alpha_reg <- paste(alpha_reg, vbywave[[w]][var])
      w <- w + 1 # Avoid repeating the term in the while loop

    }

    while (w <= (end - (varying_lags[var] - last.wave)) && w <= end) {

      alpha_reg <- paste(alpha_reg, "+", vbywave[[w]][var])
      w <- w + 1

    }

  }

  for (w in 1:(start - 1)) {
    # Add waves of DV prior to first time it is used as regression DV

    alpha_reg <- paste(alpha_reg, "+", vbywave[[w]][dv])

  }




######## Endogenous IV covariances ############################################

  # Creating empty object to which we will append the equations
  endogs_covs <- c()

  # The part of the equation with time-varying exogenous predictors is the same
  # no matter what, so making it once and will add each time later
  exogsreg <- ""
  if (!is.null(exogs)) {

    for (var in exogs) {

      if (which(exogs == var) == 1) {
        # This helps me know if it's the first time through the loop
        exogsreg <- paste("+", vbywave[[start - exogs_lags[var]]][var])

      } else {
        # otherwise prepend with +
        exogsreg <- paste(exogsreg, "+",
                          vbywave[[start - exogs_lags[var]]][var])

      }

      for (w in 2:(end - (exogs_lags[var] - last.wave))) {
        if (w > end) {next}
        # Now adding the rest of the waves
        index <- which(exogs == var)
        exogsreg <- paste(exogsreg, "+", vbywave[[w]][var])

      }

    }

  }

  # Creating the constants part of the equations, which obviously doesn't change
  if (!is.null(constants)) {

    for (var in constants) {

      index <- which(constants == var)
      if (index == 1) {

        creg <- paste("+", constants[index])

      } else {

        creg <- paste(creg, "+", constants[index])

      }

    }

  }

  # Creating endogenous IV covariances
  if (!is.null(endogs)) { # Checking if there are any endogenous variables
    # Iterating through endogenous variables
    for (var in endogs) {
      # Within each variable, need to iterate through each wave
      for (w in (start - endogs_lags[var]):(end - (endogs_lags[var] - last.wave))) {
        # with last.wave = T, w2 can be bigger than number of waves
        if (w > end) {next}

        # Create beginning of equation, including prior wave of its DV
        if (w > start - endogs_lags[var]) { # Only if there *is* a prior wave

          reg <- paste(vbywave[[w]][endogs[var]], "~~",
                       vbywave[[w - 1]][endogs[var]])

        } else {

          reg <- paste(vbywave[[w]][endogs[var]], "~~")

        }

        # Creating new wave variable which we'll add to in the while loop
        w2 <- w - 1
        # Add each lag prior to current wave, stop before trying to add wave 0
        while (w2 > start - endogs_lags[var]) {

          reg <- paste(reg, "+", vbywave[[w2 - 1]][endogs[var]])
          w2 <- w2 - 1

        }

        # Add other endogenous IVs if they exist, but not if last one in list
        if (length(endogs) > 1 && which(endogs == var) != length(endogs)) {

          # Add 1 to avoid including the var on the left-hand side of equation
          vindex <- which(endogs == var) + 1
          # Add all endogenous vars that don't yet have covariance with this var
          for (var2 in endogs[vindex:length(endogs)]) {

            # if (w >= start) {
            #
            #   reg <- paste(reg, "+", vbywave[[w]][var2])
            #
            # }

            w2 <- (end - (endogs_lags[var2] - last.wave))
            # with last.wave = T, w2 can be bigger than number of waves
            if (w2 > end) {w2 <- end}
            while (w2 >= start - endogs_lags[var2]) {

              reg <- paste(reg, "+", vbywave[[w2]][var2])
              w2 <- w2 - 1

            }

          }

        }

        if (!is.null(exogs)) { # Add the exogenous portion if needed
          reg <- paste(reg, exogsreg)
        }

        if (!is.null(constants)) { # Add the constants if needed
          reg <- paste(reg, creg)
        }

        # Add the only wave of DV not predicted in main_eqs
        for (wp in 1:(start - 1)) {

          if (start > 1) {

            reg <- paste(reg, "+", vbywave[[wp]][dv])

          }

        }

        ## Quick and dirty fix for extraneous plus sign for first wave equation
        reg <- gsub("~~ +", "~~", reg, fixed = TRUE)

        # Because these equations get so long, I'm adding a newline character
        reg <- paste(reg, "\n")

        # Add finished equation to list
        endogs_covs <- c(endogs_covs, reg)
        reg <- NULL

      }
    }
  }
  reg <- NULL

  # If there are more endogenous IVs, we need to make equations for them too
  # if (length(endogs) > 1) {
  #
  #   for (var in endogs[2:length(endogs)]) {
  #
  #     for (w in waves[(1 + start):length(waves)]) {
  #
  #       index2 <- which(endogs == var)
  #
  #       if (w - 1 > endogs_lags[index2]) {
  #         reg <- paste(vbywave[[w]][endogs[index2]], " ~~ ",
  #                      vbywave[[w-1]][endogs[index2]],
  #                      sep = "")
  #         w2 <- w - 1
  #         while (w2 > start + endogs_lags[index2]) {
  #           reg <- paste(reg, " + ", vbywave[[w2-1]][endogs[index2]],
  #                        sep = "")
  #           w2 <- w2 - 1
  #         }
  #       } else {
  #
  #         reg <- paste(vbywave[[w]][endogs[index2]], "~~")
  #
  #       }
  #
  #       if (length(endogs) > 1) {
  #         for (varn in endogs[!(endogs == var)]) {
  #
  #           if (w - 1 > start + endogs_lags[which(endogs == varn)]) {
  #             reg <- paste(reg, " + ", vbywave[[w-1]][varn],
  #                          sep = "")
  #           }
  #
  #         }
  #
  #         w2 <- w
  #         while (w2 > start + endogs_lags[which(endogs == varn)]) {
  #           reg <- paste(reg, " + ", vbywave[[w2-1]][varn],
  #                        sep = "")
  #           w2 <- w2 - 1
  #         }
  #       }
  #
  #       if (!is.null(exogs)) {
  #         reg <- paste(reg, exogsreg)
  #       }
  #
  #       if (!is.null(constants)) {
  #         reg <- paste(reg, creg)
  #       }
  #
  #       reg <- paste(reg, " + ", vbywave[[start]][dv], sep = "")
  #
  #       endogs_eqs[[w-start]][index2] <- reg
  #
  #     }
  #   }
  # }

###### Exogenous tv covariances ###############################################

  exogs_covs <- c()
  if (!is.null(exogs)) {

    for (var in exogs) {

      for (w in (start - exogs_lags[var]):(end - (exogs_lags[var] - last.wave))) {
        if (w > end) {next}

        # Create beginning of equation, including prior wave of its DV
        if (w > start - exogs_lags[var]) { # Only if there *is* a prior wave

          reg <- paste(vbywave[[w]][exogs[var]], "~~",
                       vbywave[[w - 1]][exogs[var]])

        } else {

          reg <- paste(vbywave[[w]][exogs[var]], "~~")

        }

        w2 <- w - 1
        while (w2 > start - exogs_lags[var]) {

          reg <- paste(reg, "+", vbywave[[w2 - 1]][exogs[var]])
          w2 <- w2 - 1

        }

        if (length(exogs) > 1 && which(exogs == var) != length(exogs)) {
          # Add other exogenous IVs if they exist but not for last in the list

          vindex <- which(exogs == var) + 1 # Add 1 to avoid including the var
          for (var2 in exogs[vindex:length(exogs)]) {

            index <- which(exogs == var2)

            # if (w >= start) {
            #
            #   reg <- paste(reg, "+", vbywave[[w]][var2])
            #
            # }

            w2 <- (end - (exogs_lags[var2] - last.wave))
            # with last.wave = T, w2 can be bigger than number of waves
            if (w2 > end) {w2 <- end}

            while (w2 >= start - exogs_lags[var2]) {

              reg <- paste(reg, "+", vbywave[[w2]][var2])
              w2 <- w2 - 1

            }

          }

        }

        if (!is.null(constants)) { # Add the constants if needed
          reg <- paste(reg, creg)
        }

        # Add the only wave of DV not predicted in main_eqs
        for (wp in 1:(start - 1)) {

          if (start > 1) {

            reg <- paste(reg, "+", vbywave[[wp]][dv])

          }

        }

        ## Quick and dirty fix for extraneous plus sign for first wave equation
        reg <- gsub("~~ +", "~~", reg, fixed = TRUE)

        # Add finished equation to list
        exogs_covs <- c(exogs_covs, reg)
        reg <- NULL

      }
    }
  }
  reg <- NULL

######## Constants covariances ################################################

  constants_covs <- c()
  if (!is.null(constants)) {

    for (c in constants) {

      ## Covary with all values of DV prior to first regression
      reg <- paste(c, "~~", vbywave[[1]][dv])
      if (start > 2) { # This is needed if there is more than one lag

        for (wp in 2:(start - 1)) {

          reg <- paste(reg, "+", vbywave[[wp]][dv])

        }

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
    for (w in start:end - 1) {

      if (w == 1 && start != 1) {next}
      # if ()

      reg <- paste(vbywave[[w]][dv], " ~~", sep = "")

      for (var in endogs) {

        w2 <- w + 1

        while (w2 <= end - (endogs_lags[var] - last.wave)) {
          if (w2 > end) {next}

          if ((w2 - 1) == w & which(endogs == var) == 1) {
          # Avoiding extraneous plus sign the first time through

            reg <- paste(reg, vbywave[[w2]][var])

          } else {

            reg <- paste(reg, "+", vbywave[[w2]][var])

          }

          w2 <- w2 + 1

        }

      }

      if (length(grep("~~$", reg)) > 0) {

        # Do nothing because it's an empty equation

      } else {

        endogs_errs <- c(reg, endogs_errs)

      }
      reg <- NULL

    }

  }

####### DV error variance equations (optional) #################################


  # If user wants DV error variances for each wave to be held constant, do it
  if (err.inv == TRUE) {

    err_var_eqs <- c()
    for (w in (start):length(waves)) {

      eq <- paste(vbywave[[w]][dv], " ~~ ", "v*", vbywave[[w]][dv], sep = "")
      err_var_eqs <- c(err_var_eqs, eq)

    }

    err_var_eqs_ann <- "## Holding DV error variance constant for each wave (optional)"

  } else {# Define them as NULL and they won't be included in model string

    err_var_eqs_ann <- NULL
    err_var_eqs <- NULL

  }


###### DV variance constraints #######################################

  dv_vars <- c()
  dv_vars_ann <- NULL

  if (const.inv == TRUE) {

      for (w in start:end) {

        reg <- paste(vbywave[[w]][dv], " ~ ", "var*1", sep = "")

        # Add finished equation to list
        dv_vars <- c(dv_vars, reg)
        reg <- NULL

      }

    dv_vars_ann <- "## Hold DV variance constant each wave (optional)"

  }
  reg <- NULL



###### Defining annotations to be used in final model string ##################

  alpha_eq_ann <- "## Alpha latent variable (fixed effects)"
  alpha_reg_ann <- "## Alpha regression (fixed effects)"
  main_eqs_ann <- "## Main regressions"
  endogs_errs_ann <- "## Correlating DV errors with future values of predetermined predictors"
  endogs_covs_ann <- "## Predetermined predictors covariances"
  exogs_covs_ann <- "## Exogenous (varying and invariant) predictors covariances"

  # Now save all parts of the lavaan model to one object
  all_eqs <- list(main_eqs_ann, main_eqs, alpha_eq_ann, alpha_eq,
                  alpha_reg_ann, alpha_reg, endogs_errs_ann, endogs_errs,
                  endogs_covs_ann, endogs_covs,
                  exogs_covs_ann, exogs_covs, constants_covs,
                  err_var_eqs_ann, err_var_eqs, dv_vars_ann, dv_vars)


  # Adding line breaks between the equations
  out <- lapply(all_eqs, concat) # Concat is internal function defined in pdata.R

  # Adding extra line breaks between each set of equations
  out <- concat2(out) # Concat 2 is internal function defined pdata.R

  if (print == TRUE) {
    cat(out, "\n")
  }

  ret_obj <- list(model = out, data = wframe, complete_obs = complete_obs,
                  start = start, end = end)
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
