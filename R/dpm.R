#' @title Dynamic panel models fit with maximum likelihood
#'
#' @description Estimate dynamic panel models with fixed effects via
#' maximum likelihood estimation.
#'
#' @param formula Model formula. See details for instructions on specifying
#' parameters properly.
#' @param data Data frame in "long" format. Prefers a "panel_data" object.
#' @param id Name of the data column that identifies which individual the
#'  observation is. Not needed if \code{data} is a "panel_data" object.
#' @param wave Name of the data column that identifies which wave the
#'  observation is from. Not needed if \code{data} is a "panel_data" object.
#' @param err.inv Constrain the error variance to be equal across waves.
#'  Default is FALSE.
#' @param const.inv Constrain the dependent variable's variance to be equal
#'  across waves. This removes cross-sectional dependence. Default is FALSE.
#' @param alpha.free Estimate each wave of the dependent variable's loading on
#'  the alpha latent variable. Default is FALSE, meaning each wave has a loading
#'  of 1.
#' @param y.lag Which lag(s) of the dependent variable to include in the
#'  regression. Default is 1, but any number of vector of numbers can be used.
#' @param y.free If TRUE, allows the regression coefficient(s) for the lagged
#'  dependent variable to vary over time. Default is FALSE. You may alternately
#'  provide a number or vector of numbers corresponding to which lags should
#'  vary freely.
#' @param fixed.effects Fit a fixed effects model? Default is TRUE. If FALSE,
#'  you get a random effects specification instead.
#' @param print.only Instead of estimating the model, print the \pkg{lavaan}
#'  model string to the console instead.
#' @param ... Extra parameters to pass to \code{\link[lavaan]{sem}}. Examples
#'  could be \code{missing = "fiml"} for missing data or
#'  \code{estimator = "MLM"} for robust estimation.
#'
#' @details
#'
#'  The right-hand side of the formula has two parts, separated by a bar
#'  (\code{|}). The first part should include the time-varying predictors.
#'  The second part, then, is for the time-invariant variables. If you put
#'  a time-varying variable in the second part of the formula, by default
#'  the first wave's value of that variable is treated as the constant.
#'
#'  You must include time-varying predictors. If you do not include a bar
#'  in the formula, all variables are treated as time-varying.
#'
#'  \emph{Predetermined variables}:
#'
#'  To set a variable as predetermined, or weakly exogenous, surround the
#'  variable with a \code{pre} function. For instance, if you want the variable
#'  \code{union} to be predetermined, you could specify the formula like this:
#'  \code{wks ~ pre(union) + lwage | ed}, where \code{wks} is the dependent
#'  variable, \code{lwage} is a strictly exogenous time-varying predictor,
#'  and \code{ed} is a strictly exogenous time-invariant predictor.
#'
#'  To lag a predictor, surround the variable with a \code{lag} function in
#'  the same way. Note that the lag function used is specific to this package,
#'  so it does not work the same way as the built-in lag function.
#'
#'
#' @return An object of class `dpm` which has its own \code{summary} method.
#'
#'  The `dpm` object is an extension of the `lavaan` class and has all
#'  the capabilities of `lavaan` objects, with some extras.
#'
#'  It contains extra slots for: \itemize{
#'
#'  \item \code{mod_string}, the character object used to specify the model
#'   to lavaan. This is helpful if you want to fit the model yourself or
#'   wish to check that the specification is correct.
#'  \item \code{wide_data}, the widened data frame necessary to fit the SEM.
#'
#'  }
#'
#' @author Jacob A. Long, in consultation with Richard A. Williams. All errors
#'  are Jacob's.
#'
#' @references
#'
#' Allison, P. D., Williams, R., & Moral-Benito, E. (2017). Maximum likelihood
#' for cross-lagged panel models with fixed effects. *Socius*, *3*, 1–17.
#' http://journals.sagepub.com/doi/10.1177/2378023117710578
#'
#' @export
#' @rdname dpm
#' @importFrom lavaan sem lavInspect
#' @importFrom methods as
#' @import rlang
#' @importFrom panelr is_panel
#'
#' @examples
#' # Load example data
#' data("WageData", package = "panelr")
#' # Convert data to panel_data format for ease of use
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' # Replicates Allison, Williams, & Moral-Benito (2017) analysis
#' fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
#'             err.inv = TRUE, information = "observed")
#' # Note: information = "observed" only needed to match Stata/SAS standard errors
#' summary(fit)
#'
#'

dpm <- function(formula, data, err.inv = FALSE, const.inv = FALSE,
                alpha.free = FALSE, y.lag = 1, y.free = FALSE,
                fixed.effects = TRUE, print.only = FALSE,
                id = NULL, wave = NULL, ...) {

  # Check data integrity
  if (!is_panel(data)) {

      if (!is.data.frame(data)) {
        stop("data argument must be a data frame.")
      }

      id <- expr_text(enexpr(id))
      wave <- expr_text(enexpr(wave))

      data <- panelr::panel_data(data, !! sym(id), !! sym(wave))

  } else {

    wave <- panelr::get_wave(data)
    id <- panelr::get_id(data)

  }

  pf <- cl_formula_parser(formula)

  allvars <- pf$allvars
  dv <- pf$dv
  constants <- pf$constants
  exogs <- pf$exogs
  endogs <- pf$endogs
  varying <- pf$varying

  mf <- panel_model_frame(allvars, data)

  model <- model_builder(mf = mf, dv = dv, endogs = endogs, exogs = exogs,
                         constants = constants, id = id, wave = wave,
                         err.inv = err.inv, const.inv = const.inv,
                         alpha.free = alpha.free, y.lag = y.lag,
                         y.free = y.free, fixed.effects = fixed.effects)

  if (print.only == TRUE) {
    cat(model$model)
    return(invisible(model$model))
  }

  s <- lavaan::sem(model = model$model, data = model$data, ...)

  # numwaves <- length(unique(mf$data[[wave]]))
  nobs_o <- length(unique(data[[id]]))
  # nobs_used <- lavaan::lavInspect(s, what = "ntotal")

  # out <- list(fit = s, mod_string = model$model, wide_data = model$data)
  out <- as(s, Class = "dpm")
  out@mod_string <- model$model
  out@wide_data <- model$data
  out@call <- sys.call()

  out@call_info <- list(out, dv = dv, tot_obs = nobs_o,
                        complete_obs = model$complete_obs, endogs = endogs,
                        exogs = exogs, start = model$start, end = model$end,
                        y.lag = y.lag, var_coefs = model$var_coefs,
                        y.free = y.free, fixed.effects = fixed.effects,
                        alpha.free = alpha.free, const.inv = const.inv,
                        err.inv = err.inv)

  # class(out) <- "clfe"

  invisible(out)

}


##### dpm summary #############################################################

#' @title Summarize dpm objects
#' @description The summary method is designed to offer similar arguments to
#'   `lavaan`'s summary, but with shorter and more domain-specific output.
#'
#' @param object A `dpm` object.
#' @param digits How many digits should be printed in the model summary?
#'  Default is 3. You can set a default by setting the option `"dpm-digits"`.
#' @param standardized Use `lavaan`'s method for standardizing coefficients?
#'  Default is FALSE.
#' @param ci Show confidence intervals? Default is FALSE.
#' @param se Show standard errors? Default is TRUE.
#' @param pvalue Show p values? Default is TRUE.
#' @param ci.level How wide should the confidence intervals be? Ignored if
#'  `ci` is FALSE. Default is .95.
#' @param zstat Show the z statistic? Default is TRUE.
#' @param boot.ci.type If the model was fit with bootstrapped standard errors
#'  and `ci` is TRUE, which method should be used for finding the intervals?
#'  Default is `"perc"`.
#' @param ... Ignored.
#' @export
#' @importFrom crayon underline italic

setMethod("summary", "dpm",
          function(object, standardized = FALSE, ci = FALSE, se = TRUE,
                   zstat = TRUE, pvalue = TRUE, ci.level = .95,
                   boot.ci.type = c("perc", "norm", "basic", "bca.simple"),
                   digits = getOption("dpm-digits", 3), ...) {

  # Save coefficient vector
  # TODO: give user more options here
  coefs <- lavaan::parameterestimates(object, se = TRUE, zstat = TRUE,
                                      pvalue = TRUE, ci = TRUE,
                                      standardized = standardized,
                                      boot.ci.type = boot.ci.type[1],
                                      level = ci.level)

  # Get attributes
  a <- object@call_info

  # Figure out where in the list the first and final fixed coefficient is
  fixed_coefs <- coefs[coefs$label %in% unique(a$var_coefs$coef),]

  which_coefs <- c(
    "label",
    "est",
    if (!se) {NULL} else {"se"},
    if (!ci) {NULL} else {c("ci.lower", "ci.upper")},
    if (!zstat) {NULL} else {"z"},
    if (!pvalue) {NULL} else {"pvalue"}
  )

  which_cols <- unlist(sapply(which_coefs[-1], function(x) {
    switch(x,
           "est" = "Est.",
           "se" = "S.E.",
           "ci.lower" = make_ci_labs(ci.level)[[1]],
           "ci.upper" = make_ci_labs(ci.level)[[2]],
           "z" = "z val.",
           "pvalue" = "p")
  }))

  if ("p" %in% which_cols) {which_cols <- c(which_cols, "")}

  # Cut out some of the extraneous info
  fixed_coefs <- fixed_coefs[!duplicated(fixed_coefs[,"label"]), which_coefs]

  pretty_names <- c(a$var_coefs$var)
  a$var_coefs$pretty_name <- NA
  ts <- 0
  for (i in 1:(length(pretty_names))) {

    if (a$var_coefs$lag[i] > 0) {

      pretty_names[i] <- paste(pretty_names[i], " (t - ", a$var_coefs$lag[i],
                               ")", sep = "")
      a$var_coefs$pretty_name[i] <- pretty_names[i]
      ts <- ts + 1

    } else {

      a$var_coefs$pretty_name[i] <- a$var_coefs$var[i]

    }

  }

  if (pvalue == TRUE) {
    coeft <- as.matrix(cbind(fixed_coefs[which_coefs], rep(0, nrow(fixed_coefs))))
  } else {
    coeft <- as.matrix(fixed_coefs[which_coefs])
  }

  coeft <- coeft[,colnames(coeft) %nin% "label"]

  rownames(coeft) <-
    a$var_coefs$pretty_name[sapply(fixed_coefs[,"label"], function(x) {
      which(a$var_coefs$coef == x)
    })]
  colnames(coeft) <- which_cols
  coeft <- as.data.frame(coeft)
  coeft <- round_df_char(coeft, digits)

  if (pvalue == TRUE) {
    pvals <- coeft[,"p"]

    sigstars <- c()
    for (y in 1:nrow(coeft)) {
      if (!is.finite(y) || pvals[y] > 0.1) {
        sigstars[y] <- ""
      } else if (pvals[y] <= 0.1 & pvals[y] > 0.05) {
        sigstars[y] <- "."
      } else if (pvals[y] > 0.01 & pvals[y] <= 0.05) {
        sigstars[y] <- "*"
      } else if (pvals[y] > 0.001 & pvals[y] <= 0.01) {
        sigstars[y] <- "**"
      } else if (pvals[y] <= 0.001) {
        sigstars[y] <- "***"
      }
    }

    coeft[,ncol(coeft)] <- sigstars
    names(coeft)[ncol(coeft)] <- ""
  }

  converged <- lavaan::lavInspect(object, what = "converged")
  iters <- lavaan::lavInspect(object, what = "iterations")

  fitms <- lavaan::fitmeasures(object)
  fitms <- round(fitms, digits)

  out <- list(coefficients = coeft, model = object, fitmeasures = fitms)
  class(out) <- "summary.dpm"
  out <- structure(out, dv = a$dv, tot_obs = a$tot_obs,
                   complete_obs = a$complete_obs, start = a$start, end = a$end,
                   converged = converged, iters = iters)
  return(out)

})

#' @export

print.summary.dpm <- function(x, ...) {

  a <- attributes(x)
  fitms <- x$fitmeasures
  coeft <- x$coefficients


  cat(underline("MODEL INFO:\n"))
  cat(italic("Dependent variable:"), a$dv, "\n")
  cat(italic("Total observations:"), a$tot_obs, "\n")
  cat(italic("Complete observations:"), a$complete_obs, "\n")
  cat(italic("Time periods:"), a$start, "-", a$end, "\n\n")

  cat(underline("MODEL FIT:\n"))
  cat("\U1D6D8\u00B2(", fitms["df"], ") = ", fitms["chisq"], "\n",
      sep = "")

  # cat("CFI =", fitms["cfi"],"\n") # Can't trust these yet
  # cat("TLI =", fitms["tli"],"\n")
  #
  cat(italic("RMSEA")," = ", fitms["rmsea"], ", ",
      "90% CI [", fitms["rmsea.ci.lower"],
      ", ", fitms["rmsea.ci.upper"],"]", italic("\np(RMSEA < .05)"),
      " = ", fitms["rmsea.pvalue"], "\n", sep = "")
  cat(italic("SRMR"), "=", fitms["srmr"], "\n\n")

  print(coeft)

  cat("\n")

  if (a$converged == TRUE) {
    cat("Model converged after", a$iters, "iterations\n")
  } else {
    cat("WARNING: Model failed to converge after", a$iters, "iterations\n")
  }

}

##### lavaaan summary methods #################################################

#' @title lavaan-style summary for dpm objects
#' @description This is just a quick way to get `lavaan`'s summary instead
#'  the more terse summary designed for `dpm` objects.
#' @param x The \code{\link{dpm}} object
#' @param ... Other arguments to the the lavaan function.
#'
#' @examples
#'
#' # Load example data
#' data("WageData", package = "panelr")
#' # Convert data to panel_data format for ease of use
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)
#' lav_summary(fit)
#'
#' @rdname lavaan-functions
#' @export

lav_summary <- function(x, ...) {

  lavaan::summary(as(x, "lavaan"), ...)

}

#### extractors ##############################################################

#' @title Retrieve wide-format data from fitted dpm model
#' @description This helper function provides a simple way to retrieve the
#'  widened data from a fitted [dpm::dpm()] object.
#' @param model A `dpm` object.
#' @export
get_wide_data <- function(model) {
  model@wide_data
}

#' @title Retrieve lavaan model syntax from fitted dpm model
#' @description This helper function provides a simple way to retrieve the
#'  lavaan model syntax from a fitted [dpm::dpm()] object.
#' @param model A `dpm` object.
#' @param print Print the syntax to the console so it is formatted properly?
#'  Default is TRUE.
#' @export
get_syntax <- function(model, print = TRUE) {
  if (print) {cat(model@mod_string)}
  invisible(model@mod_string)
}


