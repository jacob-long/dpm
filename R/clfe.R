#' @title Cross-lagged panel models with fixed effects
#'
#' @description Estimate dynamic panel models with fixed effects via
#' maximum likelihood estimation.
#'
#' @param formula Model formula. See details for instructions on specifying
#' parameters properly.
#' @param data Data frame in "long" format. Can be a "panel_data" object.
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
#' @param last.wave Use the final wave of lagged predictors? Default is FALSE,
#'  but enables better specification of 3-wave lagged models.
#' @param print.only Instead of estimating the model, print the \pkg{lavaan}
#'  model string to the console instead.
#' @param digits How many digits should be printed in the model summary?
#'  Default is 3.
#' @param ... Extra parameters to pass \code{\link[lavaan]{sem}}. Examples
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
#' @return An object of class "clfe," which has its own \code{summary} method.
#'
#'  The clfe object also returns: \itemize{
#'
#'  \item \code{fit}, the \code{\link[lavaan]{lavaan}} object fit by this
#'   function. This could be useful if you want to use lavaan's functions to
#'   further learn from the model
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
#' @export
#' @rdname clfe
#' @importFrom lavaan sem lavInspect
#'
#' @examples
#' # Load example data
#' library(panelr)
#' data("WageData")
#' # Convert data to panel_data format for ease of use
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' # Replicates Allison, Williams, & Moral-Benito (2017) analysis
#' fit <- clfe(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
#'             err.inv = TRUE, information = "observed")
#' # Note: information = "observed" only needed to match Stata/SAS standard errors
#' summary(fit)
#'
#'

clfe <- function(formula, data, id = NULL, wave = NULL,
                err.inv = FALSE, const.inv = FALSE,
                alpha.free = FALSE, last.wave = FALSE,
                print.only = FALSE, digits = 3, ...) {

  if (class(data)[1] != "panel_data") {

    if (!is.null(substitute(id)) && !is.null(substitute(wave))) {

      if (!("data.frame" %in% class(data))) {
        stop("data argument must be a data frame.")
      }

      id <- as.character(substitute(id))
      wave <- as.character(substitute(wave))

      data <- panelr::panel_data(data, id, wave)

    }

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
                         constants = constants, id = "id", wave = "wave",
                         err.inv = err.inv, const.inv = const.inv,
                         alpha.free = alpha.free,
                         last.wave = last.wave)

  if (print.only == TRUE) {
    cat(model$model)
    return(invisible(model$model))
  }

  s <- lavaan::sem(model = model$model, data = model$data, ...)

  numwaves <- length(unique(mf$data[["wave"]]))
  nobs_o <- length(unique(data[["id"]]))
  nobs_used <- lavaan::lavInspect(s, what = "ntotal")

  pred.labs <- c()
  pred.lags <- c()

  if (!is.null(endogs)) {
    pred.labs <- model$endogs
    pred.lags <- model$endogs_lags
  }

  if (!is.null(exogs)) {
    pred.labs <- c(pred.labs, model$exogs)
    pred.lags <- c(pred.lags, model$exogs_lags)
  }

  if (!is.null(constants)) {
    pred.labs <- c(pred.labs, constants)
    pred.lags <- c(pred.lags, rep(0, length(constants)))
  }

  out <- list(fit = s, mod_string = model$model, wide_data = model$data)

  out <- structure(out, pred.labs = pred.labs, pred.lags = pred.lags,
                   dv = dv, tot_obs = nobs_o,
                   complete_obs = model$complete_obs, endogs = endogs,
                   exogs = exogs, start = model$start, end = model$end,
                   digits = digits)

  class(out) <- "clfe"

  invisible(out)

}


##### clfe summary #############################################################

#' @export

summary.clfe <- function(object, ...) {

  # Save coefficient vector
  coefs <- lavaan::parameterestimates(object$fit)

  # Get attributes
  a <- attributes(object)

  # Figure out where in the list the first and final fixed coefficient is
  p_index <- which(coefs$label == "p")[1]
  f_index <- which(coefs$label != "")[1]
  fixed_coefs <- coefs[f_index:p_index,]

  # Cut out some of the extraneous info
  fixed_coefs <- fixed_coefs[,c("label","est","se","z","pvalue")]

  pretty_names <- c(a$pred.labs, paste(a$dv, "(t - 1)"))
  ts <- 0
  for (i in 1:(length(pretty_names) - 1)) {

    if (a$pred.lags[i] > 0) {

      pretty_names[i] <- paste(pretty_names[i], " (t - ", a$pred.lags[i],
                               ")", sep = "")
      ts <- ts + 1

    }

  }

  coeft <- as.matrix(cbind(fixed_coefs[,"est"],fixed_coefs[,"se"],
                           fixed_coefs[,"z"],fixed_coefs[,"pvalue"],
                           rep(0, nrow(fixed_coefs))))
  rownames(coeft) <- pretty_names
  colnames(coeft) <- c("Est.","S.E.","z-value","p","")

  pvals <- coeft[,"p"]
  coeft <- round(coeft, a$digits)

  sigstars <- c()
  for (y in 1:nrow(coeft)) {
    if (pvals[y] > 0.1) {
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
  coeft <- as.table(coeft)

  converged <- lavaan::lavInspect(object$fit, what = "converged")
  iters <- lavaan::lavInspect(object$fit, what = "iterations")

  fitms <- lavaan::fitmeasures(object$fit)
  fitms <- round(fitms, a$digits)

  out <- list(coefficients = coeft, model = object, fitmeasures = fitms)
  class(out) <- "summary.clfe"
  out <- structure(out, dv = a$dv, tot_obs = a$tot_bs,
                   complete_obs = a$complete_obs, start = a$start, end = a$end,
                   converged = converged, iters = iters)
  return(out)

}

#' @export

print.summary.clfe <- function(x, ...) {

  a <- attributes(x)
  fitms <- x$fitmeasures
  coeft <- x$coefficients


  cat("MODEL INFO\n")
  cat("Dependent variable:", a$dv, "\n")
  cat("Total observations:", a$tot_obs, "\n")
  cat("Complete observations:", a$complete_obs, "\n")
  cat("Time periods:", a$start, "-", a$end, "\n\n")

  cat("MODEL FIT\n")
  cat("Chi-squared (", fitms["df"], ") = ", fitms["chisq"], "\n",
      sep = "")

  # cat("CFI =", fitms["cfi"],"\n") # Can't trust these yet
  # cat("TLI =", fitms["tli"],"\n")
  #
  cat("RMSEA = ", fitms["rmsea"],", ", "90% CI [", fitms["rmsea.ci.lower"],
      ", ", fitms["rmsea.ci.upper"],"]", "\np(RMSEA < .05) = ",
      fitms["rmsea.pvalue"], "\n", sep = "")
  cat("SRMR =", fitms["srmr"], "\n\n")

  print(coeft)

  cat("\n")

  if (a$converged == TRUE) {
    cat("Model converged after", a$iters, "iterations\n")
  } else {
    cat("WARNING: Model failed to converge after", a$iters, "iterations\n")
  }

}

##### lavaaan summary methods #################################################

#' @title lavaan interface to clfe objects
#' @description These are essentially shortcuts to commonly-desired lavaan
#'  functions to be used with clfe objects, which contain a lavaan model.
#' @param x The \code{\link{clfe}} object
#' @param ... Other arguments to the the lavaan function.
#' @details Here are the currently supported functions with a brief explantion:
#'
#'   \itemize{
#'
#'     \item \code{lav_summary}: lavaan's summary function.
#'     \item \code{lav_fitmeasures}: lavaan's \code{summary}
#'
#'   }
#'
#' @examples
#'
#' # Load example data
#' data("WageData")
#' # Convert data to panel_data format for ease of use
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' fit <- clfe(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)
#' lav_summary(fit)
#' lav_fitmeasures(fit)
#'
#' @rdname lavaan-functions
#' @export

lav_summary <- function(x, ...) {

  lavaan::summary(x$fit, ...)

}

#' @rdname lavaan-functions
#' @export

lav_fitmeasures <- function(x, ...) {

  lavaan::fitmeasures(x$fit, ...)

}


