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
#' @param error.inv Constrain the error variance to be equal across waves.
#'  Default is FALSE.
#' @param const.inv Constrain the dependent variable's variance to be equal
#'  across waves (or makes its intercept equal across waves).
#'  This removes cross-sectional dependence. Default is FALSE.
#' @param alpha.free Estimate each wave of the dependent variable's loading on
#'  the alpha latent variable. Default is FALSE, meaning each wave has a loading
#'  of 1.
#' @param y.lag Which lag(s) of the dependent variable to include in the
#'  regression. Default is 1, but any number or vector of numbers can be used.
#' @param y.free If TRUE, allows the regression coefficient(s) for the lagged
#'  dependent variable to vary over time. Default is FALSE. You may alternately
#'  provide a number or vector of numbers corresponding to which lags should
#'  vary freely.
#' @param x.free If TRUE, allows the regressions coefficient(s) for the
#'  predictor(s) to vary over time. Default is FALSE. If TRUE, the predictor
#'  regression coefficient(s) can vary over time. Alternately, you may provide
#'  a character vector of predictors to allow to vary if you only want a subset
#'  of predictors to vary.
#' @param fixed.effects Fit a fixed effects model? Default is TRUE. If FALSE,
#'  you get a random effects specification instead.
#' @param partial.pre Make lagged, predetermined predictors (i.e., they are 
#'  surrounded by pre() in the model formula) correlated with the contemporaneous
#'  error term, as discussed in Allison (2022)? Default is FALSE.
#' @param print.only Instead of estimating the model, print the \pkg{lavaan}
#'  model string to the console instead.
#' @param err.inv Deprecated, same purpose as `error.inv`.
#' @param weights Equivalent to the argument to `lm`, presumably the unquoted
#'  name of a variable in the data that represents the weight. It is passed
#'  to `lavaan()`'s `sampling.weights` argument.
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
#'  If you would like to include an interaction between time-varying and
#'  time-invariant predictors, you can add a third part to the formula to
#'  specify that term.
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
#'  so it does not work the same way as the built-in lag function (i.e., it
#'  understands that you can only lag values *within* entities).
#'
#'  **Note**: CFI and TLI model fit measures for these models should not be
#'  used. They are anti-conservative compared to other implementations and
#'  we have not yet figured out how to get more plausible values.
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
#' @author Jacob A. Long, in consultation with Richard A. Williams and
#'  Paul D. Allison. All errors are Jacob's.
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
#' @importFrom stats as.formula
#' @import rlang
#' @import jtools
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
#'             error.inv = TRUE, information = "observed")
#' # Note: information = "observed" only needed to match Stata/SAS standard errors
#' summary(fit)
#'
#'

dpm <- function(formula, data, error.inv = FALSE, const.inv = FALSE,
                alpha.free = FALSE, y.lag = 1, y.free = FALSE,
                x.free = FALSE, fixed.effects = TRUE, partial.pre = FALSE,
                print.only = FALSE, id = NULL, wave = NULL, err.inv = NULL,
                weights = NULL, ...) {

  formula <- Formula::Formula(formula)
  dv <- as.character((attr(formula, "lhs")))

  # Check data integrity
  if (!is_panel(data)) {
    if (!is.data.frame(data)) {
      stop_wrap("data argument must be a data frame.")
    }

    id <- as_name(enquo(id))
    wave <- as_name(enquo(wave))
    data <- panelr::panel_data(data, !! sym(id), !! sym(wave))
  } else {
    wave <- panelr::get_wave(data)
    id <- panelr::get_id(data)
  }

  # Deal with non-numeric wave variables
  if (!is.numeric(data[[wave]])) {
    periods <- unique(data[[wave]])
    data[[wave]] <- as.numeric(data[[wave]])
  } else {
    # Coerce to 1-2-3-4-5 etc.
    periods <- unique(data[[wave]])
    data[[wave]] <- as.numeric(as.factor(data[[wave]]))
  }

  # Catch deprecated arg.
  if (!is.null(err.inv)) {
    error.inv <- err.inv
    msg_wrap("err.inv is deprecated. Please use error.inv instead.")
  }

  # Helper function to get info from model formula
  pf <- formula_parser(formula, dv, data)

  # Need to add interaction variables to data frame before calling model_frame
  if (length(pf$wint_labs) > 0 || length(pf$cint_labs) > 0 ||
      length(pf$bint_labs) > 0) {
    for (int in c(pf$wint_labs, pf$cint_labs, pf$bint_labs)) {
      new_name <- make_names(int, TRUE)
      pf$data <- dplyr::mutate(pf$data, !! new_name := !! rlang::parse_expr(int))
      pf$allvars <- c(pf$allvars, new_name)
    }
  }

  # Get the weights argument like lm() does (can be name or object)
  weights <- eval_tidy(enquo(weights), pf$data)
  # Append to data with special name
  if (!is.null(weights)) {
    pf$data[".weights"] <- weights
    pf$allvars <- c(pf$allvars, ".weights")
  }

  ## Using model_frame to allow for variable transformations in formulae
  # Requires two different strategies depending on presence/absence of lagged
  # variables since panel_model_frame returns different type of object.
  mod_formula <- paste("~", paste(pf$allvars, collapse = " + "))
  mod_formula <- as.formula(mod_formula)
  mf <- panelr::model_frame(mod_formula, data = pf$data)
  names(mf) %just% pf$v_info$root <- make_names(names(mf) %just% pf$v_info$root)
  names(mf) %just% pf$constants <- make_names(names(mf) %just% pf$constants)
  pf$constants <- make_names(pf$constants)
  names(mf) %just% dv <- make_names(names(mf) %just% dv)
  dv <- make_names(dv)

  # Quick little helper
  get_raw_vars <- function(vars) {
    sapply(vars, function(x) {all.vars(formula(paste0("~", x)))})
  }

  # Checking that the x.free argument is valid
  if (!is_false(x.free)) {
    # If it's true, I'll convert to a character vector of all varying preds
    if (is_true(x.free)) {
      x.free <- c(get_raw_vars(pf$endogs), get_raw_vars(pf$exogs), pf$constants)
    }
    if (any(x.free %nin% c(get_raw_vars(pf$endogs), get_raw_vars(pf$exogs),
                           pf$constants))) {
      stop_wrap(paste0(x.free %not% c(pf$endogs, pf$exogs), collapse = " and "),
                "provided to 'x.free' but not found among predictors.")
    }
  }

  # Helper function to write the lavaan syntax
  model <- model_builder(mf = mf, pf = pf, dv = dv, endogs = pf$endogs,
                         exogs = pf$exogs,
                         constants = pf$constants, id = id, wave = wave,
                         err.inv = error.inv, const.inv = const.inv,
                         alpha.free = alpha.free, y.lag = y.lag,
                         y.free = y.free, x.free = x.free,
                         fixed.effects = fixed.effects, 
                         partial.pre = partial.pre, weights = !is.null(weights))

  # If only printing is wanted, just print it and stop
  if (print.only == TRUE) {
    cat(model$model)
    return(invisible(model$model))
  }

  weights_arg <- if (!is.null(weights)) substitute(".weights") else NULL

  # Fit the model with lavaan
  s <- lavaan::sem(model = model$model, data = model$data,
                   sampling.weights = weights_arg, ...)

  nobs_o <- length(unique(data[[id]]))
  # nobs_used <- lavaan::lavInspect(s, what = "ntotal")

  # Initialize the dpm object
  out <- as(s, Class = "dpm")
  out@mod_string <- model$model
  out@wide_data <- model$data
  out@call <- sys.call()
  out@formula <- Formula::Formula(formula)

  out@call_info <- list(dv = dv, tot_obs = nobs_o,
                        complete_obs = model$complete_obs, endogs = pf$endogs,
                        exogs = pf$exogs, start = periods[model$start],
                        end = periods[model$end],
                        y.lag = y.lag, var_coefs = model$var_coefs,
                        y.free = y.free, x.free = x.free,
                        fixed.effects = fixed.effects,
                        alpha.free = alpha.free, const.inv = const.inv,
                        error.inv = error.inv, v_info = pf$v_info)

  return(out)

}

##### dpm summary #############################################################

#' @rdname summary.dpm
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
#' 
#' 
#' @return Returns a `summary.dpm` object, which is a list with three elements:
#'  
#'  * `model`: The `dpm` object.
#' 
#'  * `coefficients`: A data frame containing coefficient estimates,
#'   standard errors, p values, and so on.
#' 
#'  * `fitmeasures`: A numeric vector containing model fit information.
#' 
#' The primary function of the object is to be printed to the console.
#' 
#' @export
#' @importFrom crayon underline italic
#' @importFrom stats4 summary

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

  which_cols <- c("lhs", which_cols)
  which_coefs <- c("lhs", which_coefs)

  # Cut out some of the extraneous info
  if (is_false(a$y.free) & is_false(a$x.free)) {
    fixed_coefs <- fixed_coefs[!duplicated(fixed_coefs[,"label"]), which_coefs]
  } else {
    fixed_coefs <- fixed_coefs[fixed_coefs[,"op"] == "~", which_coefs]
  }

  # Combine info from variable transformations
  a$var_coefs <- merge(a$var_coefs, a$v_info, by.y = c("new_name", "lag"),
                       by.x = c("var", "lag"),
                       suffixes = c("", ".y"), all.x = TRUE)
  # Deal with constants not present in v_info
  a$var_coefs$root[is.na(a$var_coefs$root)] <-
                                      a$var_coefs$var[is.na(a$var_coefs$root)]
  # pander can't print *
  a$var_coefs$root <- gsub("\\*", ":", a$var_coefs$root)
  # Instantiate pretty names vector
  pretty_names <- c(a$var_coefs$root)
  # Create column of pretty names
  a$var_coefs$pretty_name <- NA
  for (i in 1:(length(pretty_names))) {
    if (a$var_coefs$lag[i] > 0) {
      pretty_names[i] <- paste(pretty_names[i], " (t - ", a$var_coefs$lag[i],
                               ")", sep = "")
      a$var_coefs$pretty_name[i] <- pretty_names[i]
    } else {
      a$var_coefs$pretty_name[i] <- a$var_coefs$root[i]
    }
  }

  coeft <- as.data.frame(fixed_coefs[,which_coefs])
  coeft <- coeft %not% c("label")

  coeft$coef <-
    a$var_coefs$pretty_name[sapply(fixed_coefs[,"label"], function(x) {
      which(a$var_coefs$coef == x)
    })]
  colnames(coeft) <- c(which_cols, "coef")
  coeft <- as.data.frame(coeft)

  coeft$lhs <- gsub(paste0(a$dv, "_"), "", coeft$lhs, fixed = TRUE)
  names(coeft) %just% "lhs" <- "t"

  converged <- lavaan::lavInspect(object, what = "converged")
  iters <- lavaan::lavInspect(object, what = "iterations")

  fitms <- lavaan::fitmeasures(object)
  fitms <- round(fitms, digits)

  out <- list(coefficients = coeft, model = object, fitmeasures = fitms)
  class(out) <- "summary.dpm"
  out <- structure(out, dv = a$dv, tot_obs = a$tot_obs,
                   complete_obs = a$complete_obs, start = a$start, end = a$end,
                   converged = converged, iters = iters, y.free = a$y.free,
                   x.free = a$x.free, digits = digits, pvalue = pvalue)
  return(out)

})

#' @importFrom crayon inverse
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

  if (!all(is_false(a$y.free)) | !all(is_false(a$x.free))) {
    coeft <- split(coeft, coeft$t)
    names(coeft) <- gsub(x$model@call_info$dv, "", names(coeft))
    for (i in 1:length(coeft)) {
      cat(inverse("t = ", names(coeft)[i], "\n", sep = ""))
      rownames(coeft[[i]]) <- coeft[[i]][["coef"]]
      print(md_table(coeft[[i]] %not% c("t", "coef"), digits = a$digits,
                     sig.digits = FALSE,
                     format = getOption("dpm.table.format", "markdown")))
      cat("\n")
    }
  } else {
    rownames(coeft) <- coeft$coef
    print(md_table(coeft %not% c("t", "coef"), digits = a$digits,
                   sig.digits = FALSE,
                   format = getOption("dpm.table.format", "markdown")))
    cat("\n")
  }

  if (a$converged == TRUE) {
    cat("Model converged after", a$iters, "iterations\n")
  } else {
    cat("WARNING: Model failed to converge after", a$iters, "iterations\n")
  }

}

##### Other methods ###########################################################

#' @rdname dpm-methods
#' @export
setGeneric("update")

#' @title Various methods for `dpm` objects
#' @description R likes it when these things have documentation.
#' @param object A `dpm` object
#' @param x A `dpm` object
#' @param formula. An updated formula (optional)
#' @param evaluate If updating, should the updated model be updated or just
#'  return the call? Default is TRUE, re-run the model.
#' @param ... Other arguments to update.
#' @return 
#'   **`update.dpm()`**: Returns an updated `dpm` object.
#' 
#'   **`coef.dpm()`**: Returns a numeric vector of coefficients. If the model 
#'    was fit with `x.free = TRUE` and/or `y.free = TRUE`, the coefficient 
#'    names will be formatted with an underscore and the wave corresponding to 
#'    which time period the coefficient is estimated for.
#' 
#'   **`formula.dpm()`**: Returns the formula used to fit the model as a 
#'    `Formula` object. The formula is the input to `dpm()`, not the `lavaan`
#'     syntax.
#' 
#'   **`show.dpm()`**: Returns an invisible "NULL" while printing model info 
#'    to console.
#' 
#' @examples
#'
#' data("WageData", package = "panelr")
#' wages <- panel_data(WageData, id = id, wave = t)
#' fit <- dpm(wks ~ pre(lag(union)) + lag(lwage), data = wages)
#'
#' # Re-run model without `lag(lwage)` term
#' update(fit, . ~ . - lag(lwage))
#'
#' @method update dpm
#' @import methods
#' @importFrom stats formula getCall update.formula update
#' @rdname dpm-methods
# Almost a duplicate of the default update method, but I need it here since
# lavaan has implemented a different method

update.dpm <- function(object, formula., ..., evaluate = TRUE) {

  call <- match.call(dpm, getCall(object))
  extras <- match.call(expand.dots = FALSE)$...

  if (!missing(formula.)) {
    call$formula <- update(formula(object), formula.)
  }

  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }

  if (evaluate) {eval(call, parent.frame())} else {call}

}

#' @rdname dpm-methods
#' @export
setMethod("update", "dpm", update.dpm)

#' @export
#' @importFrom methods show
#' @rdname dpm-methods

setMethod("show", "dpm", function(object) {

  x <- summary(object)
  a <- attributes(x)
  fitms <- x$fitmeasures

  cat(crayon::cyan("Dynamic Panel Model [dpm]"), ":\n\n", sep = "")

  cat(underline("MODEL INFO:\n"))
  cat(italic("Dependent variable:"), a$dv, "\n")
  cat(italic("Total observations:"), a$tot_obs, "\n")
  cat(italic("Complete observations:"), a$complete_obs, "\n")
  cat(italic("Time periods:"), a$start, "-", a$end, "\n\n")

  cat(underline("MODEL FIT:\n"))
  cat("\U1D6D8\u00B2(", fitms["df"], ") = ", fitms["chisq"], "\n",
      sep = "")

  cat(italic("RMSEA")," = ", fitms["rmsea"], ", ",
      "90% CI [", fitms["rmsea.ci.lower"],
      ", ", fitms["rmsea.ci.upper"],"]", italic("\np(RMSEA < .05)"),
      " = ", fitms["rmsea.pvalue"], "\n", sep = "")
  cat(italic("SRMR"), "=", fitms["srmr"], "\n\n")

  if (a$converged == TRUE) {
    cat("Model converged after", a$iters, "iterations\n")
  } else {
    cat(crayon::red("WARNING: Model failed to converge after", a$iters,
                    "iterations\n"))
  }

})

#' @rdname dpm-methods
#' @export
setGeneric("coef")

#' @rdname dpm-methods
#' @method coef dpm
coef.dpm <- function(object) {
  out <- summary(object)$coefficients[,"Est."]
  coef_names <- summary(object)$coefficients$coef
  if (length(out) != length(unique(coef_names))) {
    coef_names <- paste0(coef_names, "_", summary(object)$coefficients[,1])
  }
  names(out) <- coef_names
  return(out)
}

#' @export
#' @importFrom stats coef
#' @rdname dpm-methods

setMethod("coef", "dpm", coef.dpm)

#' @export
setGeneric("formula")

#' @method formula dpm
formula.dpm <- function(x) {
  return(x@formula)
}

#' @export
#' @importFrom stats formula
#' @rdname dpm-methods

setMethod("formula", "dpm", formula.dpm)

##### lavaaan summary methods #################################################

#' @title lavaan-style summary for dpm objects
#' @description This is just a quick way to get `lavaan`'s summary instead
#'  the more terse summary designed for `dpm` objects.
#' @param x The \code{\link{dpm}} object
#' @param ... Other arguments to the lavaan function.
#'
#' @return Returns a `lavaan.summary` object which contains various
#' model data described in `?lavaan::summary,lavaan-method`. 
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
#' @return 
#' 
#' A `data.frame` with input data transformed from "long" to "wide"
#' format, with just one row per person/entity. Internally, this is
#' generated by calling `panelr::widen_panel()` after some 
#' preprocessing. 
#' 
#' @examples
#'
#' data("WageData", package = "panelr")
#' wages <- panel_data(WageData, id = id, wave = t)
#' fit <- dpm(wks ~ pre(lag(union)) + lag(lwage), data = wages)
#' get_wide_data(fit)
#'
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
#' 
#' @return
#' Returns a string with the lavaan model syntax for `model`. If `print` is
#' TRUE, it is printed to the console as well.
#' 
#' @examples
#'
#' data("WageData", package = "panelr")
#' wages <- panel_data(WageData, id = id, wave = t)
#' fit <- dpm(wks ~ pre(lag(union)) + lag(lwage), data = wages)
#' get_syntax(fit)
#'
#' @export
get_syntax <- function(model, print = TRUE) {
  if (print) {cat(model@mod_string)}
  invisible(model@mod_string)
}


