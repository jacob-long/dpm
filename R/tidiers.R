
#' @title Tidy methods for dpm
#' @description `dpm` objects support the \pkg{broom} package's `tidy` method.
#' @param x A `dpm` object.
#' @param conf.int Logical indicating whether or not to include a confidence
#'  interval in the tidy data frame.
#' @param conf.level The confidence level to use for the confidence interval
#'  when `conf.int` is TRUE. Default is .95, corresponding to a 95% confidence
#'  interval.
#' @param ... Other arguments passed to \link[dpm]{summary.dpm}.
#' @examples
#' if (requireNamespace("broom")) {
#'   library(broom)
#'   # Load example data
#'   data("WageData", package = "panelr")
#'   # Convert data to panel_data format for ease of use
#'   wages <- panel_data(WageData, id = id, wave = t)
#'
#'   fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)
#'   tidy(fit)
#' }
#' @rdname dpm_tidiers
#' @rawNamespace
#' if (getRversion() >= "3.6.0") {
#'   S3method(broom::tidy, dpm)
#' } else {
#'   export(tidy.dpm)
#' }
#'

tidy.dpm <- function(x, conf.int = FALSE, conf.level = .95, ...) {

  s <- summary(x, ci = conf.int, ci.level = conf.level, ...)
  coefs <- s$coefficients
  coefs$term <- coefs$coef
  coefs$estimate <- coefs[,"Est."]
  coefs$std.error <- coefs[,"S.E."]
  coefs$statistic <- coefs[,"z val."]
  coefs$p.value <- coefs[,"p"]
  keep_cols <- c("term", "estimate", "std.error", "statistic", "p.value", "t")
  if (length(unique(coefs$t)) == 1) coefs$t <- NA

  if (conf.int == TRUE) {
    coefs$conf.low <- coefs[, stringr::str_detect(names(coefs), "%")][,1]
    coefs$conf.high <- coefs[, stringr::str_detect(names(coefs), "%")][,2]
    keep_cols <- c(keep_cols, "conf.low", "conf.high")
  }

  if (requireNamespace("tibble")) {
    tibble::as_tibble(coefs[,keep_cols], rownames = NULL)
  } else {
    rownames(coefs) <- NULL
    coefs[,keep_cols]
  }

}

#' @rdname dpm_tidiers
#' @rawNamespace
#' if (getRversion() >= "3.6.0") {
#'   S3method(broom::glance, dpm)
#' } else {
#'   export(glance.dpm)
#' }

glance.dpm <- function(x, ...) {
  s <- summary(x)$fitmeasures
  the_names <- c("df", "chisq", "ntotal", "rmsea", "rmsea.ci.lower",
                 "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  values <- s[the_names]
  names(values) %just% "ntotal" <- "N"
  df <- t(as.data.frame(values))
  if (requireNamespace("tibble")) {
    df <- tibble::as_tibble(df, rownames = NULL)
  } else {
    rownames(df) <- NULL
  }
  return(df)
}

