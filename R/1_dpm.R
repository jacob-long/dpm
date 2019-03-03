# Tells S4 that I'm using an S3 class for the formula slot
setOldClass("Formula")

#### dpm class ##################################
#' @title Dynamic Panel Model (`dpm`) class
#' @description Models fit using [dpm()] return values of this class, which
#'  inherits from \code{\link[lavaan]{lavaan-class}}.
#' @slot call_info A list of metadata about the arguments used.
#' @slot call The actual function call.
#' @slot mod_string The model formula passed to `lavaan`.
#' @slot wide_data The data provided to the `data` argument in the function
#'  call.
#' @export
setClass("dpm",
         contains = "lavaan",
         slots = c("call_info" = "list",
                   "call" = "call",
                   "formula" = "Formula",
                   "mod_string" = "character",
                   "wide_data" = "data.frame")
)
