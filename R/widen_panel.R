#' @importFrom stats complete.cases reshape

widen_panel_in <- function(data, varying, constants) {

  # id <- panelr::get_id(data)
  # wave <- panelr::get_wave(data)
  # data <-
  #   as.data.frame(data[names(data) %in% c(id, wave, varying, constants)])

  data <- panelr::widen_panel(data, varying = varying)
    # suppressWarnings(reshape(data = data, v.names = varying, timevar = wave,
    #               idvar = id, direction = "wide", sep = "_"))

  complete_obs <- sum(complete.cases(data))
  attr(data, "complete_obs") <- complete_obs

  return(data)

}


#
# lengthen_panel <- function(data, id = NULL, wave = NULL, sep = NULL) {
#
#   data <- suppressWarnings(reshape(data = data, v.names = varying, timevar = wave,
#                                    idvar = id, direction = "long", sep = "_"))
#
#   complete_obs <- length(complete.cases(data))
#   attr(data, "complete_obs") <- complete_obs
#
#   return(data)
#
# }

