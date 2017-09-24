widen_panel <- function(data, id = NULL, wave = NULL, varying, constants) {

  if (class(data)[1] == "panel_data" & is.null(id) & is.null(wave)) {
    id <- attr(data, "id")
    wave <- attr(data, "wave")
  }

  data <- data[,names(data) %in% c(id, wave, varying, constants)]

  data <- suppressWarnings(reshape(data = data, v.names = varying, timevar = wave,
                  idvar = id, direction = "wide", sep = "_"))

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

