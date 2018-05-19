tv_cov_eqs <- function(var, start, end, endogs_lags, vbywave, endogs,
                       exogsreg, creg, dv, y.lag,
                       mod_frame, min_wave, max_wave) {

  s_mod_frame <- mod_frame
  mod_frame <- data.frame(dv = NA, iv = NA)

  for (var in endogs) {
    this_mod_frame <- tv_cov_eq(var = var, start = start, end = end,
                         endogs_lags = endogs_lags,
                         vbywave = vbywave, endogs = endogs,
                         exogsreg = exogsreg, creg = creg, dv = dv,
                         y.lag = y.lag,
                         min_wave = min_wave, max_wave = max_wave)
    # Add finished df to master df
    mod_frame <- rbind(mod_frame, this_mod_frame)
  }

  # Drop the NA placeholder row plus any other missings
  mod_frame <- mod_frame[complete.cases(mod_frame),]
  mod_frame <- as.data.frame(lapply(mod_frame, as.character),
                             stringsAsFactors = FALSE)

  # Preserve original
  o_mod_frame <- mod_frame

  # Iterate through the DVs and make equations
  covs <- NULL
  for (a_dv in unique(mod_frame$dv)) {
    # Subset to this DV only
    this_frame <- mod_frame[mod_frame$dv == a_dv, ]

    # Remove covariance with self
    this_frame <- this_frame[this_frame$dv != this_frame$iv,]

    # Drop duplicates
    this_frame <- unique(this_frame)

    # Take out the reverse rels from the master frame
    drops <- which(mod_frame$dv %in% this_frame$iv & mod_frame$iv == a_dv)
    mod_frame <- mod_frame[seq_len(nrow(mod_frame)) %nin% drops,]
    # Take out the reverse rels from the master frame
    # drops <- which(mod_frame$dv %in% s_mod_frame$iv & mod_frame$iv == a_dv)
    # mod_frame <- mod_frame[seq_len(nrow(mod_frame)) %nin% drops,]
    # Take out the reverse rels from the master frame
    drops <- which(paste(a_dv, "~~", this_frame$iv) %in%
                     paste(s_mod_frame$iv, "~~",  s_mod_frame$dv))
    this_frame <- this_frame[seq_len(nrow(this_frame)) %nin% drops,]
    # Take out the reverse rels from the master frame
    drops <- which(paste(this_frame$iv, "~~", a_dv) %in%
                     paste(s_mod_frame$iv, "~~",  s_mod_frame$dv))
    this_frame <- this_frame[seq_len(nrow(this_frame)) %nin% drops,]


    # Pull out the IVs, sort them
    ivs <- sort(this_frame$iv)

    # Create the equation
    if (length(ivs) > 0) {
      eq <- paste(
        a_dv, "~~",
        paste(ivs, collapse = " + ")
      )
    } else {eq <- NULL}

    # Add to master vector
    covs <- c(covs, eq)

  }

  attr(covs, "mod_frame") <- o_mod_frame
  return(covs)

}

tv_cov_eq <- function(var, start, end, endogs_lags, vbywave, endogs,
                      exogsreg, creg, dv, y.lag, min_wave, max_wave) {

  mod_frame <- data.frame(dv = NA, iv = NA)
  ch <- function(x, ...) {as.character(x, ...)}
  # if (any(duplicated(endogs))) {
  #   dupes <- which(duplicated(endogs))
  #   for (dupe in endogs[dupes]) {
  #     lag_indices <- which(names(endogs_lags) == dupe)
  #
  #     drop_lag_index <- lag_indices[
  #       which(endogs_lags[lag_indices] != min(endogs_lags[lag_indices]))
  #     ]
  #
  #     endogs_lags <- endogs_lags[seq_len(length(endogs_lags)) %nin% drop_lag_index]
  #
  #   }
  # }

  # Within each variable, need to iterate through each wave
  for (w in (start - max(endogs_lags[[var]])):(end - min(endogs_lags[[var]]))) {

    if (w > end) {next}

    # Create beginning of equation, including prior wave of its DV
    if (w > start - max(endogs_lags[[var]])) { # Only if there *is* a prior wave

      reg <- c(vbywave[[ch(w - 1)]][endogs[[var]]])

    } else {

      reg <- NULL

    }

    # Creating new wave variable which we'll add to in the while loop
    w2 <- w - 1
    # Add each lag prior to current wave, stop before trying to add wave 0
    while (w2 > start - max(endogs_lags[[var]])) {

      reg <- c(reg, vbywave[[ch(w2 - 1)]][endogs[[var]]])
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

        w2 <- (end - min(endogs_lags[[var2]]))

        if (w2 > end) {w2 <- end}
        while (w2 >= start - max(endogs_lags[[var2]])) {

          reg <- c(reg, vbywave[[ch(w2)]][var2])
          w2 <- w2 - 1

        }

      }

    }

    # Add the only wave of DV not predicted in main_eqs
    for (wp in min_wave:(start - 1)) {

      if (start > min_wave) {

        reg <- c(reg, vbywave[[ch(wp)]][dv])

      }

    }

    # Filter out redundant terms
    reg <- reg[!duplicated(reg)]

    all_ivs <- c(reg, exogsreg, creg)
    all_ivs <- all_ivs[!duplicated(all_ivs)]

    all_dv <- rep(vbywave[[ch(w)]][endogs[[var]]], times = length(all_ivs))

    this_mod_frame <- as.data.frame(cbind(iv = all_ivs, dv = all_dv))
    mod_frame <- rbind(mod_frame, this_mod_frame)

    # fin_reg <- paste(
    #   vbywave[[w]][endogs[var]], "~~",
    #   paste(c(reg, exogsreg, creg), collapse = " + ")
    # )

    # Because these equations get so long, I'm adding a newline character
    # fin_reg <- paste(fin_reg, "\n")
    # endogs_covs <- c(endogs_covs, fin_reg)

  }

  return(mod_frame)

}
