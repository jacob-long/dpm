.xtdpdml_exogenous_vars <- function(object) {
  lines <- strsplit(object@mod_string, "\n", fixed = TRUE)[[1]]
  main_idx <- which(grepl("^## Main regressions", lines, perl = TRUE))[1]
  if (length(main_idx) == 0L) {
    return(character())
  }

  exog <- character()
  info <- object@call_info
  dv_endog <- paste0(info$dv, "_", seq.int(info$start, info$end))

  for (i in seq.int(main_idx + 1L, length(lines))) {
    line <- lines[i]
    if (grepl("^##\\s", line, perl = TRUE)) {
      break
    }
    if (!grepl("~", line, fixed = TRUE)) {
      next
    }

    rhs <- strsplit(line, "~", fixed = TRUE)[[1]][2]
    terms <- strsplit(rhs, "\\+", perl = TRUE)[[1]]
    vars <- vapply(
      terms,
      function(term) {
        term <- trimws(term)
        term <- sub(".*\\*", "", term, perl = TRUE)
        trimws(term)
      },
      character(1)
    )

    vars <- vars[nchar(vars) > 0L & vars != "1"]
    exog <- c(exog, vars)
  }

  exog <- unique(exog)
  setdiff(exog, dv_endog)
}

.xtdpdml_baseline_syntax <- function(object, exog) {
  vars <- lavaan::lavNames(object)
  exog <- intersect(exog, vars)
  var_lines <- sprintf("%s ~~ %s", vars, vars)

  if (length(exog) > 1L) {
    pairs <- utils::combn(exog, 2)
    cov_lines <- apply(pairs, 2, function(pair) {
      sprintf("%s ~~ %s", pair[1], pair[2])
    })
  } else {
    cov_lines <- character()
  }

  paste(c(var_lines, cov_lines), collapse = "\n")
}

.xtdpdml_compute_fitindices <- function(object) {
  exog <- .xtdpdml_exogenous_vars(object)
  if (!length(exog)) {
    return(NULL)
  }

  baseline_model <- .xtdpdml_baseline_syntax(object, exog)

  sample_cov <- object@SampleStats@cov[[1]]
  sample_mean <- object@SampleStats@mean[[1]]
  nobs <- object@SampleStats@ntotal
  vars <- lavaan::lavNames(object)

  dimnames(sample_cov) <- list(vars, vars)
  names(sample_mean) <- vars

  baseline_fit <- tryCatch(
    lavaan::sem(
      model = baseline_model,
      sample.cov = sample_cov,
      sample.mean = sample_mean,
      sample.nobs = nobs,
      meanstructure = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(baseline_fit)) {
    return(NULL)
  }

  base_fm <- lavaan::fitMeasures(baseline_fit, c("chisq", "df"), baseline = TRUE)
  if (is.na(base_fm["chisq"]) || base_fm["df"] <= 0) {
    return(NULL)
  }

  mod_fm <- lavaan::fitMeasures(as(object, "lavaan"), c("chisq", "df"), baseline = TRUE)

  base_chisq <- unname(base_fm["chisq"])
  base_df <- unname(base_fm["df"])
  mod_chisq <- unname(mod_fm["chisq"])
  mod_df <- unname(mod_fm["df"])

  denom_cfi <- base_chisq - base_df
  if (denom_cfi <= 0) {
    cfi <- NA_real_
  } else {
    cfi <- 1 - max((mod_chisq - mod_df) / denom_cfi, 0)
  }

  denom_tli <- (base_chisq / base_df) - 1
  if (denom_tli <= 0) {
    tli <- NA_real_
  } else {
    tli <- ((base_chisq / base_df) - (mod_chisq / mod_df)) / denom_tli
  }

  c(
    cfi = unname(cfi),
    tli = unname(tli),
    baseline.chisq = base_chisq,
    baseline.df = base_df
  )
}

fitMeasures.dpm <- function(object, fit.measures = "all",
                            baseline.model = NULL, h1.model = NULL,
                            fm.args = list(
                              standard.test = "default",
                              scaled.test = "default",
                              rmsea.ci.level = 0.9,
                              rmsea.close.h0 = 0.05,
                              rmsea.notclose.h0 = 0.08,
                              robust = TRUE,
                              cat.check.pd = TRUE
                            ),
                            output = "vector", ...) {

  fm <- lavaan::fitMeasures(
    as(object, "lavaan"),
    fit.measures = fit.measures,
    baseline.model = baseline.model,
    h1.model = h1.model,
    fm.args = fm.args,
    output = output,
    ...
  )

  if (!identical(output, "vector")) {
    return(fm)
  }

  xtfm <- .xtdpdml_compute_fitindices(object)
  if (is.null(xtfm)) {
    return(fm)
  }

  if ("cfi" %in% names(fm)) {
    fm["cfi.lavaan"] <- fm["cfi"]
    fm["cfi"] <- xtfm["cfi"]
  }

  if ("tli" %in% names(fm)) {
    fm["tli.lavaan"] <- fm["tli"]
    fm["tli"] <- xtfm["tli"]
  }

  fm["baseline.chisq.xtdpdml"] <- xtfm["baseline.chisq"]
  fm["baseline.df.xtdpdml"] <- xtfm["baseline.df"]

  fm
}

fitmeasures.dpm <- fitMeasures.dpm

setMethod("fitMeasures", signature(object = "dpm"),
          function(object, ...) fitMeasures.dpm(object, ...))

setMethod("fitmeasures", signature(object = "dpm"),
          function(object, ...) fitMeasures.dpm(object, ...))
