context("Match Socius paper")

data("WageData", package = "panelr")
wages <- panel_data(WageData, id = id, wave = t)

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           err.inv = TRUE, information = "observed")}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 76)
  expect_equivalent(s$fitmeasures["chisq"], expected = 138.48, tolerance = 0.01)
})


context("Basic model with all data types")

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)},
                error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 71)
  expect_equivalent(s$fitmeasures["chisq"], expected = 110.23, tolerance = 0.01)
})

context("Basic model with all data types x2")

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + pre(lag(ms)) + lag(lwage) + lag(ind) |
               ed + blk, data = wages)}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 124)
  expect_equivalent(s$fitmeasures["chisq"], expected = 184.47, tolerance = 0.01)
})

context("Basic model with all data types (no lags)")

fit <- tryCatch({dpm(wks ~ pre(union) + lwage | ed, data = wages)},
                error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 66)
  expect_equivalent(s$fitmeasures["chisq"], expected = 95.40, tolerance = 0.01)
})


# No lagged DV ------------------------------------------------------------


context("No lagged DV")

fit <- tryCatch({dpm(wks ~ pre(union) + lwage | ed, data = wages, y.lag = 0)},
                error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 87)
  expect_equivalent(s$fitmeasures["chisq"], expected = 223.10, tolerance = 0.01)
})

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) + occ | ed + blk,
                     data = wages, y.lag = 0)}, error = function(x) NULL)

test_that("Model runs", {

  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 101)
  expect_equivalent(s$fitmeasures["chisq"], expected = 219.53, tolerance = 0.01)
})

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) + occ | ed + blk,
                     data = wages, y.lag = 0, fixed.effects = FALSE)},
                error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 119)
  expect_equivalent(s$fitmeasures["chisq"], expected = 237.72, tolerance = 0.01)
})

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) + occ, data = wages,
                     y.lag = 0)},
                error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 91)
  expect_equivalent(s$fitmeasures["chisq"], expected = 201.97, tolerance = 0.01)
})


# Multiple y lags ---------------------------------------------------------

context("Multiple y lags")

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           y.lag = c(1,2))}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 51)
  expect_equivalent(s$fitmeasures["chisq"], expected = 79.17, tolerance = 0.01)
})

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           y.lag = c(1,3))}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 34)
  expect_equivalent(s$fitmeasures["chisq"], expected = 35.54, tolerance = 0.01)
})

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           y.lag = c(2,4))}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 19)
  expect_equivalent(s$fitmeasures["chisq"], expected = 31.72, tolerance = 0.01)
})


# Multiple predictor lags -------------------------------------------------

context("Multiple lags of predictors")

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + union + lag(lwage) | ed,
                     data = wages)}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 75)
  expect_equivalent(s$fitmeasures["chisq"], expected = 114.42, tolerance = 0.01)
})

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + union + lag(lwage) + lwage | ed,
           data = wages)}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 79)
  expect_equivalent(s$fitmeasures["chisq"], expected = 116.13, tolerance = 0.01)
})

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + pre(union) + lag(lwage) + lwage |
                       ed, data = wages)}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 74)
  expect_equivalent(s$fitmeasures["chisq"], expected = 111.79, tolerance = 0.01)
})


# Constant invariance --------------------------------------------------------

context("Constant invariance")

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + union + lag(lwage) | ed,
                     data = wages, const.inv = TRUE)}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 80)
  expect_equivalent(s$fitmeasures["chisq"], expected = 126.75, tolerance = 0.01)
})


# Alpha free --------------------------------------------------------------

context("Alpha free")

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           alpha.free = TRUE)}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 66)
  expect_equivalent(s$fitmeasures["chisq"], expected = 108.73, tolerance = 0.01)
})

# Y free --------------------------------------------------------------

context("Y free")

fit <- tryCatch({dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
                     y.free = TRUE)}, error = function(x) NULL)

test_that("Model runs", {
  expect_s4_class(fit, "dpm")
})

test_that("Model is accurate", {
  s <- summary(fit)
  expect_equivalent(as.integer(s$fitmeasures["df"]), expected = 66)
  expect_equivalent(s$fitmeasures["chisq"], expected = 108.73, tolerance = 0.01)
})
