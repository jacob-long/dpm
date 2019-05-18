context("Edge cases")

data("WageData", package = "panelr")
wages <- panel_data(WageData, id = id, wave = t)

test_that("Model fits with non-panel_data data", {
  expect_s4_class(dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = WageData,
                      id = id, wave = t), "dpm")
})

test_that("dpm messages about err.inv argument", {
  expect_message(dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
                     err.inv = TRUE))
})

test_that("dpm stops with invalid x.free", {
  expect_error(dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
                     x.free = "fem"))
})

test_that("dpm handles factor variables", {
  wages$blkf <- factor(wages$blk)
  wages$unnf <- factor(wages$union)
  expect_s4_class(dpm(wks ~ lwage + unnf + lag(unnf) | blkf, data = wages),
                  "dpm")
})

test_that("dpm can implement interactions", {
  wages$blkf <- factor(wages$blk)
  wages$unnf <- factor(wages$union)
  expect_s4_class(suppressWarnings(dpm(wks ~ lwage * union, data = wages)),
                  "dpm")
  expect_s4_class(suppressWarnings(
    dpm(wks ~ lwage | blk | lwage * blk, data = wages)), "dpm")
  expect_s4_class(suppressWarnings(
    dpm(wks ~ pre(lwage) | blk | pre(lwage) * blk, data = wages)),
                  "dpm")
  expect_s4_class(suppressWarnings(
    dpm(wks ~ lag(lwage) | blk | lag(lwage) * blk, data = wages)),
                  "dpm")
})

test_that("dpm can handle pre-determined by pre-determined interactions", {
  expect_s4_class(suppressWarnings(dpm(wks ~ pre(lwage) * pre(union),
                                       data = wages)), "dpm")
})

context("Utilities and extractors")

test_that("dpm prints lavaan syntax", {
  expect_output(dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
                     print.only = TRUE))
})

test_that("dpm summaries print confidence intervals", {
  fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)
  s <- summary(fit, ci = TRUE)
  expect_output(print(s))
  expect_true("2.5%" %in% names(s$coefficients))
})

test_that("dpm show method works", {
  fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)
  expect_output(show(fit))
})

test_that("dpm coef method works", {
  fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)
  expect_is(coef(fit), "numeric")
})

test_that("dpm update method works", {
  fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages)
  expect_s4_class(update(fit, . ~ . - lag(lwage), data = wages), "dpm")
})
