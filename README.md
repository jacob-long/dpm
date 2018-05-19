
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dpm

[![GitHub
tag](https://img.shields.io/github/tag/jacob-long/dpm.svg?label=Github)](https://github.com/jacob-long/dpm)
[![Travis-CI Build
Status](https://travis-ci.org/jacob-long/dpm.svg?branch=master)](https://travis-ci.org/jacob-long/dpm)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/jacob-long/dpm?branch=master&svg=true)](https://ci.appveyor.com/project/jacob-long/dpm)
[![Coverage
Status](https://img.shields.io/codecov/c/github/jacob-long/dpm/master.svg)](https://codecov.io/github/jacob-long/dpm?branch=master)

This R package implements the cross-lagged panel model with fixed
effects described by Allison, Williams, and Moral-Benito (2017). This
package will reshape your data, specify the model properly, and fit it
with `lavaan`.

Note: This is BETA software. Expect bugs and missing functionality. It
is best to cross-reference all results with xtdpdml for Stata. Go to
<https://www3.nd.edu/~rwilliam/dynamic/> to learn about xtdpdml and the
underlying method. You may also be interested in the article by Paul
Allison, Richard Williams, and Enrique Moral-Benito in **Socius**,
accessible
[here](http://journals.sagepub.com/doi/full/10.1177/2378023117710578)

# Installation

You will need to the `devtools` package installed to install this
package from Github as well as its companion package,
[`panelr`](https://github.com/jacob-long/panelr).

``` r
install.packages("devtools")
devtools::install_github("jacob-long/panelr")
devtools::install_github("jacob-long/dpm")
```

Also note this package’s other dependencies: `lavaan`, `stringr`,
`rlang`, and `crayon`, in addition to the dependencies of `panelr` and
the aforementioned packages.

# Usage

This package assumes your data are in *long* format, with each row
representing a single observation of a single participant. Contrast this
with *wide* format in which each row contains all observations of a
single participant. Better compatibility with wide data will be
implemented in a future release, but in the meantime you may use the
`long_panel` function implemented in `panelr` to reshape to long format
in a relatively pain-free way.

First we load the package and the `WageData` from `panelr`.

``` r
library(dpm)
data("WageData", package = "panelr")
```

This next line of code converts the data to class `panel_data`, which is
a class specific to the [`panelr`](https://github.com/jacob-long/panelr)
that helps to simplify the treatment of the long-form panel data. You
don’t have to do this, but it saves you from providing `id` and `wave`
arguments to the model fitting function each time you use it.

``` r
wages <- panel_data(WageData, id = id, wave = t)
```

## Basic formula syntax

The formula syntax used in this package is meant to be as similar to a
typical regression model as possible.

The most basic model can be specified like any other: `y ~ x`, where `y`
is the dependent variable and `x` is a time-varying predictor. If you
would like to include time-invariant predictors, you will make the
formula consist of two parts, separated with a bar (`|`) like so: `y ~ x
| z` where z is a time invariant predictor, like ethnicity.

One of the innovations of the method, however, is the notion of
predetermined, or sequentially exogenous, predictors. To specify a model
with a predetermined variable, put the variable within a `pre` function,
`y ~ pre(x1) + x2 | z`. This tells the function that `x1` is
predetermined while `x2` is strictly exogenous by assumption. You could
have multiple predetermined predictors as well (e.g., `y ~ pre(x1) +
pre(x2) | z`).

As implied by the “cross-lagged” terminology, you may also fit models
with lagged predictors. Simply apply the lag function to the lagged
predictors in the formula: `y ~ pre(lag(x1)) + lag(x2) | z`. To specify
more than 1 lag, just provide it as an argument. For instance, `y ~
pre(lag(x1, 2)) + lag(x2) | z` will use 2 lags of the `x1` variable.

## *Socius* article example

This will replicate the analysis of the wages data in the *Socius*
article that describes these models.

Note that to get matching standard errors, set `information =
"observed"` to override `lavaan`’s default, `information = "expected"`.

``` r
fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           err.inv = TRUE, information = "observed")
summary(fit)
```

    # MODEL INFO:
    # Dependent variable: wks 
    # Total observations: 595 
    # Complete observations: 595 
    # Time periods: 2 - 7 
    # 
    # MODEL FIT:
    # 𝛘²(76) = 138.476
    # RMSEA = 0.037, 90% CI [0.027, 0.047]
    # p(RMSEA < .05) = 0.986
    # SRMR = 0.025 
    # 
    #                 Est.  S.E. z val.     p    
    # union (t - 1) -1.206 0.522 -2.309 0.021   *
    # lwage (t - 1)  0.588 0.488  1.204 0.229    
    # ed            -0.107 0.056 -1.893 0.058   .
    # wks (t - 1)    0.188 0.020  9.586 0.000 ***
    # 
    # Model converged after 613 iterations

Any arguments supplied other than those that are documented within the
`clfe` function are passed on to `sem` from `lavaan`.

## Other features

### Lavaan syntax only

If you just want the `lavaan` model specification and don’t want this
package to fit the model for you, you can set `print.only = TRUE`. To
reduce the amount of output, I’m condensing `wages` to 4 waves here.

``` r
dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages[wages$t < 5,],
    print.only = TRUE)
```

    # ## Main regressions
    # 
    # wks_2 ~ en1 * union_1 + ex1 * lwage_1 + c1 * ed + p1 * wks_1
    # wks_3 ~ en1 * union_2 + ex1 * lwage_2 + c1 * ed + p1 * wks_2
    # wks_4 ~ en1 * union_3 + ex1 * lwage_3 + c1 * ed + p1 * wks_3
    # 
    # ## Alpha latent variable (random intercept)
    # 
    # alpha =~ 1 * wks_2 + 1 * wks_3 + 1 * wks_4
    # 
    # ## Alpha free to covary with observed variables (fixed effects)
    # 
    # alpha ~~  union_1 +  union_2 +  union_3 +  lwage_1 +  lwage_2 +  lwage_3 +  wks_1
    # 
    # ## Correlating DV errors with future values of predetermined predictors
    # 
    # wks_2 ~~ union_3
    # 
    # ## Predetermined predictors covariances
    # 
    # union_1 ~~ ed + lwage_1 + lwage_2 + lwage_3 + wks_1
    # union_2 ~~ ed + lwage_1 + lwage_2 + lwage_3 + union_1 + wks_1
    # union_3 ~~ ed + lwage_1 + lwage_2 + lwage_3 + union_1 + union_2 + wks_1
    # 
    # ## Exogenous (time varying and invariant) predictors covariances
    # 
    # lwage_1 ~~ ed + wks_1
    # lwage_2 ~~ ed + lwage_1 + wks_1
    # lwage_3 ~~ ed + lwage_1 + lwage_2 + wks_1
    # 
    # ed ~~ wks_1
    # 
    # ## DV error variance free to vary across waves
    # 
    # wks_2 ~~ wks_2
    # wks_3 ~~ wks_3
    # wks_4 ~~ wks_4
    # 
    # ## Let DV variance vary across waves
    # 
    # wks_2 ~ 1
    # wks_3 ~ 1
    # wks_4 ~ 1

### Extract components

Alternately, you can extract the `lavaan` model syntax and
wide-formatted data from the fitted model object to do your own
    fitting.

``` r
head(fit@wide_data)
```

    #    ed id union_1 wks_1 lwage_1 union_2 wks_2 lwage_2 union_3 wks_3 lwage_3
    # 1   9  1       0    32 5.56068       0    43 5.72031       0    40 5.99645
    # 8  11  2       0    34 6.16331       0    27 6.21461       1    33 6.26340
    # 15 12  3       1    50 5.65249       1    51 6.43615       1    50 6.54822
    # 22 10  4       0    52 6.15698       0    46 6.23832       0    46 6.30079
    # 29 16  5       1    50 6.43775       1    46 6.62007       1    40 6.63332
    # 36 12  6       0    44 6.90575       0    47 6.90575       0    47 6.90776
    #    union_4 wks_4 lwage_4 union_5 wks_5 lwage_5 union_6 wks_6 lwage_6
    # 1        0    39 5.99645       0    42 6.06146       0    35 6.17379
    # 8        0    30 6.54391       0    30 6.69703       0    37 6.79122
    # 15       1    52 6.60259       1    52 6.69580       1    52 6.77878
    # 22       0    49 6.35957       0    44 6.46925       0    52 6.56244
    # 29       0    50 6.98286       0    47 7.04752       0    47 7.31322
    # 36       0    47 7.00307       0    44 7.06902       0    45 7.52023
    #    union_7 wks_7 lwage_7
    # 1        0    32 6.24417
    # 8        0    30 6.81564
    # 15       1    46 6.86066
    # 22       0    46 6.62141
    # 29       0    49 7.29574
    # 36       0    47 7.33889

The model is a special type of `lavaan` object. This means most methods
implemented for `lavaan` objects will work on these. You can also
convert the fitted model into a typical `lavaan` object:

``` r
as(fit, "lavaan")
```

### Get full `lavaan` summary

While you could convert the model to `lavaan` model and apply any of
`lavaan`’s functions to it (and you should\!), as a convenience you can
use `lav_summary` to get `lavaan`’s summary of the model.

### Missing data

Take advantage of `lavaan`’s missing data handling by using the `missing
= "fiml"` argument, which is passed to `sem`.

# Missing features/problems

  - CFI/TLI fit measures are much different than Stata’s and
    consistently more optimistic. For now, they are not printed with the
    summary because they are probably misleading.
  - ~~You cannot use multiple lags of the same predictor (e.g., `y ~ x +
    lag(x)`).~~ (Fixed in `1.0.0`)
  - The function does not yet support input data that is already in wide
    format.
  - You cannot apply arbitrary functions to variables in the formula
    like you can with regression models. For instance, a specification
    like `y ~ scale(x)` will cause an error.

The following `xtdpdml` (Stata) options are not implemented:

  - xfree
  - ~~yfree~~ (added as `y.free` argument in `1.0.0`)
  - ~~re~~ (added as `fixed.effects` argument in `1.0.0`)
  - ~~ylag~~ (added as `y.lag` argument in `1.0.0`)
  - std (but `standardize` argument of `summary` may suffice)

## Roadmap

  - Get proper CFI/TLI statistics
  - Allow full use of formula syntax, e.g. `y ~ scale(x)`
  - Create a `predict` method and perhaps some ability to plot
    predictions
  - Add `broom` methods (`tidy`, `glance`)

# Reference

Allison, P. D., Williams, R., & Moral-Benito, E. (2017). Maximum
likelihood for cross-lagged panel models with fixed effects. *Socius*,
*3*, 1-17.
