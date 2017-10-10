<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/jacob-long/clfe.svg?branch=master)](https://travis-ci.org/jacob-long/clfe) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jacob-long/clfe?branch=master&svg=true)](https://ci.appveyor.com/project/jacob-long/clfe) [![Coverage Status](https://img.shields.io/coveralls/jacob-long/clfe.svg)](https://coveralls.io/r/jacob-long/clfe?branch=master)

This R package implements the cross-lagged panel model with fixed effects described by Allison, Williams, and Moral-Benito (2017). It is effectively a convenience wrapper to the `lavaan` package. This package will reshape your data, specify the model properly, and fit it with `lavaan`.

Note: This is ALPHA software. Expect bugs and missing functionality. Cross-reference all results with xtdpdml for Stata. Go to <https://www3.nd.edu/~rwilliam/dynamic/> to learn about xtdpdml and the underlying method. You may also be interested in the article by Paul Allison, Richard Williams, and Enrique Moral-Benito in **Socius**, accessible [here](http://journals.sagepub.com/doi/full/10.1177/2378023117710578)

Installation
============

You will need to the `devtools` package installed to install this package from Github.

``` r
install.packages("devtools")
devtools::install_github("jacob-long/clfe")
```

Also note this package's dependencies: `lavaan`, `stringr`

Usage
=====

This package assumes your data are in *long* format, with each row representing a single observation of a single participant. Contrast this with *wide* format in which each row contains all observations of a single participant. Better compatibility with wide data will be implemented in a future release.

Load the package, use built-in wages data.

``` r
library(clfe)
data("WageData", package = "clfe")
```

This next line of code converts the data to class `panel_data`, which is a class specific to this package that helps to simplify the treatment of the long-form panel data. You don't have to do this, but it saves you from providing `id` and `wave` arguments to the model fitting function each time you use it.

``` r
wages <- panel_data(WageData, id = id, wave = t)
```

Basic formula syntax
--------------------

The formula syntax used in this package is meant to be as similar to a typical regression model as possible.

The most basic model can be specified like any other: `y ~ x`, where `y` is the dependent variable and `x` is a time-varying predictor. If you would like to include time-invariant predictors, you will make the formula consist of two parts, separated with a bar (`|`) like so: `y ~ x | z` where z is a time invariant predictor, like ethnicity.

One of the innovations of the method, however, is the notion of predetermined, or sequentially exogenous, predictors. To specify a model with a predetermined variable, put the variable within a `pre` function, `y ~ pre(x1) + x2 | z`. This tells the function that `x1` is predetermined while `x2` is strictly exogenous by assumption. You could have multiple predetermined predictors as well (e.g., `y ~ pre(x1) + pre(x2) | z`).

As implied by the "cross-lagged" terminology, you may also fit models with lagged predictors. Simply apply the lag function to the lagged predictors in the formula: `y ~ pre(lag(x1)) + lag(x2) | z`. To specify more than 1 lag, just provide it as an argument. For instance, `y ~ pre(lag(x1, 2)) + lag(x2) | z` will use 2 lags of the `x1` variable.

*Socius* article example
------------------------

This will replicate the analysis of the wages data in the *Socius* article that describes these models.

Note that to get matching standard errors, set `information = "observed"` to override `lavaan`'s default, `information = "expected"`.

``` r
fit <- clfe(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           err.inv = TRUE,
           information = "observed")
summary(fit)
```

    # MODEL INFO
    # Dependent variable: wks 
    # Total observations: 
    # Complete observations: 595 
    # Time periods: 2 - 7 
    # 
    # MODEL FIT
    # Chi-squared (76) = 138.476
    # RMSEA = 0.037, 90% CI [0.027, 0.047]
    # p(RMSEA < .05) = 0.986
    # SRMR = 0.027 
    # 
    # NULL
    # 
    # Model converged after 579 iterations

Any arguments supplied other than those that are documented within the `clfe` function are passed on to `sem` from `lavaan`.

Other features
--------------

### Lavaan syntax only

If you just want the `lavaan` model specification and don't want this package to fit the model for you, you can set `print.only = TRUE`.

``` r
clfe(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages, err.inv = TRUE,
     print.only = TRUE)
```

    # ## Main regressions
    # 
    # wks_2 ~ en1*union_1 + ex1*lwage_1 + c1*ed + p*wks_1
    # wks_3 ~ en1*union_2 + ex1*lwage_2 + c1*ed + p*wks_2
    # wks_4 ~ en1*union_3 + ex1*lwage_3 + c1*ed + p*wks_3
    # wks_5 ~ en1*union_4 + ex1*lwage_4 + c1*ed + p*wks_4
    # wks_6 ~ en1*union_5 + ex1*lwage_5 + c1*ed + p*wks_5
    # wks_7 ~ en1*union_6 + ex1*lwage_6 + c1*ed + p*wks_6
    # 
    # ## Alpha latent variable (fixed effects)
    # 
    # alpha =~ 1*wks_2 + 1*wks_3 + 1*wks_4 + 1*wks_5 + 1*wks_6 + 1*wks_7
    # 
    # ## Alpha regression (fixed effects)
    # 
    # alpha ~~ union_1 + union_2 + union_3 + union_4 + union_5 + union_6 + lwage_1 + lwage_2 + lwage_3 + lwage_4 + lwage_5 + lwage_6 + wks_1
    # 
    # ## Correlating DV errors with future values of predetermined predictors
    # 
    # wks_5 ~~ union_6
    # wks_4 ~~ union_5 + union_6
    # wks_3 ~~ union_4 + union_5 + union_6
    # wks_2 ~~ union_3 + union_4 + union_5 + union_6
    # 
    # ## Predetermined predictors covariances
    # 
    # union_1 ~~ lwage_1 + lwage_2 + lwage_3 + lwage_4 + lwage_5 + lwage_6 + ed + wks_1 
    # 
    # union_2 ~~ union_1 + lwage_1 + lwage_2 + lwage_3 + lwage_4 + lwage_5 + lwage_6 + ed + wks_1 
    # 
    # union_3 ~~ union_2 + union_1 + lwage_1 + lwage_2 + lwage_3 + lwage_4 + lwage_5 + lwage_6 + ed + wks_1 
    # 
    # union_4 ~~ union_3 + union_2 + union_1 + lwage_1 + lwage_2 + lwage_3 + lwage_4 + lwage_5 + lwage_6 + ed + wks_1 
    # 
    # union_5 ~~ union_4 + union_3 + union_2 + union_1 + lwage_1 + lwage_2 + lwage_3 + lwage_4 + lwage_5 + lwage_6 + ed + wks_1 
    # 
    # union_6 ~~ union_5 + union_4 + union_3 + union_2 + union_1 + lwage_1 + lwage_2 + lwage_3 + lwage_4 + lwage_5 + lwage_6 + ed + wks_1 
    # 
    # 
    # ## Exogenous (varying and invariant) predictors covariances
    # 
    # lwage_1 ~~ ed + wks_1
    # lwage_2 ~~ lwage_1 + ed + wks_1
    # lwage_3 ~~ lwage_2 + lwage_1 + ed + wks_1
    # lwage_4 ~~ lwage_3 + lwage_2 + lwage_1 + ed + wks_1
    # lwage_5 ~~ lwage_4 + lwage_3 + lwage_2 + lwage_1 + ed + wks_1
    # lwage_6 ~~ lwage_5 + lwage_4 + lwage_3 + lwage_2 + lwage_1 + ed + wks_1
    # 
    # ed ~~ wks_1
    # 
    # ## Holding DV error variance constant for each wave (optional)
    # 
    # wks_2 ~~ v*wks_2
    # wks_3 ~~ v*wks_3
    # wks_4 ~~ v*wks_4
    # wks_5 ~~ v*wks_5
    # wks_6 ~~ v*wks_6
    # wks_7 ~~ v*wks_7

### Extract components

Alternately, you can extract the `lavaan` model syntax and wide-formatted data from the fitted model object to do your own fitting.

``` r
head(fit$wide_data)
```

    #    id ed union_1 wks_1 lwage_1 union_2 wks_2 lwage_2 union_3 wks_3 lwage_3
    # 1   1  9       0    32 5.56068       0    43 5.72031       0    40 5.99645
    # 8   2 11       0    34 6.16331       0    27 6.21461       1    33 6.26340
    # 15  3 12       1    50 5.65249       1    51 6.43615       1    50 6.54822
    # 22  4 10       0    52 6.15698       0    46 6.23832       0    46 6.30079
    # 29  5 16       1    50 6.43775       1    46 6.62007       1    40 6.63332
    # 36  6 12       0    44 6.90575       0    47 6.90575       0    47 6.90776
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

The `lavaan` model specification is stored as `fit$mod_string`. Tip: To print the `mod_string` to your console, don't use the `print` function, use the `cat` function because it will format the line breaks appropriately.

You can also get the fitted `lavaan` model object at `fit$fit`.

### Get full `lavaan` summary

While you could extract the `lavaan` model and apply any of `lavaan`'s functions to it (and you should!), as a convenience you can use `lav_summary` to get `lavaan`'s summary of the model.

### Missing data

Take advantage of `lavaan`'s missing data handling by using the `missing = "fiml"` argument, which is passed to `sem`.

Missing features/problems
=========================

-   CFI/TLI fit measures are much different than Stata's and consistently more optimistic. For now, they are not printed with the summary because they are probably misleading.
-   You cannot use multiple lags of the same predictor (e.g., `y ~ x + lag(x)`).
-   The function does not yet support input data that is already in wide format.
-   You cannot apply arbitrary functions to variables in the formula like you can with regression models. For instance, a specification like `y ~ scale(x)` will cause an error.

The following `xtdpdml` (Stata) options are not implemented:

-   xfree
-   yfree
-   re
-   ylag
-   std

Reference
=========

Allison, P. D., Williams, R., & Moral-Benito, E. (2017). Maximum likelihood for cross-lagged panel models with fixed effects. *Socius*, *3*, 1-17.
