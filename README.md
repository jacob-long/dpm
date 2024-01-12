
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dpm

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/dpm)](https://cran.r-project.org/package=dpm)
[![GitHub
tag](https://img.shields.io/github/tag/jacob-long/dpm.svg?label=Github)](https://github.com/jacob-long/dpm)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/dpm)](https://cran.r-project.org/package=dpm)
[![Build
Status](https://github.com/jacob-long/dpm/workflows/R-CMD-check/badge.svg)](https://github.com/jacob-long/interactions/actions)
[![codecov](https://codecov.io/gh/jacob-long/dpm/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jacob-long/dpm)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![MIT
License](https://img.shields.io/badge/license-MIT-blue.svg?style=flat)](https://opensource.org/license/mit/)

This R package implements the dynamic panel data modeling framework
described by Allison, Williams, and Moral-Benito (2017). This approach
allows fitting models with fixed effects that do not assume strict
exogeneity of predictors. That means you can simultaneously get the
robustness to confounding offered by fixed effects models and account
for reciprocal causation between the predictors and the outcome
variable. The estimating approach from Allison et al. provides better
finite sample performance in terms of both bias and efficiency than
other popular methods (e.g., the Arellano-Bond estimator).

These models are fit using structural equation models, using maximum
likelihood estimation and offering the missing data handling and
flexibility afforded by SEM. This package will reshape your data,
specify the model properly, and fit it with `lavaan`.

If a result doesn’t seem right, it would be a good idea to
cross-reference it with `xtdpdml` for Stata. Go to
<https://www3.nd.edu/~rwilliam/dynamic/> to learn about `xtdpdml` and
the underlying method. You may also be interested in the article by Paul
Allison, Richard Williams, and Enrique Moral-Benito in **Socius**,
accessible
[here](http://journals.sagepub.com/doi/full/10.1177/2378023117710578).

# Installation

`dpm` will soon be on CRAN. In the meantime, you can get it from Github.

``` r
install.packages("devtools")
devtools::install_github("jacob-long/dpm")
```

# Usage

This package assumes your data are in *long* format, with each row
representing a single observation of a single participant. Contrast this
with *wide* format in which each row contains all observations of a
single participant. For help on converting data from wide to long
format, check out [the
tutorial](https://panelr.jacob-long.com/articles/reshape.html) that
accompanies the `panelr` package.

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
formula consist of two parts, separated with a bar (`|`) like so:
`y ~ x | z` where z is a time invariant predictor, like ethnicity.

One of the innovations of the method, however, is the notion of
pre-determined, or sequentially exogenous, predictors. To specify a
model with a pre-determined variable, put the variable within a `pre`
function, `y ~ pre(x1) + x2 | z`. This tells the function that `x1` is
pre-determined while `x2` is strictly exogenous by assumption. You could
have multiple pre-determined predictors as well (e.g.,
`y ~ pre(x1) + pre(x2) | z`).

You may also fit models with lagged predictors. Simply apply the lag
function to the lagged predictors in the formula:
`y ~ pre(lag(x1)) + lag(x2) | z`. To specify more than 1 lag, just
provide it as an argument. For instance,
`y ~ pre(lag(x1, 2)) + lag(x2) | z` will use 2 lags of the `x1`
variable.

## *Socius* article example

This will replicate the analysis of the wages data in the *Socius*
article that describes these models.

Note that to get matching standard errors, set
`information = "observed"` to override `lavaan`’s default,
`information = "expected"`.

``` r
fit <- dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages,
           error.inv = TRUE, information = "observed")
summary(fit)
```

    MODEL INFO:
    Dependent variable: wks 
    Total observations: 595 
    Complete observations: 595 
    Time periods: 2 - 7 

    MODEL FIT:
    𝛘²(76) = 138.476
    RMSEA = 0.037, 90% CI [0.027, 0.047]
    p(RMSEA < .05) = 0.986
    SRMR = 0.025 

    |                   |   Est. |  S.E. | z val. |     p |
    |:------------------|-------:|------:|-------:|------:|
    | union (t - 1)     | -1.206 | 0.522 | -2.309 | 0.021 |
    | lwage (t - 1)     |  0.588 | 0.488 |  1.204 | 0.229 |
    | ed                | -0.107 | 0.056 | -1.893 | 0.058 |
    | wks (t - 1)       |  0.188 | 0.020 |  9.586 | 0.000 |

    Model converged after 600 iterations

Any arguments supplied other than those that are documented within the
`dpm` function are passed on to `sem` from the `lavaan` package.

## Model specification options

The following arguments allow you to make changes to the default model
specification:

-   `y.lag`: By default the lag 1 value of the DV is included as a
    predictor (this is why they are dynamic models). You may choose a
    different value or multiple values instead, including 0 (no lagged
    DV at all).
-   `fixed.effects`: By default, the model is specified as a fixed
    effects model. If you set this to FALSE, you get a random effects
    specification instead.
-   `error.inv`: This constrains error variances to be equal in each
    wave. It is FALSE by default.
-   `const.inv`: This constrains the constants to be equal in each wave.
    It is FALSE by default, but if TRUE it eliminates cross-sectional
    dependence.
-   `y.free`: This allows the regression coefficient of the lagged DV to
    vary across time. It is FALSE by default and you can either set it
    to TRUE or to the specific lag number(s).
-   `x.free`: This allows the regression coefficients for the predictors
    to vary across time. It is FALSE by default and you can either set
    it to TRUE to set all predictors’ coefficients free over time or
    else pass a vector of strings of the predictors whose coefficients
    should be set free over time.
-   `alpha.free`: If TRUE, relaxes the constraint that the fixed effects
    are equal across time. Default is FALSE to be consistent with how
    fixed effects models normally work.
-   `partial.pre`: If TRUE (FALSE by default), predetermined lagged
    predictors will also be allowed to correlate with the
    contemporaneous error term as [suggested by Paul
    Allison](https://statisticalhorizons.com/getting-the-lags-right-a-new-solution/)
    for scenarios when it’s not clear whether you have chosen the right
    lag structure.

## Summary options

You have most of the options available to you via `lavaan`’s summary
method.

You can choose to omit any of: the *z* statistics (`zstat = FALSE`), the
standard errors (`se = FALSE`), or the p values (`pvalue = FALSE`). You
may also add confidence intervals (`ci = TRUE`) at any specified level
(`ci.level = .95`). If you used bootstrapping for uncertainty intervals,
you can also specify the method (`boot.ci.type = "perc"`).

The number of digits to print can be set via `digits` or with the option
`dpm-digits`. You may also standardize coefficients via `lavaan`’s
method using `standardize = TRUE`.

## Other features

### Lavaan syntax only

If you just want the `lavaan` model specification and don’t want this
package to fit the model for you, you can set `print.only = TRUE`. To
reduce the amount of output, I’m condensing `wages` to 4 waves here.

``` r
dpm(wks ~ pre(lag(union)) + lag(lwage) | ed, data = wages[wages$t < 5,],
    print.only = TRUE)
```

    ## Main regressions

    wks_2 ~ en1 * union_1 + ex1 * lwage_1 + c1 * ed + p1 * wks_1
    wks_3 ~ en1 * union_2 + ex1 * lwage_2 + c1 * ed + p1 * wks_2
    wks_4 ~ en1 * union_3 + ex1 * lwage_3 + c1 * ed + p1 * wks_3

    ## Alpha latent variable (random intercept)

    alpha =~ 1 * wks_2 + 1 * wks_3 + 1 * wks_4

    ## Alpha free to covary with observed variables (fixed effects)

    alpha ~~  union_1 +  union_2 +  union_3 +  lwage_1 +  lwage_2 +  lwage_3 +  wks_1

    ## Correlating DV errors with future values of predetermined predictors

    wks_2 ~~ union_3

    ## Predetermined predictors covariances

    union_1 ~~ ed + lwage_1 + lwage_2 + lwage_3 + wks_1
    union_2 ~~ ed + lwage_1 + lwage_2 + lwage_3 + union_1 + wks_1
    union_3 ~~ ed + lwage_1 + lwage_2 + lwage_3 + union_1 + union_2 + wks_1

    ## Exogenous (time varying and invariant) predictors covariances

    lwage_1 ~~ ed + wks_1
    lwage_2 ~~ ed + lwage_1 + wks_1
    lwage_3 ~~ ed + lwage_1 + lwage_2 + wks_1

    ed ~~ wks_1

    ## DV error variance free to vary across waves

    wks_2 ~~ wks_2
    wks_3 ~~ wks_3
    wks_4 ~~ wks_4

    ## Let DV variance vary across waves

    wks_2 ~ 1
    wks_3 ~ 1
    wks_4 ~ 1

### Extract components

Alternately, you can extract the `lavaan` model syntax and
wide-formatted data from the fitted model object to do your own fitting
and tweaking.

``` r
get_wide_data(fit)
get_syntax(fit)
```

The model is a special type of `lavaan` object. This means most methods
implemented for `lavaan` objects will work on these. You can also
convert the fitted model into a typical `lavaan` object:

``` r
as(fit, "lavaan")
```

### Get full `lavaan` summary

While you could convert the model to `lavaan` model and apply any of
`lavaan`’s functions to it (and you should!), as a convenience you can
use `lav_summary()` to get `lavaan`’s summary of the model.

### Missing data

Take advantage of `lavaan`’s missing data handling by using the
`missing = "fiml"` argument as well as any other arguments accepted by
`lavaan::sem()`.

# Feature comparison and roadmap

-   CFI/TLI fit measures are much different than Stata’s and
    consistently more optimistic. For now, they are not printed with the
    summary because they are probably misleading.
-   ~~You cannot use multiple lags of the same predictor (e.g.,
    `y ~ x + lag(x)`).~~ (Fixed in `1.0.0`)
-   The function does not yet support input data that is already in wide
    format. (Not planning to fix)
-   ~~You cannot apply arbitrary functions to variables in the formula
    like you can with regression models. For instance, a specification
    like `y ~ scale(x)` will cause an error.~~ (Works as of `1.1.0`)

Feature parity with `xtdpdml` (Stata) is a goal. Here’s how we are doing
in terms of matching relevant `xtdpdml` options:

-   [x] `alphafree` (as `alpha.free`)
-   [x] `xfree` (as `x.free`)
-   [x] `xfree(varlist)` (as `x.free`)
-   [x] `yfree` (added as `y.free` argument in `1.0.0`)
-   [ ] `yfree(numlist)`
-   [x] `re` (added via `fixed.effects` argument in `1.0.0`)
-   [x] `errorinv` (as `error.inv`)
-   [x] `nocsd`/`constinv` (as `const.inv`)
-   [x] `ylag(numlist)` (added as `y.lag` argument in `1.0.0`; option to
    specify as 0 — no lagged DV — added in `1.1.0`)
-   [ ] `std` (but `standardize` argument of `summary` may suffice)
-   [x] `dryrun` (as `print.only`)

Many and perhaps more SEM fitting options are implemented by virtue of
accepting any `lavaan::sem()` argument.

## Roadmap

-   [ ] Get proper CFI/TLI statistics — this is a `lavaan` problem.
-   [x] Allow full use of formula syntax, e.g. `y ~ scale(x)` (fixed in
    `1.1.0`)
-   [x] Add `broom` methods (`tidy`, `glance`) (added `tidy` in `1.1.0`)
-   [ ] Create a `predict` method and perhaps some ability to plot
    predictions
-   [x] Add `x.free` option to allow the coefficients of all predictors
    to vary across periods. This will make the `summary` output a pain,
    so it will take some time to implement. (added in `1.1.1`)

# References

Allison, P. (2022, October 24). Getting the lags right – a new solution.
*Statistical Horizons*.
<https://statisticalhorizons.com/getting-the-lags-right-a-new-solution/>

Allison, P. D., Williams, R., & Moral-Benito, E. (2017). Maximum
likelihood for cross-lagged panel models with fixed effects. *Socius*,
*3*, 1–17. <https://doi.org/10.1177/2378023117710578>

Leszczensky, L., & Wolbring, T. (2022). How to deal with reverse
causality using panel data? Recommendations for researchers based on a
simulation study. *Sociological Methods & Research*, *51*(2), 837–865.
<https://doi.org/10.1177/0049124119882473>

Moral-Benito, E., Allison, P., & Williams, R. (2019). Dynamic panel data
modelling using maximum likelihood: An alternative to Arellano-Bond.
*Applied Economics*, *51*, 2221–2232.
<https://doi.org/10.1080/00036846.2018.1540854>

Williams, R., Allison, P. D., & Moral-Benito, E. (2018). Linear dynamic
panel-data estimation using maximum likelihood and structural equation
modeling. *The Stata Journal*, *18*, 293–326.
<https://doi.org/10.1177/1536867X1801800201>
