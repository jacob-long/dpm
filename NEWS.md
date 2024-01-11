# dpm 1.2.0 

## Enhancement
* For cases in which the proper lag structure is ambiguous, you 
may now use the `partial.pre` argument to `dpm()` to use the strategy 
proposed by 
[Allison (2022)](https://statisticalhorizons.com/getting-the-lags-right-a-new-solution/).

# dpm 1.1.2

## Bug fixes:
* The ability to do arbitrary variable transformations introduced 1.1.0 has
been made to actually work since `lavaan` does not allow non-syntactic variable
names.

## Feature updates:
* Specifying interactions in model formulae is now supported. To make them
predetermined, just make sure one of the terms has the `pre()` tag surrounding
it. Do *not* put the entire interaction inside `pre()`.
* You may now include sampling weights via the `weights` argument.

# dpm 1.1.1

## Bug fixes:
* Fixed a bug that caused an error when printing summaries for models with 
`y.free = TRUE`.

## Feature updates:
* `dpm()` now has the `x.free` argument, which can allow predictor variables
to have coefficients that vary by wave.
* `dpm` objects now have a `glance()` method for the `broom` package.
* You can now get wave-by-wave coefficient tables when either `x.free` or 
`y.free` are used with `dpm()`.

# dpm 1.1.0

This release contains several important updates and a 1.1.x release is
likely to be the one submitted to CRAN.

## Bug fixes:
* Fixed an error in models with no constants.
* Fixed an error that sometimes caused predetermined variables to be treated
incorrectly. (Vague, I know)
* Summaries now include coefficients for each wave of the lagged DV(s) when
`y.free` is TRUE.

## Feature updates:
* Models without lagged dependent variables are now supported (use `y.lag = 0`)
* Variable transformations in the model formula are now supported
(e.g., `y ~ scale(x)`).
* You can now update `dpm` models with `update`. 
* `dpm` objects now a have a `tidy` method via the `broom` package.

As a side note, there is now a *testing* suite in place to check models
for accuracy/consistency with `xtdpdml`. That doesn't mean there will be no
bugs, but it should help prevent any regressions. 


# dpm 1.0.0 --- major release

This is a major release with several breaking changes compared to the initial
development release.

Most noticeably, the name of the package has been changed from `clfe` to `dpm`.
This was done to better reflect the scope of the package --- the 
cross-lagged fixed effects specification (CFLE) is a special case of the 
larger group of dynamic panel model (DPM) specifications availed to users of the
package.

Accordingly, what was once the `clfe` function is now called `dpm`. 

Internally, the `dpm` class is now an S4 object that contains the `lavaan`
class. This means that any method implemented for `lavaan` objects that isn't
explicitly defined by this package should simply treat `dpm` objects as if they
were `lavaan` objects.

The `summary` method now has more options and is more similar to `lavaan`'s
summary in that regard. Of course, the summary output is much cleaner and 
more succinct.

The following arguments have been added to `dpm()`:

* `y.lag`: Equivalent to `xtdpdml`'s `ylag`. Specify which lags of the DV to 
use.
* `y.free`: Equivalent to `xtdpdml`'s `yfree`. Allow stability coefficients for
the lagged DV to vary over time.
* `fixed.effects`: Comparable to `xtdpdml`'s `re`. Use fixed effects (`TRUE`)
or random effects (`FALSE`) specification.
* `alpha.free`: Equivalent to `xtdpdml`'s `alphafree`. Allow the fixed effects
to vary over time.

# clfe 0.3.0

* Added a `NEWS.md` file to track changes to the package.

The [`panelr`](https://github.com/jacob-long/panelr) package was added as a
dependency, which has some downstream effects. 

* The `panel_data` function now belongs to `panelr`. 
* The `panel_data` object type now inherits from `tibble`, which has pluses
and minuses but I believe more upside than downside.
* The `WageData` example data is now part of the `panelr` package.

