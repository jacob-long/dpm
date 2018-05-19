## dpm 1.0.0.9000 --- major release

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
expliclty defined by this package should simply treat `dpm` objects as if they
were `lavaan` objects.

The `summary` method now has more options and is more similar to `lavaan`'s
summary in that regard. Of course, the summary output is much cleaner and 
more succinct.

The following arguments have been added to `dpm`:

* `y.lag`: Equivalent to `xtdpdml`'s `ylag`. Specify which lags of the DV to 
use.
* `y.free`: Equivalent to `xtdpdml`'s `yfree`. Allow stability coefficients for
the lagged DV to vary over time.
* `fixed.effects`: Comparable to `xtdpdml`'s `re`. Use fixed effects (`TRUE`)
or random effects (`FALSE`) specification.
* `alpha.free`: Equivalent to `xtdpdml`'s `alphafree`. Allow the fixed effects
to vary over time.

## clfe 0.3.0

* Added a `NEWS.md` file to track changes to the package.

The [`panelr`](https://github.com/jacob-long/panelr) package was added as a
dependency, which has some downstream effects. 

* The `panel_data` function now belongs to `panelr`. 
* The `panel_data` object type now inherits from `tibble`, which has pluses
and minuses but I believe more upside than downside.
* The `WageData` example data is now part of the `panelr` package.

