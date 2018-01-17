
## clfe 0.3.0

* Added a `NEWS.md` file to track changes to the package.

The [`panelr`](https://github.com/jacob-long/panelr) package was added as a
dependency, which has some downstream effects. 

* The `panel_data` function now belongs to `panelr`. 
* The `panel_data` object type now inherits from `tibble`, which has pluses
and minuses but I believe more upside than downside.
* The `WageData` example data is now part of the `panelr` package.

