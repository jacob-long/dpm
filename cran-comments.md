## Test environments
* Local macOS install, R 4.5.1
* win-builder, R-release

## R CMD check results

* Local `R CMD check --as-cran --no-manual`: 0 errors | 0 warnings | 2 notes
* win-builder: 0 errors | 0 warnings | 1 note

## Resubmission

This is a resubmission of a previously archived package, not a new package.

The win-builder note reports CRAN's historical archive metadata:

> Package was archived on CRAN
>
> X-CRAN-Comment: Archived on 2026-01-12 as requires archived package
> 'bmabasket'.

That was the reason recorded for the previous archive. The current version no
longer depends on `bmabasket`, directly or recursively, through its current
`Depends`/`Imports`.

The two local notes are due to the local check environment being unable to
verify the current time and to perform online CRAN incoming checks.
