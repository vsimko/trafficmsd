# Contributing

## Development setup

1. Install a current version of R.
2. Install the package development dependencies with `install.packages(c("devtools", "testthat", "roxygen2", "covr"))`.
3. Clone the repository and open `trafficmsd.Rproj` (or set the repository as the working directory).

## Before opening a pull request

Run the test suite:

```r
devtools::test()
```

Run package checks:

```r
devtools::check()
```

If you change roxygen comments, regenerate and commit the documentation:

```r
devtools::document()
```

Keep changes focused, add regression tests for bug fixes, and update the README or manual pages when user-visible behavior changes. Pull requests should explain the motivation, summarize behavior changes, and include the validation performed.

GitHub Actions runs package checks and unit tests for pull requests targeting the default branch. A pull request should have passing required checks before it is merged.
