
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{breedersel}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{breedersel}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
breedersel::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-06-17 13:26:56 CDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading breedersel
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
#> ── R CMD check results ────────────────────────────── breedersel 0.0.0.9000 ────
#> Duration: 57.3s
#> 
#> ❯ checking code files for non-ASCII characters ... WARNING
#>   Found the following file with non-ASCII characters:
#>     R/mod_MGIDI.R
#>   Portable packages must use only ASCII characters in their R code and
#>   NAMESPACE directives, except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#>   Function 'tools::showNonASCIIfile' can help in finding non-ASCII
#>   characters in files.
#> 
#> ❯ checking top-level files ... NOTE
#>   File
#>     LICENSE
#>   is not mentioned in the DESCRIPTION file.
#>   Non-standard files/directories found at top level:
#>     'csv' 'dev' 'docs' 'translations'
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: 'ggthemes'
#>     All declared Imports should be used.
#>   Unexported object imported by a ':::' call: 'metan:::plot.mgidi'
#>     See the note in ?`:::` about the use of this operator.
#> 
#> 0 errors ✔ | 1 warning ✖ | 2 notes ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> breedersel Coverage: 56.00%
#> R/fct_helpers.R: 0.00%
#> R/run_app.R: 0.00%
#> R/mod_data_filtering.R: 29.03%
#> R/mod_MGIDI.R: 43.13%
#> R/mod_import_table.R: 67.44%
#> R/golem_utils_server.R: 77.78%
#> R/app_server.R: 86.36%
#> R/golem_utils_ui.R: 87.94%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
```
