
<!-- README.md is generated from README.Rmd. Please edit that file -->

# breedersel

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{breedersel}` like so:

``` r
devtools::install_github("chabrault/breedersel")
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo chabrault/breedersel@HEAD
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> Warning in untar2(tarfile, files, list, exdir, restore_times): skipping pax
#> global extended headers
#> xfun      (0.52  -> 0.53   ) [CRAN]
#> rmarkdown (2.29  -> 2.30   ) [CRAN]
#> forcats   (1.0.0 -> 1.0.1  ) [CRAN]
#> vroom     (1.6.5 -> 1.6.6  ) [CRAN]
#> rio       (1.2.3 -> 1.2.4  ) [CRAN]
#> waiter    (0.2.5 -> 0.2.5.1) [CRAN]
#> Installing 6 packages: xfun, rmarkdown, forcats, vroom, rio, waiter
#> package 'xfun' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'xfun'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\cbrault\AppData\Local\Programs\R\R-4.5.1\library\00LOCK\xfun\libs\x64\xfun.dll
#> to
#> C:\Users\cbrault\AppData\Local\Programs\R\R-4.5.1\library\xfun\libs\x64\xfun.dll:
#> Permission denied
#> Warning: restored 'xfun'
#> package 'rmarkdown' successfully unpacked and MD5 sums checked
#> package 'forcats' successfully unpacked and MD5 sums checked
#> package 'vroom' successfully unpacked and MD5 sums checked
#> package 'rio' successfully unpacked and MD5 sums checked
#> package 'waiter' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\cbrault\AppData\Local\Temp\Rtmp4qSXjb\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>       ✔  checking for file 'C:\Users\cbrault\AppData\Local\Temp\Rtmp4qSXjb\remotes8df09cd12e6\chabrault-breedersel-22573ff/DESCRIPTION' (466ms)
#>       ─  preparing 'breedersel':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  excluding invalid files
#>    Subdirectory 'R' contains invalid file names:
#>      '_disable_autoload.R'
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>       ─  building 'breedersel_0.0.0.9000.tar.gz'
#>      
#> 
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
#> [1] "2025-09-29 11:49:42 CDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading breedersel
#> ── R CMD check results ────────────────────────────── breedersel 0.0.0.9000 ────
#> Duration: 1m 42.5s
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
#> ❯ checking for hidden files and directories ... NOTE
#>   Found the following hidden files and directories:
#>     .RDataTmp
#>     .RDataTmp1
#>   These were most likely included in error. See section 'Package
#>   structure' in the 'Writing R Extensions' manual.
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
#> 0 errors ✔ | 1 warning ✖ | 3 notes ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> breedersel Coverage: 52.15%
#> R/fct_helpers.R: 0.00%
#> R/run_app.R: 0.00%
#> R/mod_data_filtering.R: 28.05%
#> R/mod_MGIDI.R: 38.13%
#> R/mod_import_table.R: 67.44%
#> R/golem_utils_server.R: 77.78%
#> R/app_server.R: 84.78%
#> R/golem_utils_ui.R: 87.94%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
```
