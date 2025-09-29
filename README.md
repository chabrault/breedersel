
<!-- README.md is generated from README.Rmd. Please edit that file -->

# breedersel

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of breedersel is to provide a user-friendly interface to do
sample selection from a dataset. This package contains one function
which runs a Shiny App.

## Installation

You can install the development version of breedersel from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("chabrault/breedersel")
# or
devtools::install_github("chabrault/breedersel")
```

## Input dataset

You need a file with a column identifying the genotype and several
numerical traits in other columns. File format includes “csv”, “tsv”,
“xlsx” (with tab selection). It is preferable to have genotype-adjusted
values instead of have multiple replicated values for each genotype.

## Launch the ShinyApp

You can launch the application by running:

``` r
library(breedersel)
breedersel::run_app()
```

## Analysis steps

1.  Load input dataset. Modify or delete the columns in the import
    panel, check the type of columns (numeric or character). Validate
    which column corresponds to the genotype.

2.  View the dataset (optional)

3.  Filter the list of genotype by the value of the columns (optional)
    Select the columns to filter on. Move the slider or select the
    categories for character columns. You can track the number of rows
    left in your dataset and add back check genotypes. Once you’re done
    with the filtering, validate the table.

4.  Apply a multi-trait selection index (MGIDI) Fill the selection index
    table:

- Select the trait (double click and select a trait from the list).
- Select min/max/opti for the direction of selection (for example, you
  may want to maximize yield - so select “max” for yield trait), “opti”
  corresponds to an optimum value.
- If you have selected “opti”, indicate the optimal value.
- Indicate a numeric relative weight to apply for all the traits
  (optional, assumed an equal weight for all the traits if not filled).

Once the selection index table is filled, select the intensity of
selection (% of genotypes retained), and click on the “Analyze” button.
The selection index will be applied on the filtered dataset if this step
was not skipped.

5.  Custom graphics Drag and drop the columns into the different
    elements to build a custom plot. Select the type of plot (depends on
    the input), modify the legend, label, color palette, plot theme, and
    output the figure.
