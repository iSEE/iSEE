# Caching utilities

Utility functions to be used in a
[`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)
method, usually to identify names of elements of the
SummarizedExperiment for later use in
[`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)
to populate the user interface.

## Usage

``` r
.findAtomicFields(x)

.whichGroupable(x, max_levels = Inf)

.whichNumeric(x)

.isAssayNumeric(se, i)
```

## Arguments

- x:

  A data.frame or DataFrame, most typically the `rowData` or `colData`.

- max_levels:

  Integer scalar specifying the maximum number unique values for `x` to
  be categorical.

- se:

  The SummarizedExperiment object.

- i:

  An integer scalar or string specifying the assay of interest in `se`.

## Value

For `.findAtomicFields`, a character vector of names of columns in `x`
containing atomic R types.

For `.whichNumeric`, an integer vector containing the indices of the
numeric columns.

For `.whichGroupable`, an integer vector containing the indices of the
categorical columns.

For `.isAssayNumeric`, a logical scalar indicating whether the specified
assay as numeric.

## Details

`.findAtomicFields` is necessary as many of the widgets used by
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) (e.g.,
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html),
`datatable`) do not know how to handle more complex types being stored
as columns. Similarly, `.whichNumeric` and `.whichGroupable` can be used
to specify options for visualization modes that only make sense for
continuous or discrete variables respectively (e.g., sizing, faceting).

## Author

Aaron Lun, Kevin Rue-Albrecht, Charlotte Soneson

## Examples

``` r
x <- DataFrame(
    A = rnorm(10),
    B = sample(letters, 10),
    DataFrame = I(DataFrame(
        C = rnorm(10),
        D = sample(letters, 10)
    ))
)

.findAtomicFields(x)
#> [1] "A" "B"
.whichGroupable(x)
#>         B DataFrame 
#>         2         3 
.whichNumeric(x)
#> A 
#> 1 
```
