# Process faceting choices

Generate ggplot instructions to facet a plot by row and/or column

## Usage

``` r
.addFacets(x)
```

## Arguments

- x:

  A single-row DataFrame that contains all the input settings for the
  current panel.

## Value

A string containing a command to define the row and column faceting
covariates.

## Author

Kevin Rue-Albrecht.

## Examples

``` r
x <- ReducedDimensionPlot(
    FacetRowBy = "Column data", FacetRowByColData="Covariate_1", 
    FacetColumnBy = "Column data", FacetColumnByColData="Covariate_2") 
.addFacets(x)
#> [1] "facet_grid(FacetRow ~ FacetColumn)"
```
