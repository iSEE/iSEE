# Find rows of data within a closed lasso

Identify the rows of a data.frame lying within a closed lasso polygon,
analogous to
[`brushedPoints`](https://rdrr.io/pkg/shiny/man/brushedPoints.html).

## Usage

``` r
lassoPoints(df, lasso)
```

## Arguments

- df:

  A data.frame from which to select rows.

- lasso:

  A list containing data from a lasso.

## Value

A subset of rows from `df` with coordinates lying within `lasso`.

## Details

This function uses `in.out` from the mgcv package to identify points
within a polygon. This involves a boundary crossing algorithm that may
not be robust in the presence of complex polygons with intersecting
edges.

## See also

[`brushedPoints`](https://rdrr.io/pkg/shiny/man/brushedPoints.html)

## Author

Aaron Lun

## Examples

``` r
lasso <- list(coord=rbind(c(0, 0), c(0.5, 0), c(0, 0.5), c(0, 0)),
    closed=TRUE, mapping=list(x="X", y="Y"))
values <- data.frame(X=runif(100), Y=runif(100),
    row.names=sprintf("VALUE_%i", seq_len(100)))
lassoPoints(values, lasso)
#>                    X           Y
#> VALUE_8  0.129155676 0.072476775
#> VALUE_12 0.284979716 0.009333532
#> VALUE_39 0.009614826 0.117817924
#> VALUE_41 0.119281282 0.199199808
#> VALUE_49 0.442380808 0.056098610
#> VALUE_63 0.112004488 0.008002894
#> VALUE_66 0.463874441 0.016999303
#> VALUE_74 0.377463480 0.087603043
#> VALUE_75 0.389771624 0.100078950

# With faceting information:
lasso <- list(coord=rbind(c(0, 0), c(0.5, 0), c(0, 0.5), c(0, 0)),
    panelvar1="A", panelvar2="B", closed=TRUE,
    mapping=list(x="X", y="Y",
    panelvar1="FacetRow", panelvar2="FacetColumn"))
values <- data.frame(X=runif(100), Y=runif(100),
    FacetRow=sample(LETTERS[1:2], 100, replace=TRUE),
    FacetColumn=sample(LETTERS[1:4], 100, replace=TRUE),
    row.names=sprintf("VALUE_%i", seq_len(100)))
lassoPoints(values, lasso)
#>                   X         Y FacetRow FacetColumn
#> VALUE_19 0.07664754 0.2915067        A           B
#> VALUE_70 0.25793844 0.0886977        A           B
```
