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
#>                    X          Y
#> VALUE_1   0.09840762 0.31913564
#> VALUE_18  0.26061129 0.19330017
#> VALUE_37  0.05110693 0.17549008
#> VALUE_51  0.32752527 0.08856087
#> VALUE_53  0.33588159 0.16012563
#> VALUE_54  0.10722201 0.19520387
#> VALUE_57  0.24380122 0.23567961
#> VALUE_94  0.05336088 0.02877703
#> VALUE_100 0.09970638 0.06994667

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
#>                  X          Y FacetRow FacetColumn
#> VALUE_11 0.4350417 0.01447103        A           B
```
