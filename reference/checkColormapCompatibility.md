# Check compatibility between ExperimentColorMap and SummarizedExperiment objects

This function compares a pair of
[ExperimentColorMap](https://isee.github.io/iSEE/reference/ExperimentColorMap-class.md)
and
[SingleCellExperiment](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)
objects, and examines whether all of the `assays`, `colData`, and
`rowData` defined in the ExperimentColorMap object exist in the
SingleCellExperiment object.

## Usage

``` r
checkColormapCompatibility(ecm, se)
```

## Arguments

- ecm:

  An
  [ExperimentColorMap](https://isee.github.io/iSEE/reference/ExperimentColorMap-class.md).

- se:

  A
  [SingleCellExperiment](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html).

## Value

A character vector of incompatibility error messages, if any.

## Author

Kevin Rue-Albrecht

## Examples

``` r

# Example colormaps ----

count_colors <- function(n){
  c("black","brown","red","orange","yellow")
}

qc_color_fun <- function(n){
  qc_colors <- c("forestgreen", "firebrick1")
  names(qc_colors) <- c("Y", "N")
  return(qc_colors)
}

ecm <- ExperimentColorMap(
    assays = list(
        tophat_counts = count_colors
    ),
    colData = list(
        passes_qc_checks_s = qc_color_fun
    )
)

# Example SingleCellExperiment ----

library(scRNAseq)
sce <- ReprocessedAllenData(assays="tophat_counts")

# Test for compatibility ----

checkColormapCompatibility(ecm, sce)
#> NULL
```
