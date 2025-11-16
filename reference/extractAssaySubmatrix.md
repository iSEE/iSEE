# Extract assay submatrix

Extract an assay submatrix based on the multiple row/column selection
and any custom specifications from
[`.createCustomDimnamesModalObservers`](https://isee.github.io/iSEE/reference/createCustomDimnamesModalObservers.md).

## Usage

``` r
.extractAssaySubmatrix(
  x,
  se,
  envir,
  use_custom_row_slot,
  custom_row_text_slot,
  cap_row_selection_slot = NULL,
  as_matrix = TRUE
)
```

## Arguments

- x:

  A [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
  instance that uses the row selection modal.

- se:

  The current SummarizedExperiment object.

- envir:

  The evaluation environment. This assumes that
  [`.processMultiSelections`](https://isee.github.io/iSEE/reference/processMultiSelections.md)
  has already been run.

- use_custom_row_slot:

  String specifying the name of the slot indicating whether to use
  custom rows.

- custom_row_text_slot:

  String specifying the name of the slot holding the custom row names.
  This is expected to be of the same format as described in
  `?`[`.createCustomDimnamesModalObservers`](https://isee.github.io/iSEE/reference/createCustomDimnamesModalObservers.md).

- cap_row_selection_slot:

  String specifying the name of the slot containing the cap on the
  number of selected rows.

- as_matrix:

  Logical scalar indicating whether to coerce the submatrix into an
  ordinary matrix. If `FALSE`, the existing matrix representation is
  preserved.

## Value

A character vector of commands to set up the assay submatrix. The
submatrix itself is generated within `envir` as the `plot.data`
variable.

## Details

This is designed to extract a matrix of assay values for a subset of
rows/columns of interest, most typically for a
[ComplexHeatmapPlot](https://isee.github.io/iSEE/reference/ComplexHeatmapPlot-class.md).
It assumes that the class of `x` contains a slot indicating whether
custom rows should be used, plus a slot to hold the selected custom row
names (usually from a modal, see
[`.createCustomDimnamesModalObservers`](https://isee.github.io/iSEE/reference/createCustomDimnamesModalObservers.md)).

If a multiple row selection is present in `envir` and custom rows are
*not* to be used, that selection is used to define the rows of the
submatrix. All columns are returned in the submatrix unless a multiple
column selection is present in `envir` and the `SelectEffect` in `x` is
“Restrict”, in which case only the selected columns are returned.

## Author

Kevin Rue-Albrecht
