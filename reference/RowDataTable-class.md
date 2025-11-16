# The RowDataTable panel

The RowDataTable is a panel class for creating a
[ColumnTable](https://isee.github.io/iSEE/reference/ColumnTable-class.md)
where the value of the table is defined as the `rowData` of the
SummarizedExperiment. It provides functionality to extract the `rowData`
to coerce it into an appropriate data.frame in preparation for
rendering.

## Slot overview

This class inherits all slots from its parent
[ColumnTable](https://isee.github.io/iSEE/reference/ColumnTable-class.md)
and [Table](https://isee.github.io/iSEE/reference/Table-class.md)
classes.

## Constructor

`RowDataTable(...)` creates an instance of a RowDataTable class, where
any slot and its value can be passed to `...` as a named argument.

Note that `ColSearch` should be a character vector of length equal to
the total number of columns in the `rowData`, though only the entries
for the atomic fields will actually be used.

## Supported methods

In the following code snippets, `x` is an instance of a RowDataTable
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x)`
  adds a `"RowDataTable"` entry containing `valid.rowData.names`, a
  character vector of names of atomic columns of the `rowData`. This
  will also call the equivalent
  [ColumnTable](https://isee.github.io/iSEE/reference/ColumnTable-class.md)
  method.

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  adjusts `ColSearch` to a character vector of length equal to the
  number of atomic fields in the `rowData`. This will also call the
  equivalent
  [ColumnTable](https://isee.github.io/iSEE/reference/ColumnTable-class.md)
  method for further refinements to `x`.

For defining the interface:

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
  will return `"Row data table"`.

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)`(x)`
  will return the specified default color for this panel class.

For creating the output:

- [`.generateTable`](https://isee.github.io/iSEE/reference/table-generics.md)`(x, envir)`
  will modify `envir` to contain the relevant data.frame for display,
  while returning a character vector of commands required to produce
  that data.frame. Each row of the data.frame should correspond to a row
  of the SummarizedExperiment.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing the steps of a panel-specific tour.

Unless explicitly specialized above, all methods from the parent class
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) are also
available.

## Author

Aaron Lun

## Examples

``` r
#################
# For end-users #
#################

x <- RowDataTable()
x[["Selected"]]
#> [1] NA
x[["Selected"]] <- "SOME_ROW_NAME"

##################
# For developers #
##################

library(scater)
sce <- mockSCE()

# Sets the search columns appropriately.
sce <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce)
#> Panel object of class RowDataTable
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     DataBoxOpen: FALSE
#>     HiddenColumns: 
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionSource: ---
#>     Search: 
#>     SearchColumns: 
#>     Selected: Gene_0001
#>     SelectionBoxOpen: FALSE
#>     VersionInfo: list of length 1
```
