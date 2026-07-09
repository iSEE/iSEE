# The RowDataPlot panel

The RowDataPlot is a panel class for creating a
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
where the y-axis represents a variable from the `rowData` of a
SummarizedExperiment object. It provides slots and methods for
specifying which variable to use on the y-axis (and, optionally, also
the x-axis), as well as a method to create the data.frame in preparation
for plotting.

## Slot overview

The following slots control the variables to be shown:

- `YAxis`, a string specifying the row of the `rowData` to show on the
  y-axis. If `NA`, defaults to the first valid field (see
  `?"`[`.refineParameters,RowDotPlot-method`](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)`"`).

- `XAxis`, string specifying what should be plotting on the x-axis. This
  can be any one of `"None"`, `"Row data"` and `"Row selection"`.
  Defaults to `"None"`.

- `XAxisRowData`, string specifying the row of the `rowData` to show on
  the x-axis. If `NA`, defaults to the first valid field.

In addition, this class inherits all slots from its parent
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md),
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Constructor

`RowDataPlot(...)` creates an instance of a RowDataPlot class, where any
slot and its value can be passed to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a RowDataPlot
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  returns `x` after replacing any `NA` value in `YAxis` or
  `XAxisRowData` with the name of the first valid `rowData` variable.
  This will also call the equivalent
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
  method for further refinements to `x`. If no valid row metadata
  variables are available, `NULL` is returned instead.

For defining the interface:

- [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  returns a list of interface elements for manipulating all slots
  described above.

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)`(x)`
  will return the specified default color for this panel class.

- [`.allowableXAxisChoices`](https://isee.github.io/iSEE/reference/metadata-plot-generics.md)`(x, se)`
  returns a character vector specifying the acceptable variables in
  `rowData(se)` that can be used as choices for the x-axis. This
  consists of all variables with atomic values.

- [`.allowableYAxisChoices`](https://isee.github.io/iSEE/reference/metadata-plot-generics.md)`(x, se)`
  returns a character vector specifying the acceptable variables in
  `rowData(se)` that can be used as choices for the y-axis. This
  consists of all variables with atomic values.

For monitoring reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers for all slots described above and in the parent
  classes. This will also call the equivalent
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
  method.

For controlling selections:

- [`.multiSelectionInvalidated`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `TRUE` if the x-axis uses multiple row selections, such that
  the point coordinates may change upon updates to upstream selections
  in transmitting panels. Otherwise, it dispatches to the
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
  method.

For defining the panel name:

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
  will return `"Row data plot"`.

For creating the plot:

- [`.generateDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x, envir)`
  will create a data.frame of row metadata variables in `envir`. It will
  return the commands required to do so as well as a list of labels.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing a panel-specific tour.

## Subclass expectations

Subclasses do not have to provide any methods, as this is a concrete
class.

## See also

[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md),
for the immediate parent class.

## Author

Aaron Lun

## Examples

``` r
#################
# For end-users #
#################

x <- RowDataPlot()
x[["XAxis"]]
#> [1] "None"
x[["XAxis"]] <- "Row data"

##################
# For developers #
##################

library(scater)
sce <- mockSCE()
library(scrapper)
sce <- normalizeRnaCounts.se(sce)

# Spits out a NULL and a warning if is nothing to plot.
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Warning: no valid y-axis 'rowData' fields for 'RowDataPlot'
#> NULL

# Replaces the default with something sensible.
rowData(sce)$Stuff <- runif(nrow(sce))
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class RowDataPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     BrushData: 
#>     ColorBy: None
#>     ColorByDefaultColor: black
#>     ColorByFeatureDynamicSource: FALSE
#>     ColorByFeatureName: Gene_0001
#>     ColorByFeatureNameColor: red
#>     ColorByFeatureSource: ---
#>     ColorByRowData: Stuff
#>     ColorBySampleDynamicSource: FALSE
#>     ColorBySampleName: Cell_001
#>     ColorBySampleNameAssay: logcounts
#>     ColorBySampleSource: ---
#>     ContourAdd: FALSE
#>     ContourColor: blue
#>     CustomLabels: FALSE
#>     CustomLabelsText: Gene_0001
#>     DataBoxOpen: FALSE
#>     Downsample: FALSE
#>     DownsampleResolution: 200
#>     FacetColumnBy: None
#>     FacetColumnByRowData: NA
#>     FacetRowBy: None
#>     FacetRowByRowData: NA
#>     FixAspectRatio: FALSE
#>     FontSize: 1
#>     HoverInfo: TRUE
#>     LabelCenters: FALSE
#>     LabelCentersBy: NA
#>     LabelCentersColor: black
#>     LegendPointSize: 1
#>     LegendPosition: Bottom
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     PointAlpha: 1
#>     PointSize: 1
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionRestrict: FALSE
#>     RowSelectionSource: ---
#>     SelectionAlpha: 0.1
#>     SelectionBoxOpen: FALSE
#>     SelectionHistory: 
#>     ShapeBy: None
#>     ShapeByRowData: NA
#>     SizeBy: None
#>     SizeByRowData: Stuff
#>     TooltipRowData: 
#>     VersionInfo: list of length 1
#>     ViolinAdd: TRUE
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Color
#>     XAxis: Row data
#>     XAxisRowData: Stuff
#>     YAxis: Stuff
#>     ZoomData: 
```
