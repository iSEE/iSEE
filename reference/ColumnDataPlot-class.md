# The ColumnDataPlot panel

The ColumnDataPlot is a panel class for creating a
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
where the y-axis represents a variable from the `colData` of a
SummarizedExperiment object. It provides slots and methods for
specifying which variable to use on the y-axis (and, optionally, also
the x-axis), as well as a method to create the data.frame in preparation
for plotting.

## Slot overview

The following slots control the column data information that is used:

- `YAxis`, a string specifying the column of the `colData` to show on
  the y-axis. If `NA`, defaults to the first valid field (see
  `?"`[`.refineParameters,ColumnDotPlot-method`](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)`"`).

- `XAxis`, string specifying what should be plotting on the x-axis. This
  can be any one of `"None"`, `"Column data"` or `"Column selection"`.
  Defaults to `"None"`.

- `XAxisColumnData`, string specifying the column of the `colData` to
  show on the x-axis. If `NA`, defaults to the first valid field.

In addition, this class inherits all slots from its parent
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md),
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Constructor

`ColumnDataPlot(...)` creates an instance of a ColumnDataPlot class,
where any slot and its value can be passed to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a ColumnDataPlot
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  returns `x` after replacing any `NA` value in `YAxis` or
  `XAxisColumnData` with the name of the first valid `colData` variable.
  This will also call the equivalent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method for further refinements to `x`. If no valid column metadata
  variables are available, `NULL` is returned instead.

For defining the interface:

- [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  returns a list of interface elements for manipulating all slots
  described above.

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)`(x)`
  will return the specified default color for this panel class.

- [`.allowableXAxisChoices`](https://isee.github.io/iSEE/reference/metadata-plot-generics.md)`(x, se)`
  returns a character vector specifying the acceptable variables in
  `colData(se)` that can be used as choices for the x-axis. This
  consists of all variables with atomic values.

- [`.allowableYAxisChoices`](https://isee.github.io/iSEE/reference/metadata-plot-generics.md)`(x, se)`
  returns a character vector specifying the acceptable variables in
  `colData(se)` that can be used as choices for the y-axis. This
  consists of all variables with atomic values.

For monitoring reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers for all slots described above and in the parent
  classes. This will also call the equivalent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method.

For controlling selections:

- [`.multiSelectionInvalidated`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `TRUE` if the x-axis uses multiple column selections, such
  that the point coordinates may change upon updates to upstream
  selections in transmitting panels. Otherwise, it dispatches to the
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method.

For defining the panel name:

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
  will return `"Column data plot"`.

For creating the plot:

- [`.generateDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x, envir)`
  will create a data.frame of column metadata variables in `envir`. It
  will return the commands required to do so as well as a list of
  labels.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing a panel-specific tour.

## Subclass expectations

Subclasses do not have to provide any methods, as this is a concrete
class.

## See also

[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md),
for the immediate parent class.

## Author

Aaron Lun

## Examples

``` r
#################
# For end-users #
#################

x <- ColumnDataPlot()
x[["XAxis"]]
#> [1] "None"
x[["XAxis"]] <- "Column data"

##################
# For developers #
##################

library(scater)
#> Loading required package: scuttle
#> Loading required package: ggplot2
sce <- mockSCE()
library(scrapper)
#> 
#> Attaching package: ‘scrapper’
#> The following objects are masked from ‘package:scater’:
#> 
#>     aggregateAcrossCells, normalizeCounts
#> The following objects are masked from ‘package:scuttle’:
#> 
#>     aggregateAcrossCells, normalizeCounts
sce <- normalizeRnaCounts.se(sce)

old_cd <- colData(sce)
colData(sce) <- NULL

# Spits out a NULL and a warning if there is nothing to plot.
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Warning: no valid y-axis 'colData' fields for 'ColumnDataPlot'
#> NULL

# Replaces the default with something sensible.
colData(sce) <- old_cd
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class ColumnDataPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     BrushData: 
#>     ColorBy: None
#>     ColorByColumnData: Mutation_Status
#>     ColorByDefaultColor: black
#>     ColorByFeatureDynamicSource: FALSE
#>     ColorByFeatureName: Gene_0001
#>     ColorByFeatureNameAssay: logcounts
#>     ColorByFeatureSource: ---
#>     ColorBySampleDynamicSource: FALSE
#>     ColorBySampleName: Cell_001
#>     ColorBySampleNameColor: red
#>     ColorBySampleSource: ---
#>     ColumnSelectionDynamicSource: FALSE
#>     ColumnSelectionRestrict: FALSE
#>     ColumnSelectionSource: ---
#>     ContourAdd: FALSE
#>     ContourColor: blue
#>     CustomLabels: FALSE
#>     CustomLabelsText: Cell_001
#>     DataBoxOpen: FALSE
#>     Downsample: FALSE
#>     DownsampleResolution: 200
#>     FacetColumnBy: None
#>     FacetColumnByColData: Mutation_Status
#>     FacetRowBy: None
#>     FacetRowByColData: Mutation_Status
#>     FixAspectRatio: FALSE
#>     FontSize: 1
#>     HoverInfo: TRUE
#>     LabelCenters: FALSE
#>     LabelCentersBy: Mutation_Status
#>     LabelCentersColor: black
#>     LegendPointSize: 1
#>     LegendPosition: Bottom
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     PointAlpha: 1
#>     PointSize: 1
#>     SelectionAlpha: 0.1
#>     SelectionBoxOpen: FALSE
#>     SelectionHistory: 
#>     ShapeBy: None
#>     ShapeByColumnData: Mutation_Status
#>     SizeBy: None
#>     SizeByColumnData: sizeFactor
#>     TooltipColumnData: 
#>     VersionInfo: list of length 1
#>     ViolinAdd: TRUE
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Color
#>     XAxis: Column data
#>     XAxisColumnData: Mutation_Status
#>     YAxis: Mutation_Status
#>     ZoomData: 
```
