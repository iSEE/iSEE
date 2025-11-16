# The ReducedDimensionPlot panel

The ReducedDimensionPlot is a panel class for creating a
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
where the coordinates of each column/sample are taken from the
[`reducedDims`](https://rdrr.io/pkg/SingleCellExperiment/man/reducedDims.html)
of a
[SingleCellExperiment](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)
object. It provides slots and methods to specify which dimensionality
reduction result to use and to create the data.frame with the
coordinates of the specified results for plotting.

## ReducedDimensionPlot slot overview

The following slots control the dimensionality reduction result that is
used:

- `Type`, a string specifying the name of the dimensionality reduction
  result. If `NA`, defaults to the first entry of
  [`reducedDims`](https://rdrr.io/pkg/SingleCellExperiment/man/reducedDims.html).

- `XAxis`, integer scalar specifying the dimension to plot on the
  x-axis. Defaults to 1.

- `YAxis`, integer scalar specifying the dimension to plot on the
  y-axis. Defaults to 2.

In addition, this class inherits all slots from its parent
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md),
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Constructor

`ReducedDimensionPlot(...)` creates an instance of a
ReducedDimensionPlot class, where any slot and its value can be passed
to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a
ReducedDimensionPlot class. Refer to the documentation for each method
for more details on the remaining arguments.

For setting up data values:

- [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x)`
  adds a `"ReducedDimensionPlot"` entry containing
  `valid.reducedDim.names`, a character vector of names of valid
  dimensionality reduction results (i.e., at least one dimension). This
  will also call the equivalent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method.

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  replaces `NA` values in `RedDimType` with the first valid
  dimensionality reduction result name in `se`. This will also call the
  equivalent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method for further refinements to `x`. If no dimensionality reduction
  results are available, `NULL` is returned instead.

For defining the interface:

- [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  returns a list of interface elements for manipulating all slots
  described above.

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)`(x)`
  will return the specified default color for this panel class.

For monitoring reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers for all slots described above and in the parent
  classes. This will also call the equivalent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method.

For defining the panel name:

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
  will return `"Reduced dimension plot"`.

For creating the plot:

- [`.generateDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x, envir)`
  will create a data.frame of reduced dimension coordinates in `envir`.
  It will return the commands required to do so as well as a list of
  labels.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing a panel-specific tour.

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

x <- ReducedDimensionPlot()
x[["Type"]]
#> [1] NA
x[["Type"]] <- "TSNE"

##################
# For developers #
##################

library(scater)
sce <- mockSCE()
sce <- logNormCounts(sce)

# Spits out a NULL and a warning if no reducedDims are available.
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Warning: no 'reducedDims' with non-zero dimensions for 'ReducedDimensionPlot'
#> NULL

# Replaces the default with something sensible.
sce <- runPCA(sce)
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class ReducedDimensionPlot
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
#>     Type: PCA
#>     VersionInfo: list of length 1
#>     ViolinAdd: TRUE
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Color
#>     XAxis: 1
#>     YAxis: 2
#>     ZoomData: 
```
