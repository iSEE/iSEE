# The FeatureAssayPlot panel

The FeatureAssayPlot is a panel class for creating a
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
where the y-axis represents the expression of a feature of interest,
using the `assay` values of the SummarizedExperiment. It provides slots
and methods to specify the feature and what to plot on the x-axis, as
well as a method to actually create a data.frame containing those pieces
of data in preparation for plotting.

## Slot overview

The following slots control the values on the y-axis:

- `YAxisFeatureName`, a string specifying the name of the feature to
  plot on the y-axis. If `NA`, defaults to the first row name of the
  SummarizedExperiment object.

- `Assay`, string specifying the name of the assay to use for obtaining
  expression values. Defaults to `"logcounts"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md),
  falling back to the name of the first valid assay (see
  `?"`[`.cacheCommonInfo,DotPlot-method`](https://isee.github.io/iSEE/reference/DotPlot-class.md)`"`
  for the definition of validity).

- `YAxisFeatureSource`, string specifying the encoded name of the
  transmitting panel to obtain a single selection that replaces
  `YAxisFeatureName`. Defaults to `"---"`, i.e., no transmission is
  performed.

- `YAxisFeatureDynamicSource`, a logical scalar indicating whether `x`
  should dynamically change its selection source for the y-axis.
  Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

The following slots control the values on the x-axis:

- `XAxis`, string specifying what should be plotting on the x-axis. This
  can be any one of `"None"`, `"Feature name"`, `"Column data"` or
  `"Column selection"`. Defaults to `"None"`.

- `XAxisColumnData`, string specifying which column of the `colData`
  should be shown on the x-axis, if `XAxis="Column data"`. Defaults to
  the first valid `colData` field (see
  `?"`[`.refineParameters,ColumnDotPlot-method`](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)`"`
  for details).

- `XAaxisFeatureName`, string specifying the name of the feature to plot
  on the x-axis, if `XAxis="Feature name"`. Defaults to the first row
  name.

- `XAxisFeatureSource`, string specifying the encoded name of the
  transmitting panel to obtain a single selection that replaces
  `XAxisFeatureName`. Defaults to `"---"`, i.e., no transmission is
  performed.

- `XAxisFeatureDynamicSource`, a logical scalar indicating whether `x`
  should dynamically change its selection source for the x-axis.
  Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

In addition, this class inherits all slots from its parent
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md),
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Constructor

`FeatureAssayPlot(...)` creates an instance of a FeatureAssayPlot class,
where any slot and its value can be passed to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a FeatureAssayPlot
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  replaces any `NA` values in `XAxisFeatureName` and `YAxisFeatureName`
  with the first row name; any `NA` value in `Assay` with the first
  valid assay name; and any `NA` value in `XAxisColumnData` with the
  first valid column metadata field. This will also call the equivalent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method for further refinements to `x`. If no rows or assays are
  present, `NULL` is returned instead.

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
  will return `"Feature assay plot"`.

For creating the plot:

- [`.generateDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x, envir)`
  will create a data.frame of feature expression values in `envir`. It
  will return the commands required to do so as well as a list of
  labels.

For managing selections:

- [`.singleSelectionSlots`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  will return a list specifying the slots that can be updated by single
  selections in transmitter panels, mostly related to the choice of
  feature on the x- and y-axes. This includes the output of the method
  for the parent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  class.

- [`.multiSelectionInvalidated`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `TRUE` if the x-axis uses multiple column selections, such
  that the point coordinates may change upon updates to upstream
  selections in transmitting panels. Otherwise, it dispatches to the
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing a panel-specific tour.

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

x <- FeatureAssayPlot()
x[["XAxis"]]
#> [1] "None"
x[["Assay"]] <- "logcounts"
x[["XAxisColumnData"]] <- "stuff"

##################
# For developers #
##################

library(scater)
sce <- mockSCE()
sce <- logNormCounts(sce)

old_assay_names <- assayNames(sce)
assayNames(sce) <- character(length(old_assay_names))

# Spits out a NULL and a warning if no assays are named.
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class FeatureAssayPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     Assay: 
#>     BrushData: 
#>     ColorBy: None
#>     ColorByColumnData: Mutation_Status
#>     ColorByDefaultColor: black
#>     ColorByFeatureDynamicSource: FALSE
#>     ColorByFeatureName: Gene_0001
#>     ColorByFeatureNameAssay: 
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
#>     XAxis: None
#>     XAxisColumnData: Mutation_Status
#>     XAxisFeatureDynamicSource: FALSE
#>     XAxisFeatureName: Gene_0001
#>     XAxisFeatureSource: ---
#>     YAxisFeatureDynamicSource: FALSE
#>     YAxisFeatureName: Gene_0001
#>     YAxisFeatureSource: ---
#>     ZoomData: 

# Replaces the default with something sensible.
assayNames(sce) <- old_assay_names
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class FeatureAssayPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     Assay: logcounts
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
#>     XAxis: None
#>     XAxisColumnData: Mutation_Status
#>     XAxisFeatureDynamicSource: FALSE
#>     XAxisFeatureName: Gene_0001
#>     XAxisFeatureSource: ---
#>     YAxisFeatureDynamicSource: FALSE
#>     YAxisFeatureName: Gene_0001
#>     YAxisFeatureSource: ---
#>     ZoomData: 
```
