# The SampleAssayPlot panel

The SampleAssayPlot is a panel class for creating a
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
where the y-axis represents the expression of a sample of interest,
using the `assay` values of the SummarizedExperiment. It provides slots
and methods for specifying the sample and what to plot on the x-axis, as
well as a method to actually create a data.frame containing those pieces
of data in preparation for plotting.

## Slot overview

The following slots control the values on the y-axis:

- `YAxisSampleName`, a string specifying the name of the sample to plot
  on the y-axis. If `NA`, defaults to the first column name of the
  SummarizedExperiment object.

- `Assay`, string specifying the name of the assay to use for obtaining
  expression values. Defaults to `"logcounts"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md),
  falling back to the name of the first valid assay (see
  `?"`[`.cacheCommonInfo,DotPlot-method`](https://isee.github.io/iSEE/reference/DotPlot-class.md)`"`
  for the definition of validity).

- `YAxisSampleSource`, string specifying the encoded name of the
  transmitting panel to obtain a single selection that replaces
  `YAxisSampleName`. Defaults to `"---"`, i.e., no transmission is
  performed.

- `YAxisSampleDynamicSource`, a logical scalar indicating whether `x`
  should dynamically change its selection source for the y-axis.
  Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

The following slots control the values on the x-axis:

- `XAxis`, string specifying what should be plotted on the x-axis. This
  can be any one of `"None"`, `"Sample name"`, `"Row data"` or
  `"Row selection"`. Defaults to `"None"`.

- `XAxisColumnData`, string specifying which column of the `colData`
  should be shown on the x-axis, if `XAxis="Column data"`. Defaults to
  the first valid `colData` field (see
  `?"`[`.refineParameters,ColumnDotPlot-method`](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)`"`
  for details).

- `XAaxisSampleName`, string specifying the name of the sample to plot
  on the x-axis, if `XAxis="Sample name"`. Defaults to the first column
  name.

- `XAxisSampleSource`, string specifying the encoded name of the
  transmitting panel to obtain a single selection that replaces
  `XAxisSampleName`. Defaults to `"---"`, i.e., no transmission is
  performed.

- `XAxisSampleDynamicSource`, a logical scalar indicating whether `x`
  should dynamically change its selection source for the x-axis.
  Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

In addition, this class inherits all slots from its parent
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md),
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Constructor

`SampleAssayPlot(...)` creates an instance of a SampleAssayPlot class,
where any slot and its value can be passed to `...` as a named argument.

## Supported methods

In the following code snippets, `x` is an instance of a SampleAssayPlot
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  replaces any `NA` values in `XAxisSampleName` and `YAxisSampleName`
  with the first column name; any `NA` value in `Assay` with the first
  valid assay name; and any `NA` value in `XAxisColumnData` with the
  first valid column metadata field. This will also call the equivalent
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
  method for further refinements to `x`. If no columns or assays are
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
  will return `"Sample assay plot"`.

For creating the plot:

- [`.generateDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x, envir)`
  will create a data.frame of sample assay values in `envir`. It will
  return the commands required to do so as well as a list of labels.

For managing selections:

- [`.singleSelectionSlots`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  will return a list specifying the slots that can be updated by single
  selections in transmitter panels, mostly related to the choice of
  sample on the x- and y-axes. This includes the output of the method
  for the parent
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
  class.

- [`.multiSelectionInvalidated`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `TRUE` if the x-axis uses multiple row selections, such that
  the point coordinates may change upon updates to upstream selections
  in transmitting panels. Otherwise, it dispatches to the
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
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

x <- SampleAssayPlot()
x[["XAxis"]]
#> [1] "None"
x[["Assay"]] <- "logcounts"
x[["XAxisRowData"]] <- "stuff"

##################
# For developers #
##################

library(scater)
sce <- mockSCE()
library(scrapper)
sce <- normalizeRnaCounts.se(sce)

old_assay_names <- assayNames(sce)
assayNames(sce) <- character(length(old_assay_names))

# Spits out a NULL and a warning if no assays are named.
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class SampleAssayPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     Assay: 
#>     BrushData: 
#>     ColorBy: None
#>     ColorByDefaultColor: black
#>     ColorByFeatureDynamicSource: FALSE
#>     ColorByFeatureName: Gene_0001
#>     ColorByFeatureNameColor: red
#>     ColorByFeatureSource: ---
#>     ColorByRowData: NA
#>     ColorBySampleDynamicSource: FALSE
#>     ColorBySampleName: Cell_001
#>     ColorBySampleNameAssay: 
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
#>     SizeByRowData: NA
#>     TooltipRowData: 
#>     VersionInfo: list of length 1
#>     ViolinAdd: TRUE
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Color
#>     XAxis: None
#>     XAxisRowData: stuff
#>     XAxisSampleDynamicSource: FALSE
#>     XAxisSampleName: Cell_001
#>     XAxisSampleSource: ---
#>     YAxisSampleDynamicSource: FALSE
#>     YAxisSampleName: Cell_001
#>     YAxisSampleSource: ---
#>     ZoomData: 

# Replaces the default with something sensible.
assayNames(sce) <- old_assay_names
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class SampleAssayPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     Assay: logcounts
#>     BrushData: 
#>     ColorBy: None
#>     ColorByDefaultColor: black
#>     ColorByFeatureDynamicSource: FALSE
#>     ColorByFeatureName: Gene_0001
#>     ColorByFeatureNameColor: red
#>     ColorByFeatureSource: ---
#>     ColorByRowData: NA
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
#>     SizeByRowData: NA
#>     TooltipRowData: 
#>     VersionInfo: list of length 1
#>     ViolinAdd: TRUE
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Color
#>     XAxis: None
#>     XAxisRowData: stuff
#>     XAxisSampleDynamicSource: FALSE
#>     XAxisSampleName: Cell_001
#>     XAxisSampleSource: ---
#>     YAxisSampleDynamicSource: FALSE
#>     YAxisSampleName: Cell_001
#>     YAxisSampleSource: ---
#>     ZoomData: 
```
