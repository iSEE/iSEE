# The ComplexHeatmapPlot panel

The ComplexHeatmapPlot is a panel class for creating a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) that
displays an assay of a SummarizedExperiment object as a `Heatmap` with
features as rows and samples and columns, respectively. It provides
slots and methods for specifying the features of interest, which assay
to display in the main heatmap, any transformations to perform on the
data, and which metadata variables to display as row and column heatmap
annotations.

## ComplexHeatmapPlot slot overview

The following slots control the rows that are used:

- `CustomRows`, a logical scalar indicating whether the custom list of
  rows should be used. If `FALSE`, the incoming selection is used
  instead. Defaults to `TRUE`.

- `CustomRowsText`, string containing newline-separated row names. This
  specifies which rows of the SummarizedExperiment object are to be
  shown in the heatmap. If `NA`, defaults to the first row name of the
  SummarizedExperiment.

- `CapRowSelection`, an integer specifying the cap on the number of rows
  from a multiple selection to show in the heatmap, to avoid a frozen
  state when the application attempts to process a very large heatmap.
  Ignored if `CustomRows = TRUE`. Defaults to 200.

The following slots control the metadata variables that are used:

- `ColumnData`, a character vector specifying columns of the `colData`
  to show as `columnAnnotation`. Defaults to `character(0)`.

- `RowData`, a character vector specifying columns of the `rowData` to
  show as `rowAnnotation`. Defaults to `character(0)`.

- `ShowColumnSelection`, a logical vector indicating whether the column
  selection should be shown as an extra annotation bar. Defaults to
  `TRUE`.

- `OrderColumnSelection`, a logical vector indicating whether the column
  selection should be used to order columns in the heatmap. Defaults to
  `TRUE`.

The following slots control the choice of assay values:

- `Assay`, string specifying the name of the assay to use for obtaining
  expression values. Defaults to `"logcounts"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md),
  falling back to the first valid assay name (see `.cacheCommonInfo`
  below).

The following slots control the clustering of rows:

- `ClusterRows`, a logical scalar indicating whether rows should be
  clustered by their assay values. Defaults to `FALSE`.

- `ClusterRowsDistance`, string specifying a distance measure to use.
  This can be any one of `"euclidean"`, `"maximum"`, `"manhattan"`,
  `"canberra"`, `"binary"`, `"minkowski"`, `"pearson"`, `"spearman"`, or
  `"kendall"`. Defaults to `"spearman"`.

- `ClusterRowsMethod`, string specifying a linkage method to use. This
  can be any one of `"ward.D"`, `"ward.D2"`, `"single"`, `"complete"`,
  `"average"`, `"mcquitty"`, `"median"`, or `"centroid"`. Defaults to
  `"ward.D2"`.

The following control transformations applied to rows:

- `AssayCenterRows` is a logical scalar indicating whether assay values
  should be centered for each row.

- `AssayScaleRows` is a logical scalar indicating whether assay values
  should be scaled for each row. This transformation is only applicable
  if `AssayCenterRows` is `TRUE`.

The following slots control the color scale:

- `CustomBounds` is logical scale indicating whether the color scale
  should be constrained by an upper and/or a lower bounds.

- `LowerBound` is a numerical value setting the lower bound of the color
  scale; or `NA` to disable the lower bound when `CustomBounds` is
  `TRUE`.

- `UpperBound` is a numerical value setting the lower bound of the color
  scale; or `NA` to disable the upper bound when `CustomBounds` is
  `TRUE`.

- `DivergentColormap` is a character scalar indicating a 3-color
  divergent colormap to use when `AssayCenterRows` is `TRUE`.

The following slots refer to general plotting parameters:

- `ShowDimNames`, a character vector specifying the dimensions for which
  to display names. This can contain zero or more of `"Rows"` and
  `"Columns"`. Defaults to `"Rows"`.

- `NamesRowFontSize`, a numerical value setting the font size of the row
  names.

- `NamesColumnFontSize`, a numerical value setting the font size of the
  column names.

- `LegendPosition`, string specifying the position of the legend on the
  plot. Defaults to `"Bottom"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md)
  but can also be `"Right"`.

- `LegendDirection`, string specifying the orientation of the legend on
  the plot for continuous covariates. Defaults to `"Horizontal"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md)
  but can also be `"Vertical"`.

The following slots control some aspects of the user interface:

- `DataBoxOpen`, a logical scalar indicating whether the data parameter
  box should be open. Defaults to `FALSE`.

- `VisualBoxOpen`, a logical scalar indicating whether the visual
  parameter box should be open. Defaults to `FALSE`.

In addition, this class inherits all slots from its parent
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

## Constructor

`ComplexHeatmapPlot(...)` creates an instance of a ComplexHeatmapPlot
class, where any slot and its value can be passed to `...` as a named
argument.

## Supported methods

In the following code snippets, `x` is an instance of a
ComplexHeatmapPlot class. Refer to the documentation for each method for
more details on the remaining arguments.

For setting up data values:

- [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x)`
  adds a `"ComplexHeatmapPlot"` entry containing `valid.assay.names`, a
  character vector of valid (i.e., non-empty) assay names;
  `discrete.assay.names`, a character vector of valid assay names with
  discrete atomic values; `continuous.assay.names`, a character vector
  of valid assay names with continuous atomic values;
  `valid.colData.names`, a character vector of names of columns in
  `colData` that are valid; `discrete.colData.names`, a character vector
  of names for columns in `colData` with discrete atomic values;
  `continuous.colData.names`, a character vector of names of columns in
  `colData` with continuous atomic values; `valid.rowData.names`, a
  character vector of names of columns in `rowData` that are valid;
  `discrete.rowData.names`, a character vector of names for columns in
  `rowData` with discrete atomic values; `continuous.rowData.names`, a
  character vector of names of columns in `rowData` with continuous
  atomic values. Valid assay names are defined as those that are
  non-empty, i.e., not `""`; valid columns in `colData` and `rowData`
  are defined as those that contain atomic values. This will also call
  the equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  replaces any `NA` value in `"Assay"` with the first valid assay name;
  and `NA` value in `"CustomRowsText"` with the first row name. This
  will also call the equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method
  for further refinements to `x`. If no valid column metadata fields are
  available, `NULL` is returned instead.

For defining the interface:

- [`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  defines the user interface for manipulating all slots described above
  and in the parent classes. TODO It will also create a data parameter
  box that can respond to specialized
  [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md),
  and a visual parameter box and a selection parameter box both specific
  to the `ComplexHeatmapPlot` panel. This will *override* the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

- [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  returns a list of interface elements for manipulating all slots
  described above.

- [`.defineOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x)`
  returns a UI element for a brushable plot.

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)`(x)`
  will return the specified default color for this panel class.

- [`.hideInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, field)`
  returns a logical scalar indicating whether the interface element
  corresponding to `field` should be hidden. This returns `TRUE` for the
  selection history (`"SelectionHistory"`), otherwise it dispatches to
  the [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
  method.

For generating the output:

- [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, all_memory, all_contents)`
  returns a list containing `plot`, a Heatmap object; `commands`, a list
  of character vector containing the R commands required to generate
  `contents` and `plot`; and `contents` and `varname`, both set to
  `NULL` as this is not a transmitting panel.

- [`.exportOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, all_memory, all_contents)`
  will create a PDF file containing the current plot, and return a
  string containing the path to that PDF. This assumes that the `plot`
  field returned by
  [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
  is a Heatmap object.

For monitoring reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers for all slots described above and in the parent
  classes. This will also call the equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

- [`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, output, pObjects, rObjects)`
  will add a rendered plot element to `output`. The reactive expression
  will add the contents of the plot to `pObjects$contents` and the
  relevant commands to `pObjects$commands`. This will also call the
  equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method
  to render the panel information text boxes.

For defining the panel name:

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
  will return `"Complex heatmap"`.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing a panel-specific tour.

## See also

[Panel](https://isee.github.io/iSEE/reference/Panel-class.md), for the
immediate parent class.

## Author

Kevin Rue-Albrecht

## Examples

``` r
#################
# For end-users #
#################

x <- ComplexHeatmapPlot()
x[["ShowDimNames"]]
#> [1] "Rows"
x[["ShowDimNames"]] <- c("Rows", "Columns")

##################
# For developers #
##################

library(scater)
sce <- mockSCE()
sce <- logNormCounts(sce)

old_cd <- colData(sce)
colData(sce) <- NULL

# Spits out a NULL and a warning if there is nothing to plot.
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class ComplexHeatmapPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     Assay: logcounts
#>     AssayCenterRows: FALSE
#>     AssayScaleRows: FALSE
#>     CapRowSelection: 200
#>     ClusterRows: FALSE
#>     ClusterRowsDistance: spearman
#>     ClusterRowsMethod: ward.D2
#>     ColumnData: 
#>     ColumnSelectionDynamicSource: FALSE
#>     ColumnSelectionRestrict: FALSE
#>     ColumnSelectionSource: ---
#>     CustomBounds: FALSE
#>     CustomRows: TRUE
#>     CustomRowsText: Gene_0001
#>     DataBoxOpen: FALSE
#>     DivergentColormap: purple < black < yellow
#>     LegendDirection: Horizontal
#>     LegendPosition: Bottom
#>     LowerBound: NA
#>     NamesColumnFontSize: 10
#>     NamesRowFontSize: 10
#>     OrderColumnSelection: TRUE
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     RowData: 
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionSource: ---
#>     SelectionBoxOpen: FALSE
#>     ShowColumnSelection: TRUE
#>     ShowDimNames: Rows Columns
#>     UpperBound: NA
#>     VersionInfo: list of length 1
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Annotations

# Replaces the default with something sensible.
colData(sce) <- old_cd
sce0 <- .cacheCommonInfo(x, sce)
.refineParameters(x, sce0)
#> Panel object of class ComplexHeatmapPlot
#>   Get or set individual parameters with ‘[[’ 
#>   Available parameters:
#>     Assay: logcounts
#>     AssayCenterRows: FALSE
#>     AssayScaleRows: FALSE
#>     CapRowSelection: 200
#>     ClusterRows: FALSE
#>     ClusterRowsDistance: spearman
#>     ClusterRowsMethod: ward.D2
#>     ColumnData: 
#>     ColumnSelectionDynamicSource: FALSE
#>     ColumnSelectionRestrict: FALSE
#>     ColumnSelectionSource: ---
#>     CustomBounds: FALSE
#>     CustomRows: TRUE
#>     CustomRowsText: Gene_0001
#>     DataBoxOpen: FALSE
#>     DivergentColormap: purple < black < yellow
#>     LegendDirection: Horizontal
#>     LegendPosition: Bottom
#>     LowerBound: NA
#>     NamesColumnFontSize: 10
#>     NamesRowFontSize: 10
#>     OrderColumnSelection: TRUE
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     RowData: 
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionSource: ---
#>     SelectionBoxOpen: FALSE
#>     ShowColumnSelection: TRUE
#>     ShowDimNames: Rows Columns
#>     UpperBound: NA
#>     VersionInfo: list of length 1
#>     VisualBoxOpen: FALSE
#>     VisualChoices: Annotations
```
