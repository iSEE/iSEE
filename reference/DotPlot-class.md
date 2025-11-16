# The DotPlot virtual class

The DotPlot is a virtual class for all panels where each row or column
in the SummarizedExperiment is represented by no more than one point
(i.e., a “dot”) in a brushable
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) plot. It
provides slots and methods to create the plot, to control various
aesthetics of the dots, and to store the brush or lasso selection.

## Slot overview

The following slots are relevant to coloring of the points:

- `ColorBy`, a string specifying how points should be colored. This
  should be one of `"None"`, `"Feature name"`, `"Sample name"` and
  either `"Column data"` (for
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s)
  or `"Row data"` (for
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s).
  Defaults to `"None"`.

- `ColorByDefaultColor`, a string specifying the default color to use
  for all points if `ColorBy="None"`. Defaults to `"black"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `ColorByFeatureName`, a string specifying the feature to be used for
  coloring points when `ColorBy="Feature name"`. For
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s,
  this is used to highlight the point corresponding to the selected
  feature; for
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s,
  this is used to color each point according to the expression of that
  feature. If `NA`, this defaults to the name of the first row.

- `ColorByFeatureSource`, a string specifying the name of the panel to
  use for transmitting the feature selection to `ColorByFeatureName`.
  Defaults to `"---"`.

- `ColorBySampleName`, a string specifying the sample to be used for
  coloring points when `ColorBy="Sample name"`. For
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s,
  this is used to color each point according to the expression of that
  sample; for
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s,
  this is used to highlight the point corresponding to the selected
  sample. If `NA`, this defaults to the name of the first column.

- `ColorBySampleSource`, a string specifying the name of the panel to
  use for transmitting the sample selection to `ColorBySampleNameColor`.
  Defaults to `"---"`.

- `ColorByFeatureDynamicSource`, a logical scalar indicating whether `x`
  should dynamically change its selection source when coloring by
  feature. Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `ColorBySampleDynamicSource`, a logical scalar indicating whether `x`
  should dynamically change its selection source when coloring by
  feature. Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `SelectionAlpha`, a numeric scalar in \[0, 1\] specifying the
  transparency to use for non-selected points. Defaults to 0.1 in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

The following slots control other metadata-related aesthetic aspects of
the points:

- `ShapeBy`, a string specifying how the point shape should be
  determined. This should be one of `"None"` and either `"Column data"`
  (for
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s)
  or `"Row data"` (for
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s).
  Defaults to `"None"`.

- `SizeBy`, a string specifying the metadata field for controlling point
  size. This should be one of `"None"` and either `"Column data"` (for
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s)
  or `"Row data"` (for
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s).
  Defaults to `"None"`.

The following slots control the faceting:

- `FacetRowBy`, a string indicating what to use for creating row facets.
  For
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s,
  this should be one of `"None"`, `"Row data"` or `"Row selection"`. For
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s,
  this should be one of `"None"`, `"Column data"` or
  `"Column selection"`. Defaults to `"None"`, i.e., no row faceting.

- `FacetByColumn`, a string indicating what to use for creating column
  facets. For
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s,
  this should be one of `"None"`, `"Row data"` or `"Row selection"`. For
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s,
  this should be one of `"None"`, `"Column data"` or
  `"Column selection"`. Defaults to `"None"`, i.e., no column faceting.

The following slots control any text to be shown on the plot:

- `LabelCenters`, a logical scalar indicating whether the label the
  centers (technically medoids) of all cells in each group, where groups
  are defined by a discrete covariate in the relevant metadata field.
  Defaults to `FALSE`.

- `LabelCentersBy`, a string specifying the metadata field to define the
  groups when `LabelCenters` is `TRUE`. This should be a discrete
  variable in `rowData` or `colData` for
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s
  and
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s,
  respectively. Defaults to the name of the first column.

- `LabelCentersColor`, a string specifying the color used for the labels
  at the center of each group. Only used when `LabelCenters` is `TRUE`.
  Defaults to `"black"`.

- `CustomLabels`, a logical scalar indicating whether custom labels
  should be inserted on specific points. Defaults to `FALSE`.

- `CustomLabelsText`, a (possibly multi-line) string with the names of
  the points to label when `CustomLabels` is set to `TRUE`. Each line
  should contain the name of a row or column for
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s
  and
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s,
  respectively. Leading and trailing whitespace are stripped, and all
  text on a line after `#` is ignored. Defaults to the name of the first
  row/column.

The following slots control interactions with the plot image:

- `ZoomData`, a named numeric vector of plot coordinates with `"xmin"`,
  `"xmax"`, `"ymin"` and `"ymax"` elements parameterizing the zoom
  boundaries. Defaults to an empty vector, i.e., no zoom.

- `BrushData`, a list containing either a Shiny brush (see
  `?brushedPoints`) or an iSEE lasso (see
  `?`[`lassoPoints`](https://isee.github.io/iSEE/reference/lassoPoints.md)).
  Defaults to an empty list, i.e., no brush or lasso.

- `HoverInfo`, a logical scalar indicating whether the feature/sample
  name should be shown upon mouse-over of the point. Defaults to `TRUE`.

The following slots control some aspects of the user interface:

- `DataBoxOpen`, a logical scalar indicating whether the data parameter
  box should be open. Defaults to `FALSE`.

- `VisualBoxOpen`, a logical scalar indicating whether the visual
  parameter box should be open. Defaults to `FALSE`.

- `VisualChoices`, a character vector specifying the visible interface
  elements upon initialization. This can contain zero or more of
  `"Color"`, `"Shape"`, `"Size"`, `"Point"` , `"Facet"`, `"Text"`, and
  `"Other"`. Defaults to `"Color"`.

The following slots control the addition of a contour:

- `ContourAdd`, logical scalar indicating whether a contour should be
  added to a (scatter) plot. Defaults to `FALSE`.

- `ContourColor`, string specifying the color to use for the contour
  lines. Defaults to `"blue"`.

The following slot controls whether the aspect ratio is fixed to 1 or
not:

- `FixAspectRatio`, logical scalar indicating whether the aspect ratio
  of a scatter plot should be fixed to 1. Defaults to `FALSE`.

The following slot controls whether the violin boundaries are plotted or
not:

- `ViolinAdd`, logical scalar indicating whether the violin boundaries
  should be plotted. Defaults to `TRUE`.

The following slots control the general appearance of the points.

- `PointSize`, positive numeric scalar specifying the relative size of
  the points. Defaults to 1.

- `PointAlpha`, non-negative numeric scalar specifying the transparency
  of the points. Defaults to 1, i.e., not transparent.

- `Downsample`, logical scalar indicating whether to downsample points
  for faster plotting. Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `DownsampleResolution`, numeric scalar specifying the resolution of
  the downsampling grid (see
  `?`[`subsetPointsByGrid`](https://isee.github.io/iSEE/reference/subsetPointsByGrid.md))
  if `Downsample=TRUE`. Larger values correspond to reduced downsampling
  at the cost of plotting speed. Defaults to 200 in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

The following slots refer to general plotting parameters:

- `FontSize`, positive numeric scalar specifying the relative font size.
  Defaults to 1 in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `PointSize`, positive numeric scalar specifying the relative point
  size. Defaults to 1 in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `LegendPosition`, string specifying the position of the legend on the
  plot. Defaults to `"Bottom"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).
  Other valid choices include `"Right"` and `"None"`.

In addition, this class inherits all slots from its parent
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

## Supported methods

In the following code snippets, `x` is an instance of a DotPlot class.
Refer to the documentation for each method for more details on the
remaining arguments.

For setting up the objects:

- [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x)`
  adds a `"DotPlot"` entry containing `valid.assay.names`, a character
  vector of valid assay names. Valid names are defined as those that are
  non-empty, i.e., not `""`. This method will also call the equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  replaces `NA` values in `ColorByFeatureName` and
  `ColorBySampleNameColor` with the first row and column name,
  respectively, of `se`. This will also call the equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

For defining the interface:

- [`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  defines the user interface for manipulating all slots described above
  and in the parent classes. It will also create a data parameter box
  that can respond to specialized
  [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md).
  This will *override* the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

- [`.defineVisualColorInterface`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se, select_info)`
  defines the user interface subpanel for manipulating the color of the
  points.

- [`.defineVisualShapeInterface`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se)`
  defines the user interface subpanel for manipulating the shape of the
  points.

- [`.defineVisualSizeInterface`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se)`
  defines the user interface subpanel for manipulating the size of the
  points.

- [`.defineVisualPointInterface`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se)`
  defines the user interface subpanel for manipulating other
  point-related parameters.

- [`.defineVisualFacetInterface`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se)`
  defines the user interface subpanel for manipulating facet-related
  parameters.

- [`.defineVisualTextInterface`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se)`
  defines the user interface subpanel for manipulating text-related
  parameters.

- [`.defineVisualOtherInterface`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se)`
  defines the user interface subpanel for manipulating other parameters.
  Currently this returns `NULL`.

- [`.defineOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x)`
  returns a UI element for a brushable plot.

- [`.allowableColorByDataChoices`](https://isee.github.io/iSEE/reference/visual-parameters-generics.md)`(x, se)`
  returns a character vector containing all atomic variables in the
  relevant `*Data` dimension.

For generating the output:

- [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, all_memory, all_contents)`
  returns a list containing `contents`, a data.frame with one row per
  point currently present in the plot; `plot`, a
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object;
  `commands`, a list of character vector containing the R commands
  required to generate `contents` and `plot`; and `varname`, a string
  containing the name of the variable in `commands` that was used to
  obtain `contents`.

- [`.generateDotPlot`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x, labels, envir)`
  returns a list containing `plot` and `commands`, as described above.
  This is called within
  [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
  for all DotPlot instances by default. Methods are also guaranteed to
  generate a `dot.plot` variable in `envir` containing the
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object
  corresponding to `plot`.

- [`.prioritizeDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x, envir)`
  returns `NULL`.

- [`.colorByNoneDotPlotField`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x)`
  returns `NULL`.

- [`.colorByNoneDotPlotScale`](https://isee.github.io/iSEE/reference/plot-generics.md)`(x)`
  returns `NULL`.

- [`.exportOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, all_memory, all_contents)`
  will create a PDF file containing the current plot, and return a
  string containing the path to that PDF. This assumes that the `plot`
  field returned by
  [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
  is a [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object.

For defining reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers for some (but not all!) of the slots. This will also
  call the equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

- [`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, output, pObjects, rObjects)`
  will add a rendered plot element to `output`. The reactive expression
  will add the contents of the plot to `pObjects$contents` and the
  relevant commands to `pObjects$commands`. This will also call the
  equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method
  to render the panel information text boxes.

For controlling selections:

- [`.multiSelectionCommands`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x, index)`
  returns a character vector of R expressions that - when evaluated -
  returns a character vector of the names of selected points in the
  active and/or saved selections of `x`. The active selection is
  returned if `index=NA`, otherwise one of the saved selection is
  returned.

- [`.multiSelectionActive`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `x[["BrushData"]]` or `NULL` if there is no brush or closed
  lasso.

- [`.multiSelectionClear`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `x` after setting the `BrushData` slot to an empty list.

- [`.singleSelectionValue`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x, contents)`
  returns the name of the first selected element in the active brush. If
  no brush is active, `NULL` is returned instead.

- [`.singleSelectionSlots`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  will return a list specifying the slots that can be updated by single
  selections in transmitter panels, mostly related to the choice of
  coloring parameters. This includes the output of `callNextMethod`.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing the steps of a tour relevant to
  subclasses, mostly describing the specification of visual effects and
  the creation of a brush or lasso.

Unless explicitly specialized above, all methods from the parent class
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) are also
available.

## Subclass expectations

The DotPlot is a rather vaguely defined class for which the only purpose
is to avoid duplicating code for
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s
and
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s.
We recommend extending those subclasses instead.

## See also

[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)
and
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md),
which are more amenable to extension.

## Author

Aaron Lun
