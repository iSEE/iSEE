# The RowDotPlot virtual class

The RowDotPlot is a virtual class where each row in the
SummarizedExperiment is represented by no more than one point (i.e., a
“dot”) in a brushable
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) plot. It
provides slots and methods to extract `rowData` fields to control the
per-point aesthetics on the plot. This panel will transmit row
identities in both its single and multiple selections, and it can
receive multiple row selections but not multiple column selections.

## Slot overview

The following slots control coloring of the points:

- `ColorByRowData`, a string specifying the `rowData` field for
  controlling point color, if `ColorBy="Row data"` (see the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class).
  Defaults to the first valid field (see `.cacheCommonInfo` below).

- `ColorBySampleNameAssay`, a string specifying the assay of the
  SummarizedExperiment object containing values to use for coloring, if
  `ColorBy="Sample name"`. Defaults to `"logcounts"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md),
  falling back to the name of the first valid assay (see
  `?"`[`.cacheCommonInfo,DotPlot-method`](https://isee.github.io/iSEE/reference/DotPlot-class.md)`"`
  for the definition of validity).

- `ColorByFeatureNameColor`, a string specifying the color to use for
  coloring an individual sample on the plot, if
  `ColorBy="Feature name"`. Defaults to `"red"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

The following slots control other metadata-related aesthetic aspects of
the points:

- `ShapeByRowData`, a string specifying the `rowData` field for
  controlling point shape, if `ShapeBy="Row data"` (see the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class).
  The specified field should contain categorical values; defaults to the
  first such field.

- `SizeByRowData`, a string specifying the `rowData` field for
  controlling point size, if `SizeBy="Row data"` (see the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class).
  The specified field should contain continuous values; defaults to the
  first such field.

- `TooltipRowData`, a character vector specifying `rowData` fields to
  show in the tooltip. Defaults to \`character(0)\`, which displays only
  the \`rownames\` value of the data point.

In addition, this class inherits all slots from its parent
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Supported methods

In the following code snippets, `x` is an instance of a RowDotPlot
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x)`
  adds a `"RowDotPlot"` entry containing `valid.rowData.names`, a
  character vector of valid column data names (i.e., containing atomic
  values); `discrete.rowData.names`, a character vector of names for
  discrete columns; and `continuous.rowData.names`, a character vector
  of names of continuous columns. This will also call the equivalent
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
  method.

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  replaces `NA` values in `ColorByFeatAssay` with the first valid assay
  name in `se`. This will also call the equivalent
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
  method.

For defining the interface:

- [`.hideInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, field)`
  returns a logical scalar indicating whether the interface element
  corresponding to `field` should be hidden. This returns `TRUE` for row
  selection parameters (`"RowSelectionSource"` and
  `"RowSelectionRestrict"`), otherwise it dispatches to the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

For monitoring reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers for all slots in the RowDotPlot. This will also call
  the equivalent
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
  method.

For controlling selections:

- [`.multiSelectionRestricted`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns a logical scalar indicating whether `x` is restricting the
  plotted points to those that were selected in a transmitting panel.

- [`.multiSelectionDimension`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `"row"` to indicate that a multiple row selection is being
  transmitted.

- [`.multiSelectionInvalidated`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `TRUE` if the faceting options use the multiple row
  selections, such that the point coordinates/domain may change upon
  updates to upstream selections in transmitting panels.

- [`.singleSelectionDimension`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  returns `"feature"` to indicate that a feature identity is being
  transmitted.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing the steps of a tour relevant to
  subclasses, mostly tuning the more generic descriptions from the same
  method of the parent
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md).

- [`.getDotPlotColorHelp`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x, color_choices)`
  returns a data.frame containing the documentation for the `"ColorBy"`
  UI element, specialized for row-based dot plots.

Unless explicitly specialized above, all methods from the parent classes
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) are also
available.

## Subclass expectations

Subclasses are expected to implement methods for, at least:

- [`.generateDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)

The method for
[`.generateDotPlotData`](https://isee.github.io/iSEE/reference/plot-generics.md)
should create a `plot.data` data.frame with one row per row in the
SummarizedExperiment object.

## See also

[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md), for
the immediate parent class that contains the actual slot definitions.

## Author

Aaron Lun
