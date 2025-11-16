# The ColumnDotPlot virtual class

The ColumnDotPlot is a virtual class where each column in the
SummarizedExperiment is represented by no more than one point (i.e., a
“dot”) in a brushable
[ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) plot. It
provides slots and methods to extract `colData` fields to control the
per-point aesthetics on the plot. This panel will transmit column
identities in both its single and multiple selections, and it can
receive multiple column selections but not multiple row selections.

## Slot overview

The following slots control coloring of the points:

- `ColorByColumnData`, a string specifying the `colData` field for
  controlling point color, if `ColorBy="Column data"` (see the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class).
  Defaults to the first valid field (see `.cacheCommonInfo` below).

- `ColorByFeatureNameAssay`, a string specifying the assay of the
  SummarizedExperiment object containing values to use for coloring, if
  `ColorBy="Feature name"`. Defaults to `"logcounts"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md),
  falling back to the name of the first valid assay (see
  `?"`[`.cacheCommonInfo,DotPlot-method`](https://isee.github.io/iSEE/reference/DotPlot-class.md)`"`
  for the definition of validity).

- `ColorBySampleNameColor`, a string specifying the color to use for
  coloring an individual sample on the plot, if `ColorBy="Sample name"`.
  Defaults to `"red"` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

The following slots control other metadata-related aesthetic aspects of
the points:

- `ShapeByColumnData`, a string specifying the `colData` field for
  controlling point shape, if `ShapeBy="Column data"` (see the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class).
  The specified field should contain categorical values; defaults to the
  first such valid field.

- `SizeByColumnData`, a string specifying the `colData` field for
  controlling point size, if `SizeBy="Column data"` (see the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class).
  The specified field should contain continuous values; defaults to the
  first such valid field.

- `TooltipColumnData`, a character vector specifying `colData` fields to
  show in the tooltip. Defaults to \`character(0)\`, which displays only
  the \`colnames\` value of the data point.

In addition, this class inherits all slots from its parent
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Supported methods

In the following code snippets, `x` is an instance of a ColumnDotPlot
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x)`
  adds a `"ColumnDotPlot"` entry containing `valid.colData.names`, a
  character vector of names of columns that are valid (i.e., contain
  atomic values); `discrete.colData.names`, a character vector of names
  for columns with discrete atomic values; and
  `continuous.colData.names`, a character vector of names of columns
  with continuous atomic values. This will also call the equivalent
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
  sets up observers for all slots described above and in the parent
  classes. This will also call the equivalent
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
  method.

For controlling selections:

- [`.multiSelectionRestricted`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns a logical scalar indicating whether `x` is restricting the
  plotted points to those that were selected in a transmitting panel.

- [`.multiSelectionDimension`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `"column"` to indicate that a multiple column selection is
  being transmitted.

- [`.multiSelectionInvalidated`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `TRUE` if the faceting options use multiple column selections,
  such that the point coordinates/domain may change upon updates to
  upstream selections in transmitting panels.

- [`.singleSelectionDimension`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  returns `"sample"` to indicate that a sample identity is being
  transmitted.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing the steps of a tour relevant to
  subclasses, mostly tuning the more generic descriptions from the same
  method of the parent
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md).

- [`.getDotPlotColorHelp`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x, color_choices)`
  returns a data.frame containing the documentation for the `"ColorBy"`
  UI element, specialized for column-based dot plots.

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
should create a `plot.data` data.frame with one row per column in the
SummarizedExperiment object.

## See also

[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md), for
the immediate parent class that contains the actual slot definitions.

## Author

Aaron Lun
