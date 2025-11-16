# Generics for visual DotPlot parameters

These generics allow subclasses to override the user interface elements
controlling visual parameters of
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
panels.

## Interface definition

In all of the code snippets below, `x` is a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) instance
and `se` is the SummarizedExperiment object.

- `.defineVisualColorInterface(x, se, select_info)` should return a HTML
  tag definition that contains UI input elements controlling the `color`
  aesthetic of `ggplot` objects. Here, `select_info` is a list of two
  character vectors named `row` and `column`, which specifies the names
  of panels available for transmitting single selections on the
  rows/columns respectively. A common use case would involve adding
  elements to change the default color of the points or to color by a
  chosen metadata field/assay values.

- `.defineVisualShapeInterface(x, se)` should return a HTML tag
  definition that contains UI input elements controlling the `shape`
  aesthetic of `ggplot` objects. A common use case would involve adding
  elements to change the shape of each point according to a chosen
  metadata field.

- `.defineVisualSizeInterface(x, se)` should return a HTML tag
  definition that contains UI input elements controlling the `size`
  aesthetic of `ggplot` objects. A common use case would involve adding
  elements to change the size of each point according to a chosen
  metadata field or assay values.

- `.defineVisualPointInterface(x, se)` should return a HTML tag
  definition that contains UI input elements controlling other
  aesthetics of `ggplot` objects. This might include controlling the
  transparency or downsampling.

- `.defineVisualFacetInterface(x, se)` should return a HTML tag
  definition that contains UI input elements controlling the
  `facet_grid` applied to `ggplot` objects. This typically involves
  providing UI elements to choose the metadata variables to use for
  faceting.

- `.defineVisualTextInterface(x, se)` should return a HTML tag
  definition that contains UI input elements controlling the appearance
  of non-data text elements of `ggplot` objects. This typically involves
  matters such as the font size and legend position.

- `.defineVisualOtherInterface(x)` should a HTML tag definition that
  contains UI inputs elements to display in the `"Other"` section of the
  visual parameters. This is a grab-bag of other parameters that don't
  fit into the more defined categories above.

A method for any of these generics may also return `NULL`, in which case
the corresponding section of the visual parameter box is completely
hidden.

All of these generics are called by
[`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)
for DotPlot subclasses. Developers of subclasses can simply specialize
these generics to change the UI instead of reimplementing
`.defineInterface` itself.

When implementing methods for these generics, it is a good idea to make
use of information precomputed by
[`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md).
For example,
[`.cacheCommonInfo,ColumnDotPlot-method`](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
will add vectors specifying whether a variable in the `colData` is valid
and discrete or continuous.

## Controlling `ColorBy*Data` choices

`.allowableColorByDataChoices(x, se)` should return a character vector
of the allowable row/column data variables to use when `ColorBy` is set
to `"Row data"` or `"Column data"` for
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s
and
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s,
respectively. The default method will use all available (atomic)
variables, but subclasses can specialize this to only allow, e.g.,
continuous or discrete variables.

## Controlling hover choices

`.getTooltipUI(x, se, name)` should return an `HTML` tag definition
representing information to display in the tooltip that is displayed in
`DotPlot` panels when hovering over a data point. The data point is
identified by `name`, its `rownames` or `colnames` value in `se`.

## Author

Kevin Rue-Albrecht
