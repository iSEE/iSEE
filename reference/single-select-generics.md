# Generics for controlling single selections

A panel can create a single selection on either the rows or columns and
transmit this selection to another panel for use as an aesthetic
parameter. For example, users can click on a
[RowTable](https://isee.github.io/iSEE/reference/RowTable-class.md) to
select a gene of interest, and then the panel can transmit the
identities of that row to another panel for coloring by that selected
gene's expression. This suite of generics controls the behavior of these
single selections.

## Specifying the nature of the selection

Given an instance of the
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class `x`,
`.singleSelectionDimension(x)` should return a string specifying whether
the panel's single selection would contain a `"feature"`, `"sample"`, or
if the Panel in `x` does not perform single selections at all
(`"none"`). The output should be constant for all instances of `x`.

## Obtaining the selected element

`.singleSelectionValue(x, contents)` should return a string specifying
the selected row or column in `x` that is to be transmitted to other
panels. If no row or column is selected, it should return `NULL`.

`contents` is any arbitrary structure returned by
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
for `x` in the field of the same name. This should contain all of the
information necessary to determine the name of the selected row/column.
For example, a data.frame of coordinates is stored by
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s to
identify the point selected by a brush/lasso.

## Indicating the receiving slots

`.singleSelectionSlots(x)` controls how `x` should *respond* to a single
selection. It should return a list of lists, where each internal list
describes a set of slots in `x` that might respond to a single selection
from a transmitting panel. This internal list should contain at least
entries with the following names:

- `param`, the name of the slot of `x` that can potentially respond to a
  single selection in a transmitting panel, e.g., `ColorByFeatureName`
  in [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s.

- `source`, the name of the slot of `x` that indicates which
  transmitting panel to respond to, e.g., `ColorByFeatureSource` in
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s.

For each set of responsive slots, the expected paradigm is that the user
interface will contain two
[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)
elements, one for each of the `param` and `source` slots. Users are free
to manually alter the choice of feature/sample in the `param`'s
`selectInput`. Users are also allowed to change the identity of the
transmitting panel via the `source`'s `selectInput`, which will
automatically update the chosen entry in the `param`'s `selectInput`
when the transmitter's single selection changes.

Developers are strongly recommended to follow the above paradigm. In
fact, the observers to perform these updates are automatically set up by
[`.createObservers,Panel-method`](https://isee.github.io/iSEE/reference/Panel-class.md)
if the internal list also contains the following named entries:

- `dimension`, a string set to either `"feature"` or `"sample"`. This
  specifies whether the slot specified by `param` contains the identity
  of a single feature or a single sample. If this is not present, no
  observers will be set up.

- `dynamic`, the name of the slot indicating whether the choice of
  transmitting panel should change dynamically. One example would be
  `"ColorByFeatureDynamicSource"` for
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s. If
  supplied, a
  [`checkboxInput`](https://rdrr.io/pkg/shiny/man/checkboxInput.html)
  should also be present in the UI to turn on/off dynamic choices for
  this parameter. This field can be missing if the current panel does
  not support dynamic selection sources.

- `use_mode`, the name of the slot of `x` containing the current usage
  mode. This is used in cases where there are multiple choices of which
  only one involves using information held in `source`. An example would
  be `ColorBy` in
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s
  where coloring by feature name is only one of many options, such that
  the panel should only respond to transmitted single selections when
  the user intends to color by feature name. If the value of this field
  is `NA`, the usage mode for `x` is assumed to be such that the panel
  should always respond to transmitted single selections.

- `use_value`, a string containing the relevant value of the slot
  specified by `use_mode` in order for the panel to respond to
  transmitted single selections. An example would be `"Feature name"` in
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s.
  This field can be missing if `use_mode` is `NA`.

- `protected`, a logical scalar indicating whether the slot specified by
  `param` is “protected”, i.e., changing this value will cause all
  existing selections to be invalidated and will trigger re-rendering of
  the children receiving multiple selections. This is `FALSE` for purely
  aesthetic parameters (e.g., coloring) and `TRUE` for data-related
  parameters (e.g., `XAxisFeatureName` in
  [FeatureAssayPlot](https://isee.github.io/iSEE/reference/FeatureAssayPlot-class.md)).

## Author

Aaron Lun
