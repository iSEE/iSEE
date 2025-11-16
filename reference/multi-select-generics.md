# Generics for controlling multiple selections

A panel can create a multiple selection on either the rows or columns
and transmit this selection to another panel to affect its set of
displayed points. For example, users can brush on a
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s to
select a set of points, and then the panel can transmit the identities
of those points to another panel for highlighting.

This suite of generics controls the behavior of these multiple
selections. In all of the code chunks shown below, `x` is assumed to be
an instance of the
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

## Possibility of selection

`.isBrushable(x)` should return a logical specifying whether the panel
supports selection using a Shiny brush or lasso waypoints. The output
should be constant for all instances of `x` and is used to govern the
reporting of multiple selections in the code tracker.

## Specifying the dimension

`.multiSelectionDimension(x)` should return a string specifying whether
the selection contains rows (`"row"`), columns (`"column"`) or if the
Panel in `x` does not perform multiple selections at all (`"none"`). The
output should be constant for all instances of `x` and is used to govern
the interface choices for the selection parameters.

## Specifying the active selection

`.multiSelectionActive(x)` should return some structure containing all
parameters required to identify all points in the active multiple
selection of `x`. For example, the
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) method
for this generic would return the contents of the `BrushData` slot,
usually a list containing a Shiny brush or lasso waypoints for
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
classes. If `.multiSelectionActive(x)` returns `NULL`, `x` is assumed to
have no active multiple selection.

The active selection is considered to be the one that can be directly
changed by the user, as compared to saved selections that are not
modifiable (other than being deleted on a first-in-last-out basis). This
generic is primarily used to bundle up selection parameters to be stored
in the `SelectionHistory` slot when the user saves the current active
selection.

## Evaluating the selection

`.multiSelectionCommands(x, index)` is expected to return a character
vector of commands to generate a character vector of row or column names
in the desired multiple selection of `x`. If `index=NA`, the desired
selection is the currently active one; developers can assume that
`.multiSelectionActive(x)` returns a non-`NULL` value in this case.
Otherwise, for an integer `index`, it refers to the corresponding saved
selection in the `SelectionHistory`.

The commands will be evaluated in an environment containing:

- `select`, a variable of the same type as returned by
  `.multiSelectionActive(x)`. This will contain the active selection if
  `index=NA` and one of the saved selections otherwise. For example, for
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s,
  `select` will be either a Shiny brush or a lasso structure.

- `contents`, some arbitrary content saved by the rendering expression
  in
  [`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x)`.
  This is most often a data.frame but can be anything as long as
  `.multiSelectionCommands` knows how to process it. For example, a
  data.frame of coordinates is stored by
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s to
  identify the points selected by a brush/lasso.

- `se`, the SummarizedExperiment object containing the current dataset.

The output commands are expected to produce a character vector named
`selected` in the evaluation environment. All other variables generated
by the commands should be prefixed with `.` to avoid name clashes.

## Determining the available points for selection

`.multiSelectionAvailable(x, contents)` is expected to return an integer
scalar specifying the number of points available for selection in the
the current instance of the panel `x`. The `contents` field in the
output of
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
is passed to the `contents` argument of this generic.

The default method for this generic returns `nrow(contents)` for all
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
subclasses, assuming that `contents` is a data.frame where each row
represents a point. If not, this method needs to be specialized in order
to return an accurate total of available points, which is ultimately
used to compute the percentage selected in the multiple selection
information panels.

## Destroying selections

`.multiSelectionClear(x)` should return `x` after removing the active
selection, i.e., so that nothing is selected. This is used internally to
remove multiple selections that do not make sense after protected
parameters have changed. For example, a brush or lasso made on a PCA
plot in
[ReducedDimensionPlot](https://isee.github.io/iSEE/reference/ReducedDimensionPlot-class.md)s
would not make sense after switching to t-SNE coordinates, so the
application will automatically erase those selections to avoid
misleading conclusions.

## Responding to selections

These generics control how `x` responds to a transmitted multiple
selection, not how `x` itself transmits selections.

`.multiSelectionRestricted(x)` should return a logical scalar indicating
whether `x`'s displayed contents will be restricted to the selection
transmitted from *another panel*. This is used to determine whether
child panels of `x` need to be re-rendered when `x`'s transmitter
changes its multiple selection. For example, the method for
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s
and
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s
would return `TRUE` if `RowSelectionRestrict=TRUE` or
`ColumnSelectionRestrict=TRUE`, respectively. Otherwise, it would be
`FALSE` as the transmitted selection is only used for aesthetics, not
for changing the identity of the displayed points.

`.multiSelectionInvalidated(x)` should return a logical scalar
indicating whether a transmission of a multiple selection to `x`
invalidates `x`'s own existing selections. This should only be `TRUE` in
special circumstances, e.g., if receipt of a new multiple selection
causes recalculation of coordinates in a
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md).

`.multiSelectionResponsive(x, dim)` should return a logical scalar
indicating whether `x` is responsive to an incoming multiple selection
on dimension `dim`. `dim` is equal to `"row"` if the multiple selection
of rows, otherwise it is equal to `"column"` for a multiple selection of
columns. For example, the method for
[ComplexHeatmapPlot](https://isee.github.io/iSEE/reference/ComplexHeatmapPlot-class.md)
would return `TRUE` when an incoming selection originates from a
row-oriented panel and `CustomRows=FALSE`. Otherwise, it would be
`FALSE` as the dimension of the transmitted selection is dismissed by
the options of the child panel.

## Author

Aaron Lun
