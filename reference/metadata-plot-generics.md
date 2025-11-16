# Generics for row/column metadata plots

These generics allow subclasses to refine the choices of allowable
variables on the x- and y-axes of a
[ColumnDataPlot](https://isee.github.io/iSEE/reference/ColumnDataPlot-class.md)
or
[RowDataPlot](https://isee.github.io/iSEE/reference/RowDataPlot-class.md).
This is most useful for restricting the visualization to a subset of
variables, e.g., only taking log-fold changes in a y-axis of a MA plot.

## Allowable y-axis choices

`.allowableYAxisChoices(x, se)` takes `x`, a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) instance;
and `se`, the SummarizedExperiment object. It is expected to return a
character vector containing the names of acceptable variables to show on
the y-axis. For
[ColumnDataPlot](https://isee.github.io/iSEE/reference/ColumnDataPlot-class.md)s,
these should be a subset of the variables in `colData(se)`, while for
[RowDataPlot](https://isee.github.io/iSEE/reference/RowDataPlot-class.md)s,
these should be a subset of the variables in `rowData(se)`.

Given a constant `se`, the output of this function should be constant
for all instances of the same panel class. As such, it is a good idea to
make use of information precomputed by
[`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md).
For example,
[`.cacheCommonInfo,ColumnDotPlot-method`](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
will add vectors specifying whether a variable in the `colData` is valid
and discrete or continuous, which can be intersected with any additional
requirements in a subclass's method for this generic.

This generic is called by
[`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)
for
[ColumnDataPlot](https://isee.github.io/iSEE/reference/ColumnDataPlot-class.md)s
and
[RowDataPlot](https://isee.github.io/iSEE/reference/RowDataPlot-class.md)s.
Thus, developers wanting to restrict those choices for subclasses can
simply specialize `.allowableYAxisChoices` rather than reimplementing
`.defineDataInterface`.

## Allowable x-axis choices

`.allowableXAxisChoices(x, se)` is the same as above but controls the
variables that can be shown on the x-axis. This need not return the same
subset of variables as `.allowableYAxisChoices`. However, again, the
output of this function should be constant for all instances of the same
class and a constant `se`.

## Author

Aaron Lun
