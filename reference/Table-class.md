# The Table class

The Table is a virtual class for all panels containing a `datatable`
widget from the DT package, where each row *usually* corresponds to a
row or column of the SummarizedExperiment object. It provides observers
for rendering the table widget, monitoring single selections, and
applying global and column-specific searches (which serve as multiple
selections).

## Slot overview

The following slots control aspects of the
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) selection:

- `Selected`, a string containing the name of the currently selected row
  of the data.frame. Defaults to `NA`, in which case the value should be
  chosen by the subclass'
  [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)
  method.

- `Search`, a string containing the regular expression for the global
  search. Defaults to `""`, i.e., no search.

- `SearchColumns`, a unnamed character vector of length equal to the
  number of columns of the data.frame, where each entry contains the
  search string for its corresponding column. Alternatively, a character
  vector of variable length, containing search strings for one or more
  columns. Defaults to an character vector of length zero, which is
  internally expanded to an vector of zero-length strings, i.e., no
  search.

The following slots control the appearance of the table:

- `HiddenColumns`, a character vector containing names of columns to
  hide. Defaults to an empty vector.

In addition, this class inherits all slots from its parent
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

## Supported methods

In the following code snippets, `x` is an instance of a Table class.
Refer to the documentation for each method for more details on the
remaining arguments.

For defining the interface:

- [`.defineOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x)`
  returns a UI element for a
  [`dataTableOutput`](https://rdrr.io/pkg/DT/man/dataTableOutput.html)
  widget.

- [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x)`
  will create interface elements for modifying the table, namely to
  choose which columns to hide. Note that this is populated by
  [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
  upon table rendering, as we do not know the available columns before
  that point.

For defining reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers for all of the slots. This will also call the
  equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

- [`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, output, pObjects, rObjects)`
  will add a rendered `datatable` object to `output`. This will also
  call the equivalent
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method
  to render the panel information text boxes.

- [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, all_memory, all_contents)`
  returns a list containing `contents`, a data.frame with one row per
  point currently present in the table; `commands`, a list of character
  vector containing the R commands required to generate `contents` and
  `plot`; and `varname`, a string specifying the name of the variable in
  `commands` used to generate `contents`.

- [`.exportOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, all_memory, all_contents)`
  will create a CSV file containing the current table, and return a
  string containing the path to that file. This assumes that the
  `contents` field returned by
  [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
  is a data.frame or can be coerced into one.

For controlling selections:

- [`.multiSelectionRestricted`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `TRUE`. Transmission of a selection to a Table will manifest
  as a subsetting of the rows.

- [`.multiSelectionActive`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns a list containing the contents of `x[["Search"]]` and
  `x[["ColumnSearch"]]`. If both contain only empty strings, a `NULL` is
  returned instead.

- [`.multiSelectionCommands`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x, index)`
  returns a character vector of R expressions that - when evaluated -
  return a character vector of the row names of the table after applying
  all search filters. The value of `index` is ignored.

- [`.singleSelectionValue`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x, contents)`
  returns the name of the row that was last selected in the `datatable`
  widget.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns an data.frame containing the steps of a tour relevant to
  subclasses, mostly describing the effect of selection from other
  panels and the use of row filters to transmit selections.

Unless explicitly specialized above, all methods from the parent class
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) are also
available.

## Subclass expectations

The Table is a rather vaguely defined class for which the only purpose
is to avoid duplicating code for
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s
and
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s.
We recommend extending those subclasses instead.

## See also

[Panel](https://isee.github.io/iSEE/reference/Panel-class.md), for the
immediate parent class.

## Author

Aaron Lun
