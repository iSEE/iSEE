# The Panel virtual class

The Panel is a virtual base class for all iSEE panels. It provides slots
and methods to control the height and width of each panel, as well as
functionality to control the choice of “transmitting” panels from which
to receive a multiple row/column selection.

## Slot overview

The following slots are relevant to panel organization:

- `PanelId`, an integer scalar specifying the identifier for the panel.
  This should be unique across panels of the same concrete class.

- `PanelWidth`, an integer scalar specifying the width of the panel.
  Bootstrap coordinates are used so this value should lie between 2 and
  12; defaults to 4 in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `PanelHeight`, an integer scalar specifying the height of the panel in
  pixels. This is expected to lie between 400 and 1000; defaults to 500
  in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

The following slots are relevant to *receiving* a multiple selection on
the rows:

- `RowSelectionSource`, a string specifying the name of the transmitting
  panel from which to receive a multiple row selection (e.g.,
  `"RowDataPlot1"`). Defaults to `"---"`.

- `RowSelectionDynamicSource`, a logical scalar indicating whether `x`
  should dynamically change its selection source for multiple row
  selections. Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `RowSelectionRestrict`, a logical scalar indicating whether the
  display of `x` should be restricted to the rows in the multiple
  selection received from a transmitting panel. Defaults to `FALSE`.

The following slots are relevant to *receiving* a multiple selection on
the columns:

- `ColumnSelectionSource`, a string specifying the name of the
  transmitting panel from which to receive a multiple column selection
  (e.g., `"ColumnDataPlot1"`). Defaults to `"---"`.

- `ColumnSelectionDynamicSource`, a logical scalar indicating whether
  `x` should dynamically change its selection source for multiple column
  selections. Defaults to `FALSE` in
  [`getPanelDefault`](https://isee.github.io/iSEE/reference/panelDefaults.md).

- `ColumnSelectionRestrict`, a logical scalar indicating whether the
  display of `x` should be restricted to the columns in the multiple
  selection received from a transmitting panel. Defaults to `FALSE`.

There are also the following miscellaneous slots:

- `SelectionBoxOpen`, a logical scalar indicating whether the selection
  parameter box should be open at initialization. Defaults to `FALSE`.

- `SelectionHistory`, a list of arbitrary elements that contain
  parameters for saved multiple selections. Each element of this list
  corresponds to one saved selection in the current panel. Defaults to
  an empty list.

- `VersionInfo`, a named list of
  [package_version](https://rdrr.io/r/base/numeric_version.html) objects
  specifying the versions of packages used to create a given Panel
  instance. This information is used to inform
  [`updateObject`](https://rdrr.io/pkg/BiocGenerics/man/updateObject.html)
  of any updates that need to be applied. By default, it is filled with
  a single `"iSEE"` entry containing the current version of iSEE.

## Getting and setting slots

In all of the following code chunks, `x` is an instance of a Panel, and
`i` is a string containing the slot name:

- `x[[i]]` returns the value of a slot named `i`.

- `x[[i]] <- value` modifies `x` so that the value in slot `i` is
  replaced with `value`.

- `show(x)` will print a summary of all (non-hidden) slots and their
  values.

## Supported methods

In the following code snippets, `x` is an instance of a
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  calls
  [`updateObject`](https://rdrr.io/pkg/BiocGenerics/man/updateObject.html)`(x)`.
  If `x` is up to date, this operation is a no-op and returns `x`
  without modification.

- [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  is a no-op, returning `se` without modification.

For defining the interface:

- [`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  will return a list of collapsible boxes for changing data and
  selection parameters. The data parameter box will be populated based
  on
  [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md).

- [`.defineDataInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, se, select_info)`
  will return an empty list.

- [`.hideInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, field)`
  will always return `FALSE`.

For monitoring reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  will add observers to respond to changes in multiple selection
  options. It will also call
  [`.singleSelectionSlots`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  to set up observers for responding to transmitted single selections.

- [`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, output, pObjects, rObjects)`
  will add elements to `output` for rendering the information textboxes
  at the bottom of each panel. Each panel should specialize this method
  to add rendering expressions for the actual output (e.g., plots,
  tables), followed by a `callNextMethod` to create the textboxes.

For generating output:

- [`.exportOutput`](https://isee.github.io/iSEE/reference/output-generics.md)`(x, se, all_memory, all_contents)`
  is a no-op, i.e., it will return an empty character vector and create
  no files.

For documentation:

- [`.definePanelTour`](https://isee.github.io/iSEE/reference/documentation-generics.md)`(x)`
  returns a data.frame containing the selection-related steps of the
  tour.

For controlling selections:

- [`.multiSelectionRestricted`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  will always return `TRUE`.

- [`.multiSelectionDimension`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  will always return `"none"`.

- [`.multiSelectionActive`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  will always return `NULL`.

- [`.multiSelectionClear`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  will always return `x`.

- [`.multiSelectionInvalidated`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  will always return `FALSE`.

- [`.multiSelectionAvailable`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x, contents)`
  will return `nrow(contents)`.

- [`.multiSelectionResponsive`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x, dim)`
  will always return `TRUE`.

- [`.singleSelectionDimension`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  will always return `"none"`.

- [`.singleSelectionValue`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  will always return `NULL`.

- [`.singleSelectionSlots`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  will always return an empty list.

## Subclass expectations

Subclasses are required to implement methods for:

- [`.defineOutput`](https://isee.github.io/iSEE/reference/output-generics.md)

- [`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)

- [`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md)

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)

Subclasses that transmit selections should also implement specialized
methods for selection-related parameters listed above.

## See also

[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) and
[Table](https://isee.github.io/iSEE/reference/Table-class.md), for
examples of direct subclasses.

## Author

Aaron Lun
