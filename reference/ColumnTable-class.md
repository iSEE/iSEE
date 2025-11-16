# The ColumnTable class

The ColumnTable is a virtual class where each column in the
SummarizedExperiment is represented by no more than row in a `datatable`
widget. In panels of this class, single and multiple selections can only
be transmitted on the samples.

## Slot overview

No new slots are added. All slots provided in the
[Table](https://isee.github.io/iSEE/reference/Table-class.md) parent
class are available.

## Supported methods

In the following code snippets, `x` is an instance of a ColumnTable
class. Refer to the documentation for each method for more details on
the remaining arguments.

For setting up data values:

- [`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md)`(x, se)`
  replaces `NA` values in `Selected` with the first column name of `se`.
  This will also call the equivalent
  [Table](https://isee.github.io/iSEE/reference/Table-class.md) method.

For defining the interface:

- [`.hideInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, field)`
  returns a logical scalar indicating whether the interface element
  corresponding to `field` should be hidden. This returns `TRUE` for row
  selection parameters (`"RowSelectionSource"` and
  `"RowSelectionRestrict"`), otherwise it dispatches to the
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) method.

For monitoring reactive expressions:

- [`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)`(x, se, input, session, pObjects, rObjects)`
  sets up observers to propagate changes in the `Selected` to linked
  plots. This will also call the equivalent
  [Table](https://isee.github.io/iSEE/reference/Table-class.md) method.

For controlling selections:

- [`.multiSelectionDimension`](https://isee.github.io/iSEE/reference/multi-select-generics.md)`(x)`
  returns `"column"` to indicate that a column selection is being
  transmitted.

- [`.singleSelectionDimension`](https://isee.github.io/iSEE/reference/single-select-generics.md)`(x)`
  returns `"sample"` to indicate that a sample identity is being
  transmitted.

For rendering output:

- [`.showSelectionDetails`](https://isee.github.io/iSEE/reference/table-generics.md)`(x)`
  returns a HTML element containing details about the selected row. This
  requires a function to be registered by
  [`registerAppOptions`](https://isee.github.io/iSEE/reference/registerAppOptions.md)
  under the option name `"ColumnTable.select.details"`. The function
  should take a string containing the name of a feature (i.e., the
  current selection in the ColumnTable) and returns a HTML element. If
  no function is registered, `NULL` is returned.

Unless explicitly specialized above, all methods from the parent classes
[Table](https://isee.github.io/iSEE/reference/Table-class.md) and
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) are also
available.

## Subclass expectations

Subclasses are expected to implement methods for:

- [`.generateTable`](https://isee.github.io/iSEE/reference/table-generics.md)

- [`.fullName`](https://isee.github.io/iSEE/reference/getEncodedName.md)

- [`.panelColor`](https://isee.github.io/iSEE/reference/getPanelColor.md)

The method for
[`.generateTable`](https://isee.github.io/iSEE/reference/table-generics.md)
should create a `tab` data.frame where each row corresponds to a column
in the SummarizedExperiment object.

## See also

[Table](https://isee.github.io/iSEE/reference/Table-class.md), for the
immediate parent class that contains the actual slot definitions.

## Author

Aaron Lun
