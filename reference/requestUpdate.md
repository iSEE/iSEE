# Request Panel updates

Request a re-rendering of the
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) output via
reactive variables.

## Usage

``` r
.requestUpdate(panel_name, rObjects)

.requestCleanUpdate(panel_name, pObjects, rObjects)

.requestActiveSelectionUpdate(
  panel_name,
  session,
  pObjects,
  rObjects,
  update_output = TRUE
)
```

## Arguments

- panel_name:

  String containing the panel name.

- rObjects:

  A reactive list of values generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- pObjects:

  An environment containing global parameters generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- session:

  The Shiny session object from the server function.

- update_output:

  A logical scalar indicating whether to call `.requestUpdate` as well.

## Value

`.requestUpdate` will modify `rObjects` to request a re-rendering of the
specified panel.

`.requestCleanUpdate` will also remove all active/saved selections in
the chosen panel.

`.requestActiveSelectionUpdate` will modify `rObjects` to indicate that
the active multiple selection for `panel_name` has changed. If
`update_output=TRUE`, it will also request a re-rendering of the panel.

All functions will invisibly return `NULL`.

## Details

`.requestUpdate` should be used in various observers to request a
re-rendering of the panel, usually in response to user-driven parameter
changes in
[`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md).

`.requestCleanUpdate` is used for changes to protected parameters that
invalidate existing multiple selections, e.g., if the coordinates change
in a [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md),
existing brushes and lassos are usually not applicable.

`.requestActiveSelectionUpdate` should be used in the observer
expression that implements the panel's multiple selection mechanism.

## See also

[`.createProtectedParameterObservers`](https://isee.github.io/iSEE/reference/createProtectedParameterObservers.md),
for examples where the update-requesting functions are used.

## Author

Aaron Lun
