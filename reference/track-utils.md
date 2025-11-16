# Track internal events

Utility functions to track internal events for a panel by monitoring the
status of reactive variables in `rObjects`.

## Usage

``` r
.trackUpdate(panel_name, rObjects)

.trackSingleSelection(panel_name, rObjects)

.trackMultiSelection(panel_name, rObjects)

.trackRelinkedSelection(panel_name, rObjects)
```

## Arguments

- panel_name:

  String containing the panel name.

- rObjects:

  A reactive list of values generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

## Value

All functions will cause the current reactive context to respond to the
designated event. `NULL` is returned invisibly.

## Details

`.trackUpdate` will track whether an update has been requested to the
current panel via
[`.requestUpdate`](https://isee.github.io/iSEE/reference/requestUpdate.md).

`.trackSingleSelection` will track whether the single selection in the
current panel has changed. Note that this will not cause a reaction if
the change involves cancelling a single selection.

`.trackMultiSelection` will track whether the multiple selections in the
current panel have changed. This will respond for both active and saved
selections.

`.trackRelinkedSelection` will track whether the single or multiple
selection sources have changed.

These functions should be called within observer or rendering
expressions to trigger their evaluation upon panel updates. It is only
safe to call these functions within expressions for the same panel,
e.g., to synchronize multiple output elements. Calling them with another
`panel_name` would be unusual, not least because communication between
panels is managed by the
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) framework and is
outside of the scope of the per-panel observers.

## Author

Aaron Lun
