# Create observers for a modal for custom dimnames

Create observers to launch a modal where users can input a list of
custom row or column names. These observers register input changes in
the app's memory and request an update to the affected panel.

## Usage

``` r
.createCustomDimnamesModalObservers(
  plot_name,
  slot_name,
  button_name,
  se,
  input,
  session,
  pObjects,
  rObjects,
  source_type
)
```

## Arguments

- plot_name:

  String containing the name of the current panel.

- slot_name:

  String specifying the slot of containing the names of the custom
  features. This will be modified by user interactions with the modal.

- button_name:

  String containing the name of the button in the panel UI that launches
  the modal.

- se:

  A SummarizedExperiment object after running
  [`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md).

- input:

  The Shiny input object from the server function.

- session:

  The Shiny session object from the server function.

- pObjects:

  An environment containing global parameters generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- rObjects:

  A reactive list of values generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- source_type:

  String specifying the type of the panel that is source of the
  selection, either `"row"` or `"column"`.

## Value

Observers are set up to launch the modal and monitor its UI elements. A
`NULL` is invisibly returned.

## Details

This should be called in
[`.createObservers`](https://isee.github.io/iSEE/reference/observer-generics.md)
for the target panel. It assumes that a button element with the suffix
`button_name` is available in the UI (i.e., the full name is created by
concatenated `plot_name` with `button_name`).

The modal UI provides options to sort the dimnames, validate them, clear
the current text and import a selection from a specified row
transmitter. These are all transient until “Apply” is clicked, at which
point the app's memory is modified and an update is requested.

The custom names are stored in the `slot_name` as a single string with
names separated by newlines. Hashes are treated as comments and any
content after a hash is ignored when interpreting the names. Any leading
and trailing whitespace is also ignored during interpretation.

## Author

Kevin Rue-Albrecht
