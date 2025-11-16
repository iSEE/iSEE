# Define parameter observers

Define a series of observers to track “protected” or “unprotected”
parameters for a given panel. These will register input changes to each
specified parameter in the app's memory and request an update to the
output of the affected panel.

## Usage

``` r
.createUnprotectedParameterObservers(
  panel_name,
  fields,
  input,
  pObjects,
  rObjects,
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)

.createProtectedParameterObservers(
  panel_name,
  fields,
  input,
  pObjects,
  rObjects,
  ignoreInit = TRUE,
  ignoreNULL = TRUE
)
```

## Arguments

- panel_name:

  String containing the name of the panel.

- fields:

  Character vector of names of parameters for which to set up observers.

- input:

  The Shiny input object from the server function.

- pObjects:

  An environment containing global parameters generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- rObjects:

  A reactive list of values generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- ignoreInit, ignoreNULL:

  Further arguments to pass to
  [`observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html).

## Value

Observers are set up to monitor the UI elements that can change the
protected and non-fundamental parameters. A `NULL` is invisibly
returned.

## Details

A protected parameter is one that breaks existing multiple selections,
e.g., by changing the actual data being plotted. Alterations to
protected parameters will clear all active and saved selections in the
panel, as those existing selections are assumed to not make any sense in
the context of the modified output of that panel.

By comparison, an unprotected parameter only changes the aesthetics and
will not clear existing selections.

## See also

[`.requestUpdate`](https://isee.github.io/iSEE/reference/requestUpdate.md)
and
[`.requestCleanUpdate`](https://isee.github.io/iSEE/reference/requestUpdate.md),
used to trigger updates to the panel output.

## Author

Aaron Lun
