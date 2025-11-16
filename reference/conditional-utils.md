# Conditional elements on radio or checkbox selection

Creates a conditional UI element that appears when the user picks a
certain choice in a radio button, single checkbox or checkbox group
interface element.

## Usage

``` r
.conditionalOnRadio(id, choice, ...)

.conditionalOnCheckSolo(id, on_select = TRUE, ...)

.conditionalOnCheckGroup(id, choice, ...)
```

## Arguments

- id:

  String containing the ID of the UI element controlling the relevant
  choice.

- choice:

  String containing the choice for the radio button or checkbox group on
  which to show the conditional element(s).

- ...:

  UI elements to show conditionally.

- on_select:

  Logical scalar specifying whether the conditional element should be
  shown upon selection in a check box, or upon de-selection (if
  `FALSE`).

## Value

A HTML object containing interface elements in `...` that only appear
when the relevant condition is satisfied.

## Details

These functions are just wrappers around
[`conditionalPanel`](https://rdrr.io/pkg/shiny/man/conditionalPanel.html),
with the added value coming from the pre-written conditional expressions
in Javascript. They are useful for hiding elements that are only
relevant when the right radio button or checkbox is selected. This means
that we avoid cluttering the UI with options that are not immediately
useful to the user.

## See also

[`conditionalPanel`](https://rdrr.io/pkg/shiny/man/conditionalPanel.html),
which is used under the hood.

## Author

Aaron Lun
