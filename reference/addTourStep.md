# Add a step to the tour

Utility to add a step to the panel-specific rintrojs tour, generating
the `element` tag automatically.

## Usage

``` r
.addTourStep(x, field, text, is_selectize = FALSE)
```

## Arguments

- x:

  A [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) object
  to be toured.

- field:

  String containing the name of the slot of `x`, itself corresponding to
  an interface element to highlight.

- text:

  String containing the text to show in the corresponding step of the
  tour.

- is_selectize:

  Logical scalar indicating whether `field` corresponds to a selectize
  element.

## Value

Character vector of length two. The first entry contains the `element`
tag to identify the interface element to highlight, while the second
entry contains the `text`.

Alternatively, `NULL` may be returned if
[`.hideInterface`](https://isee.github.io/iSEE/reference/interface-generics.md)`(x, field)`
indicates that the corresponding interface element has been hidden.

## Author

Aaron Lun
