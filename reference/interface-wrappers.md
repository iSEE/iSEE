# iSEE UI element wrappers

Wrapper functions to create the standard shiny user interface elements,
accompanied by an optional help icon that opens an interactive tour
describing the purpose of the element. Also responds to requests to hide
a particular element via
[`.hideInterface`](https://isee.github.io/iSEE/reference/interface-generics.md).

## Usage

``` r
.selectInput.iSEE(x, field, label, ..., help = TRUE)

.selectizeInput.iSEE(x, field, label, ..., help = TRUE)

.checkboxInput.iSEE(x, field, label, ..., help = TRUE)

.checkboxGroupInput.iSEE(x, field, label, ..., help = TRUE)

.sliderInput.iSEE(x, field, label, ..., help = TRUE)

.numericInput.iSEE(x, field, label, ..., help = TRUE)

.radioButtons.iSEE(x, field, label, ..., help = TRUE)
```

## Arguments

- x:

  A [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) object
  for which to construct an interface element.

- field:

  String containing the name of the parameter controlled by the
  interface element.

- label:

  String specifying the label to be shown.

- ...:

  Further arguments to be passed to the corresponding shiny function.

- help:

  Logical scalar indicating whether a help icon should be added to the
  label.

## Value

The output of `FUN(id, ..)` is returned where `FUN` is set the
corresponding shiny function, e.g.,
[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html) for
`.selectInput.iSEE`. `id` is defined by concatenating
[`.getEncodedName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
and `field` (separated by an underscore).

If `.hideInterface(x, field)` is `TRUE`, the output is wrapped inside a
`hidden` call.

## Author

Aaron Lun
