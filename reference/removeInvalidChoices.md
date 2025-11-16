# Remove invalid values in multiple choices

Removes invalid values in a slot of a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) object.
This is usually called in
[`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md).

## Usage

``` r
.removeInvalidChoices(x, field, choices)
```

## Arguments

- x:

  An instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- field:

  String containing the name of the relevant slot.

- choices:

  Character vector of permissible values for this slot.

## Value

`x` where the slot named `field` is replaced only with the values that
exist in `choices`.

## Author

Kevin Rue-Albrecht
