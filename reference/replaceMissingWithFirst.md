# Replace with first choice

Replace an `NA` or invalid value in a slot of a
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) object
with the first valid choice. This is usually called in
[`.refineParameters`](https://isee.github.io/iSEE/reference/setup-generics.md).

## Usage

``` r
.replaceMissingWithFirst(x, field, choices)
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

`x` where the slot named `field` is replaced with `choices[1]` if its
value was previously `NA` or did not exist in `choices`.

## Author

Aaron Lun
