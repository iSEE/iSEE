# Get panel names

Get panel names

## Usage

``` r
.fullName(x)

.getEncodedName(x)

.getFullName(x)
```

## Arguments

- x:

  An instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

## Value

For `.getEncodedName`, a string containing the encoded panel name of
`x`.

For `.fullName`, a string containing the full (plain-English) name of
the class.

For `.getFullName`, a string containing the full name of `x`.

## Details

The encoded name is used internally as the name of various fields in
`input`, `output` and reactive lists.

The full name is what should be shown in the interface and visible to
the end-user.

## Author

Aaron Lun
