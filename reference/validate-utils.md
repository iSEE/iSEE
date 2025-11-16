# Validation error utilities

Helper functions to implement
[`setValidity`](https://rdrr.io/r/methods/validObject.html) methods for
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
subclasses.

## Usage

``` r
.singleStringError(msg, x, fields)

.validLogicalError(msg, x, fields)

.validStringError(msg, x, fields)

.allowableChoiceError(msg, x, field, allowable)

.multipleChoiceError(msg, x, field, allowable)

.validNumberError(msg, x, field, lower, upper)
```

## Arguments

- msg:

  Character vector containing the current error messages.

- x:

  An instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
  subclass.

- fields:

  Character vector containing the names of the relevant slots.

- field:

  String containing the name of the relevant slot under investigation.

- allowable:

  Character vector of allowable choices for a multiple-choice selection.

- lower:

  Numeric scalar specifying the lower bound of possible values.

- upper:

  Numeric scalar specifying the upper bound of possible values.

## Value

All functions return a character vector containing `msg`, possibly
appended with additional error messages.

## Details

`.singleStringError` adds an error message if any of the slots named in
`fields` does not contain a single string.

`.validStringError` adds an error message if any of the slots named in
`fields` does not contain a single non-`NA` string.

`.validLogicalError` adds an error message if any of the slots named in
`fields` does not contain a non-`NA` logical scalar.

`.allowableChoiceError` adds an error message if the slot named `field`
does not have a value in `allowable`, assuming it contains a single
string.

`.multipleChoiceError` adds an error message if the slot named `field`
does not have all of its values in `allowable`, assuming it contains a
character vector of any length.

`.validNumberError` adds an error message if the slot named `field` is
not a non-`NA` number within \[`lower`, `upper`\].

## Author

Aaron Lun
