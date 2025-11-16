# Process multiple selections

Generate and execute commands to process multiple selections, creating
variables in the evaluation environment with the identity of the
selected rows or columns.

## Usage

``` r
.processMultiSelections(x, all_memory, all_contents, envir)
```

## Arguments

- x:

  An instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- all_memory:

  A named list of
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
  instances containing parameters for the current app state.

- all_contents:

  A named list of arbitrary contents with one entry per panel.

- envir:

  The evaluation environment. This is assumed to already contain `se`,
  the SummarizedExperiment object for the current dataset.

## Value

`envir` is populated with one, none or both of `col_selected` and/or
`row_selected`, depending on whether `x` is receiving a multiple
selection on the rows and/or columns. The return value is the character
vector of commands required to construct those variables.

## Details

This function is primarily intended for use by developers of new panels.
It should be called inside
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
to easily process row/column multiple selections. Developers can check
whether `row_selected` or `col_selected` exists in `envir` to determine
whether any row or column selection was performed (and adjust the
behavior of `.generateOutput` accordingly).

## See also

[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
and its related generic
[`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md),
where this function should generally be used.

## Author

Aaron Lun
