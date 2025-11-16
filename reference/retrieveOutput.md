# Retrieve the panel output

Retrieve the results of a previous
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
call on this panel.

## Usage

``` r
.retrieveOutput(panel_name, se, pObjects, rObjects)
```

## Arguments

- panel_name:

  String containing the panel name.

- se:

  A SummarizedExperiment object containing the current dataset.

- pObjects:

  An environment containing global parameters generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

- rObjects:

  A reactive list of values generated in the
  [`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md) app.

## Value

The output of running
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
for the current panel. Several fields in `pObjects` are also modified as
a side-effect.

## Details

This function should be used in the rendering expression in
[`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md).
It takes care of a number of house-keeping tasks required to satisfy
[`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md)'s
requirements, e.g., responding to
[`.requestUpdate`](https://isee.github.io/iSEE/reference/requestUpdate.md)
modifications to `rObjects`, setting the `commands` and `contents` and
`varname` in `pObjects`.

This function will attempt to retrieve the cached output of
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
if it was used elsewhere in the app. After retrieval, the cached value
is wiped to ensure that it does not go stale. If no cached value is
found,
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md)
is called directly.

## See also

[`.renderOutput`](https://isee.github.io/iSEE/reference/output-generics.md),
where this function should be called.

[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md),
which is called by this function.

## Author

Aaron Lun
