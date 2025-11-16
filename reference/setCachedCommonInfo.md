# Set and get cached commons

Get and set common cached information for each class. The setter should
only ever be called in
[`.cacheCommonInfo`](https://isee.github.io/iSEE/reference/setup-generics.md).
The getter can be called anywhere but most usually in
[`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md).

## Usage

``` r
.setCachedCommonInfo(se, cls, ...)

.getCachedCommonInfo(se, cls)
```

## Arguments

- se:

  A SummarizedExperiment object containing the current dataset.

- cls:

  String containing the name of the class for which this information is
  cached.

- ...:

  Any number of named R objects to cache.

## Value

`.setCachedCommonInfo` returns `se` with `...` added to its
[`int_metadata`](https://rdrr.io/pkg/SingleCellExperiment/man/internals.html).

`.getCachedCommonInfo` retrieves the cached common information for class
`cls`.

## See also

`?"`[`cache-utils`](https://isee.github.io/iSEE/reference/cache-utils.md)`"`,
for utilities to define some cached variables.

## Author

Aaron Lun

## Examples

``` r
se <- SummarizedExperiment()
se <- .setCachedCommonInfo(se, "SomePanelClass",
    something=1, more_things=TRUE, something_else="A")
.getCachedCommonInfo(se, "SomePanelClass")
#> $something
#> [1] 1
#> 
#> $more_things
#> [1] TRUE
#> 
#> $something_else
#> [1] "A"
#> 
```
