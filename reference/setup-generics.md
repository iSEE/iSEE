# Generics for setting up parameters

These generics are related to the initial setup of the iSEE application.

## Caching common information

`.cacheCommonInfo(x, se)` computes common values that can be re-used for
all panels with the same class as `x`. The following arguments are
required:

- `x`, an instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- `se`, the SummarizedExperiment object containing the current dataset.

It is expected to return `se` with (optionally) extra fields added to
[`int_metadata`](https://rdrr.io/pkg/SingleCellExperiment/man/internals.html)`(se)$iSEE`.
Each field should be named according to the class name and contain some
common information that is constant for all instances of the class of
`x` - see
[`.setCachedCommonInfo`](https://isee.github.io/iSEE/reference/setCachedCommonInfo.md)
for an appropriate setter utility. The goal is to avoid repeated
recomputation of required values when creating user interface elements
or observers that respond to those elements.

Methods for this generic should start by checking whether the metadata
already contains the class name, and returning `se` without modification
if this is the case. Otherwise, it should
[`callNextMethod`](https://rdrr.io/r/methods/NextMethod.html) to fill in
the cache values from the parent classes, before adding cached values
under the class name for `x`. This means that any modification to `se`
will only be performed once per class, so any cached values should be
constant for all instances of the same class.

Values from the cache can also be
[`deparse`](https://rdrr.io/r/base/deparse.html)d and used to assemble
rendering commands in
[`.generateOutput`](https://isee.github.io/iSEE/reference/output-generics.md).
However, those same commands should not make any use of the cache
itself, i.e., they should not call
[`.getCachedCommonInfo`](https://isee.github.io/iSEE/reference/setCachedCommonInfo.md).
This is because the code tracker does not capture the code used to
construct the cache, so the commands that are shown to the user will
make use of a cache that is not present in the original `se` object.

## Refining parameters

`.refineParameters(x, se)` enforces appropriate settings for each
parameter in `x`. The following arguments are required:

- `x`, an instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- `se`, the SummarizedExperiment object containing the current dataset.

Methods for this generic should return a copy of `x` where slots with
invalid values are replaced with appropriate entries from `se`. This is
necessary because the constructor and validity methods for `x` do not
know about `se`; thus, certain slots (e.g., for the row/column names)
cannot be set to a reasonable default or checked by the validity method.
By comparison, `.refineParameters` can catch and correct invalid values
as it has access to `se`.

We recommend specializing
[`initialize`](https://rdrr.io/r/methods/new.html) to fill any
yet-to-be-determined slots with `NA` defaults. `.refineParameters` can
then be used to sweep across these slots and replace them with
appropriate entries, typically by using
[`.getCachedCommonInfo`](https://isee.github.io/iSEE/reference/setCachedCommonInfo.md)
to extract the cached set of potential valid values. Of course, any
slots that are not `se`-dependent should just be set at construction and
checked by the validity method.

It is also possible for this generic to return `NULL`, which is used as
an indicator that `se` does not contain information to meaningfully show
any instance of the class of `x` in the iSEE app. For example, the
method for
[ReducedDimensionPlot](https://isee.github.io/iSEE/reference/ReducedDimensionPlot-class.md)
will return `NULL` if `se` is not a
[SingleCellExperiment](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)
containing some dimensionality reduction results.

## Author

Aaron Lun
