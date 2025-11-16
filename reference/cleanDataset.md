# Clean the dataset

Clean the SummarizedExperiment by making sure that names of various
fields are available and unique. Also transfer any information stored in
`rowRanges` into `rowData`.

## Usage

``` r
cleanDataset(se)

# S4 method for class 'SummarizedExperiment'
cleanDataset(se)

# S4 method for class 'SingleCellExperiment'
cleanDataset(se)
```

## Arguments

- se:

  A SummarizedExperiment object or one of its subclasses.

## Value

A cleaned version of `se`.

## Details

Various [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)s
assume that the row and column names of the input SummarizedExperiment
are available and unique. This function enforces that, adding
consecutive integer names if not available and calling
[`make.unique`](https://rdrr.io/r/base/make.unique.html) if they are
duplicated.

Various [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)s
further assume that the `assay`, `rowData`, `colData` names are unique;
if this is not the case, `selectInput` behaves in unexpected (and
incorrect) ways. This function enforces that as well by running them
through [`make.unique`](https://rdrr.io/r/base/make.unique.html).

Finally, positional information in `rowRanges` is not accessible to
`iSEE`. This function moves this information into `rowData`, prefixing
the column names with `rowRanges_`.

For
[SingleCellExperiment](https://rdrr.io/pkg/SingleCellExperiment/man/SingleCellExperiment.html)
object, we enforce uniqueness in the
[`reducedDims`](https://rdrr.io/pkg/SingleCellExperiment/man/reducedDims.html).

All changes result in warnings as a “sensible” object is not expected to
require any work.

## Author

Aaron Lun, Charlotte Soneson

## Examples

``` r
# Creating a very naughty SE.
se <- SummarizedExperiment(list(cbind(1:10, 2:11), cbind(2:11, 3:12)),
   colData=DataFrame(A=1:2, A=3:4, check.names=FALSE), 
   rowData=DataFrame(B=1:10, B=1:10, check.names=FALSE))
se
#> class: SummarizedExperiment 
#> dim: 10 2 
#> metadata(0):
#> assays(2): '' ''
#> rownames: NULL
#> rowData names(2): B B
#> colnames: NULL
#> colData names(2): A A

cleanDataset(se)
#> Warning: filling in the missing 'rownames(se)'
#> Warning: filling in the missing 'colnames(se)'
#> Warning: duplicated 'colnames(rowData(se))' detected, making them unique
#> Warning: duplicated 'colnames(colData(se))' detected, making them unique
#> Warning: empty 'assayNames(se)' detected, renaming to 'unnamed'
#> Warning: duplicated 'assayNames(se)' detected, making them unique
#> class: SummarizedExperiment 
#> dim: 10 2 
#> metadata(0):
#> assays(2): unnamed unnamed.1
#> rownames(10): 1 2 ... 9 10
#> rowData names(2): B B.1
#> colnames(2): 1 2
#> colData names(2): A A.1
```
