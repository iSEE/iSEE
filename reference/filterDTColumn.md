# Filter DT columns

Filter a data.frame based on the DT `datatable` widget column search
string.

## Usage

``` r
filterDTColumn(x, search)

filterDT(df, column, global)
```

## Arguments

- x:

  A numeric or character vector, usually representing a column of a
  data.frame.

- search:

  A string specifying the search filter to apply to `x`.

- df:

  A data.frame that was used in the `datatable` widget.

- column:

  A character vector of per-column search strings to apply to `df`. If
  any entry is an empty string, the corresponding column is not used for
  any filtering.

- global:

  String containing a regular expression to search for across all
  columns in `df` (and row names, if present). If an empty string, no
  filtering is performed.

## Value

A logical vector indicating which entries of `x` or rows of `df` are to
be retained.

## Details

For character `x`, `search` is treated as a regular expression.

For numeric `x`, `search` should have the form `LOWER ... UPPER` where
all elements in \[LOWER, UPPER\] are retained.

For factor `x`, `search` should have the form
`["choice_1", "choice_2", etc.]`. This is also the case for logical `x`,
albeit with the only choices being `"true"` or `"false"`.

`filterDT` will retain all rows where (i) any value in any column (after
coercion to a string) matches `global`, and (ii) the value in each
column satisfies the filter specified in the corresponding entry of
`column`. Setting `global` to an empty string will skip requirement (i)
while setting any entry of `column` to an empty string will skip
requirement (ii) for the affected column.

Ideally, `ncol(df)` and `length(searches)` would be the same, but if
not, `filterDT` will simply filter on the first N entries where N is the
smaller of the two.

Any `NA` element in `x` will be treated as a no-match. The same applies
for each column of `df` that has non-empty `column`. Note that a
no-match in one column does not preclude a successful match in another
column by `global`.

## See also

`datatable` and associated documentation for more details about column
searches.

## Author

Aaron Lun

## Examples

``` r
# Regular expression:
filterDTColumn(LETTERS, "A|B|C")
#>  [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE

# Range query:
filterDTColumn(runif(20), "0.1 ... 0.5")
#>  [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
#> [13]  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE

# Factor query:
filterDTColumn(factor(letters), "['a', 'b', 'c']")
#>  [1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE

# Works on DataFrames:
X <- data.frame(row.names=LETTERS, thing=runif(26), 
    stuff=sample(letters[1:3], 26, replace=TRUE),
    stringsAsFactors=FALSE)

filterDT(X, c("0 ... 0.5", "a|b"), global="")
#>  [1] FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE
#> [13] FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE
filterDT(X, "", global="A")
#>  [1]  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [25] FALSE FALSE
```
