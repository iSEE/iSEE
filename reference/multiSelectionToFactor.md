# Convert multiple selections into a factor

Convert multiple selection information into a factor, typically for use
as a covariate or for coloring.

## Usage

``` r
multiSelectionToFactor(selected, all.names)
```

## Arguments

- selected:

  A named list of character vectors, containing the names of selected
  observations for different selections. Vectors for different
  selections may overlap.

- all.names:

  Character vector of all observations.

## Value

A factor containing the set(s) to which each observation is assigned.
Multiple sets are encoded as comma-separated strings. Unselected
observations are listed as `"unselected"`.

## Author

Aaron Lun

## Examples

``` r
multiSelectionToFactor(list(active=c("A", "B"), 
    saved1=c("B", "C"), saved2=c("D", "E", "F")),
    all.names=LETTERS[1:10])
#>             A             B             C             D             E 
#>        active active,saved1        saved1        saved2        saved2 
#>             F             G             H             I             J 
#>        saved2    unselected    unselected    unselected    unselected 
#> Levels: active active,saved1 saved1 saved2 unselected
```
