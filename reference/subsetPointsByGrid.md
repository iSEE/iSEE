# Subset points for faster plotting

Subset points using a grid-based system, to avoid unnecessary rendering
when plotting.

## Usage

``` r
subsetPointsByGrid(X, Y, resolution = 200, grouping = NULL)
```

## Arguments

- X:

  A numeric vector of x-coordinates for all points.

- Y:

  A numeric vector of y-coordinates for all points, of the same length
  as `X`.

- resolution:

  A positive integer specifying the number of bins on each axis of the
  grid.

  Alternatively, if `grouping` is specified, this may be a named integer
  vector containing the number of bins to be used for each level.

- grouping:

  A character vector of length equal to `X` specifying the group to
  which each point is assigned. By default, all points belong to the
  same group.

## Value

A logical vector indicating which points should be retained.

## Details

This function will define a grid of the specified resolution across the
plot. Each point is allocated to a grid location (i.e., pair of bins on
the x- and y-axes). If multiple points are allocated to a given
location, only the last/right-most point is retained. This mimics the
fact that plotting will overwrite earlier points with later points. In
this manner, we can avoid unnecessary rendering of earlier points that
would not show up anyway.

If `grouping` is specified, redundant points are only identified within
each unique level. The resolution of downsampling within each level can
be varied by passing an integer vector to `resolution`. This can be
useful for tuning the downsampling when points differ in importance,
e.g., in a MA plot, points corresponding to non-DE genes can be
aggressively downsampled while points corresponding to DE genes should
generally be retained.

For plots where `X` and `Y` are originally categorical, use the jittered
versions as input to this function.

## Author

Aaron Lun

## Examples

``` r
X <- rnorm(100000)
Y <- X + rnorm(100000)

summary(subsetPointsByGrid(X, Y, resolution=100))
#>    Mode   FALSE    TRUE 
#> logical   96501    3499 

summary(subsetPointsByGrid(X, Y, resolution=200))
#>    Mode   FALSE    TRUE 
#> logical   89516   10484 

summary(subsetPointsByGrid(X, Y, resolution=1000))
#>    Mode   FALSE    TRUE 
#> logical   29680   70320 
```
