# Generate ggplot aesthetic instructions

Generate ggplot aesthetic instructions

## Usage

``` r
.buildAes(
  x = TRUE,
  y = TRUE,
  color = FALSE,
  shape = FALSE,
  size = FALSE,
  fill = FALSE,
  group = FALSE,
  alt = NULL
)
```

## Arguments

- x:

  A `logical` that indicates whether to enable `x` in the aesthetic
  instructions (default: `TRUE`).

- y:

  A `logical` that indicates whether to enable `y` in the aesthetic
  instructions (default: `TRUE`).

- color:

  A `logical` that indicates whether to enable `color` in the aesthetic
  instructions (default: `FALSE`).

- shape:

  A `logical` that indicates whether to enable `shape` in the aesthetic
  instructions (default: `FALSE`).

- size:

  A `logical` that indicates whether to enable `size` in the aesthetic
  instructions (default: `FALSE`).

- fill:

  A `logical` that indicates whether to enable `fill` in the aesthetic
  instructions (default: `FALSE`).

- group:

  A `logical` that indicates whether to enable `group` in the aesthetic
  instructions (default: `FALSE`).

- alt:

  Alternative aesthetics, supplied as a named character vector.

## Value

Aesthetic instructions for
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) as a
character value.

## Author

Kevin Rue-Albrecht

## Examples

``` r
.buildAes()
#> [1] "aes(x=X, y=Y)"
```
