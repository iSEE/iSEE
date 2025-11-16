# Generate ggplot title and label instructions

Generate ggplot title and label instructions

## Usage

``` r
.buildLabs(
  x = NULL,
  y = NULL,
  color = NULL,
  shape = NULL,
  size = NULL,
  fill = NULL,
  group = NULL,
  title = NULL,
  subtitle = NULL
)
```

## Arguments

- x:

  The character label for the horizontal axis.

- y:

  x The character label for the vertical axis.

- color:

  The character title for the color scale legend.

- shape:

  The character title for the point shape legend.

- size:

  The character title for the point size legend.

- fill:

  The character title for the color fill legend.

- group:

  The character title for the group legend.

- title:

  The character title for the plot title.

- subtitle:

  The character title for the plot subtitle

## Value

Title and label instructions for
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) as a
character value.

## Details

If any argument is `NULL`, the corresponding label is not set.

## Author

Kevin Rue-Albrecht

## Examples

``` r
cat(.buildLabs(y = "Title for Y axis", color = "Color label"))
#> labs(y="Title for Y axis", color="Color label") +
```
