# Get panel colors

Functions to get/set panel colors at the user and developer level. This
determines the color of the panel header as well as (for
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s) the
color and fill of the brush.

## Usage

``` r
.panelColor(x)

.getPanelColor(x)
```

## Arguments

- x:

  An instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

## Value

A string containing the color assigned to the class of `x`.

## Details

For developers: `.panelColor` is a method that should be subclassed for
each [Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
subclass. This determines the color theme for all instances of that
class for use in, e.g., headers and box shadings. Developers should
choose a color that is dark enough to serve as a background for white
text. We recommend defining colors as hex color codes for full
compatibility with both HTML elements and R plots.

For users: by default, `.getPanelColor` will return the default color of
each panel as specified by the developer in `.panelColor`. However,
users can override this by setting the `panel.color` global option to a
named character vector of colors (see Examples). This can be used to
customize the color scheme for any given call to
[`iSEE`](https://isee.github.io/iSEE/reference/iSEE.md). The names of
the vector should be set to the name of class to be overridden; if a
class is not named here, its default color is used.

## Author

Aaron Lun

## Examples

``` r
rdp <- ReducedDimensionPlot()

# Default color, as specified by the developer:
.panelColor(rdp)
#> [1] "#3565AA"

# Still the default color:
.getPanelColor(rdp)
#> [1] "#3565AA"

# Overriding the default colors:
sce <- SingleCellExperiment(list(logcounts=matrix(rnorm(1000), ncol=100)))
reducedDim(sce, "PCA") <- matrix(runif(200), ncol=2)

sce <- registerAppOptions(sce, panel.color=c(ReducedDimensionPlot="#1e90ff"))
if (interactive()) {
    iSEE(sce, initial=list(rdp))
}
```
