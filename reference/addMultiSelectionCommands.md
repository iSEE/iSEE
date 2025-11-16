# Add multiple selection plotting commands

Add [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
instructions to create brushes and lassos for both saved and active
multiple selections in a
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md) panel.

## Usage

``` r
.addMultiSelectionPlotCommands(x, envir, commands, flip = FALSE)
```

## Arguments

- x:

  An instance of a
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
  class.

- envir:

  The environment in which the
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) commands
  are to be evaluated.

- commands:

  A character vector representing the sequence of commands to create the
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

- flip:

  A logical scalar indicating whether the x- and y-axes are flipped,
  only relevant to horizontal violin plots.

## Value

A character vector containing `commands` plus any additional commands
required to draw the self selections.

## Details

This is a utility function that is intended for use in
[`.generateDotPlot`](https://isee.github.io/iSEE/reference/plot-generics.md).
It will modify `envir` by adding `all_active` and `all_saved` variables,
so developers should not use these names for their own variables in
`envir`.

If no self-selection structures exist in `x`, `commands` is returned
directly without modification.

## Author

Aaron Lun
