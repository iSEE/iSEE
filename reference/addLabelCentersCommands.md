# Add centered label plotting commands

Add [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
instructions to label the center of each group on a scatter plot. This
is a utility function that is intended for use in
[`.generateDotPlot`](https://isee.github.io/iSEE/reference/plot-generics.md).

## Usage

``` r
.addLabelCentersCommands(x, commands)
```

## Arguments

- x:

  An instance of a
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
  class.

- commands:

  A character vector representing the sequence of commands to create the
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Value

A character vector containing `commands` plus any additional commands
required to generate the labels.

## Author

Aaron Lun
