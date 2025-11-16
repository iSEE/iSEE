# Add custom label plotting commands

Add [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
instructions to add custom labels to specified points in a
[DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md). This
is a utility function that is intended for use in
[`.generateDotPlot`](https://isee.github.io/iSEE/reference/plot-generics.md).

## Usage

``` r
.addCustomLabelsCommands(x, commands, plot_type)
```

## Arguments

- x:

  An instance of a
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)
  class.

- commands:

  A character vector representing the sequence of commands to create the
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

- plot_type:

  String specifying the type of plot, e.g., `"scatter"`, `"square"`,
  `"violin"`.

## Value

A character vector containing `commands` plus any additional commands
required to generate the labels.

## Author

Kevin Rue-Albrecht, Aaron Lun
