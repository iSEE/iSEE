# Global iSEE options

Get or set global values that are used by relevant panels during
construction and application initialization. This has been deprecated in
favor of
[`panelDefaults`](https://isee.github.io/iSEE/reference/panelDefaults.md)
(for options that apply during
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
construction) and
[`registerAppOptions`](https://isee.github.io/iSEE/reference/registerAppOptions.md)
(for options that apply during application runtime).

## Usage

``` r
iSEEOptions
```

## Commands

`str(iSEEOptions$get())` will show the default values for all options.

`iSEEOptions$set(name=value)` will set the `name`d option to `value`.

`iSEEOptions$restore()` will reset the global options to the package
default values.

## Available options

- `point.color`:

  Default color of data points in `DotPlot` panels (character).

- `point.size`:

  Default size of data points in `DotPlot` panels (numeric).

- `point.alpha`:

  Default alpha level controlling transparency of data points in
  `DotPlot` panels (numeric).

- `downsample`:

  Enable visual downsampling in `DotPlot` panels (logical).

- `downsample.resolution`:

  Resolution of the visual downsampling, if active (numeric).

- `selected.color`:

  Color of selected data points in `DotPlot` panels (character).

- `selected.alpha`:

  Alpha level controlling transparency of data points *not* selected in
  `DotPlot` panels (numeric).

- `selection.dynamic.single`:

  Toggle dynamic single selections for all panels (logical).

- `selection.dynamic.multiple`:

  Toggle dynamic multiple selections for all panels (logical).

- `contour.color`:

  Color of the 2d density estimation contour in `DotPlot` panels
  (character).

- `font.size`:

  Global multiplier controlling the magnification of plot title and text
  elements in `DotPlot` panels (numeric).

- `legend.position`:

  Position of the legend in `DotPlot` and `ComplexHeatmapPlot` panels
  (one of `"Bottom"`, `"Right"`, or `"None"`).

- `legend.direction`:

  Position of the legend in `DotPlot` and `ComplexHeatmapPlot` panels
  (one of `"Horizontal"`, `"Vertical"`).

- `panel.width`:

  Default panel grid width (must be between 1 and 12).

- `panel.height`:

  Default panel height (in pixels).

- `panel.color`:

  Named character vector of colors. The names of the vector should be
  set to the name of class to be overridden; if a class is not named
  here, its default color is used. It is highly recommended to define
  colors as hex color codes (e.g., `"#1e90ff"`), for full compatibility
  with both HTML elements and R plots.

- `color.maxlevels`:

  Maximum number of levels for a categorical variable used for coloring.
  Variables with more levels are coerced to numeric to avoid problems
  with an overly-large legend. Defaults to 24.

- `factor.maxlevels`:

  Maximum number of levels for a categorical variable to be used
  anywhere in the app. Variables with more levels are coerced to numeric
  to avoid rendering delays. Defaults to 100.

- `assay`:

  Character vector of assay names to use if available, in order of
  preference.

- `RowTable.select.details`:

  A function that takes a string containing the name of a feature (i.e.,
  the current selection in the
  [RowTable](https://isee.github.io/iSEE/reference/RowTable-class.md))
  and returns a HTML element with more details.

- `ColumnTable.select.details`:

  A function that takes a string containing the name of a sample (i.e.,
  the current selection in the
  [ColumnTable](https://isee.github.io/iSEE/reference/ColumnTable-class.md))
  and returns a HTML element with more details.

## Author

Kevin Rue-Albrecht

## Examples

``` r
iSEEOptions$get('downsample'); iSEEOptions$get('selected.color')
#> NULL
#> NULL
```
