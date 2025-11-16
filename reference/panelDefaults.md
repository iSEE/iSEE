# Panel defaults

Get or set default parameters that are used by certain
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) during
their construction. This allows users to easily change the defaults for
multiple panels simultaneously without having to manually specify them
in the constructor.

## Usage

``` r
panelDefaults(...)

getPanelDefault(name, error = TRUE)
```

## Arguments

- ...:

  Named options to set. Alternatively a single named list containing the
  options to set.

- name:

  String containing the name of the option to retrieve. Alternatively
  `NULL`, in which case the current values of all options are returned
  as a named list.

- error:

  Logical scalar indicating whether an error should be raised if `name`
  cannot be found.

## Value

`panelDefaults` will return a named list of the values of all options.
If `...` is non-empty, `panelDefaults` will modify the global options
that are used during the constructors for the relevant
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.
(Note that the return value still contains the values *before* the
modification is applied.)

`getPanelDefault` will return the current value of the requested option.
If `error=TRUE` and `name` is not present, an error is raised; otherwise
`NULL` is returned.

## Details

All options set by `panelDefaults` will only affect
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md)
construction and have no effect on the behavior of Panels that are
already constructed. Most options are named after the affected slot in
the relevant Panel subclass.

For general
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md)s:

- `PanelWidth`, defaults to 4.

- `PanelHeight`, defaults to 500.

For [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s:

- `ColorByDefaultColor`, defaults to `"black"`.

- `PointSize`, defaults to 1.

- `PointAlpha`, defaults to 1.

- `Downsample`, defaults to `FALSE`.

- `DownsampleResolution`, defaults to 200.

- `SelectionAlpha`, defaults to 0.1.

- `ContourColor`, defaults to `"blue"`.

- `FontSize`, defaults to 1.

- `LegendPointSize`, defaults to 1.

For
[RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s:

- `TooltipRowData`, defaults to `character(0)`.

For
[ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s:

- `TooltipColumnData`, defaults to `character(0)`.

For
[ComplexHeatmapPlot](https://isee.github.io/iSEE/reference/ComplexHeatmapPlot-class.md)s:

- `LegendDirection`, defaults to `"Horizontal"`.

A few options affect multiple subclasses that independently define the
same slot:

- `LegendPosition`, defaults to `"Bottom"`. Affects
  [DotPlot](https://isee.github.io/iSEE/reference/DotPlot-class.md)s and
  [ComplexHeatmapPlot](https://isee.github.io/iSEE/reference/ComplexHeatmapPlot-class.md)s.

- `Assay`, defaults to `"logcounts"`. Affects
  [FeatureAssayPlot](https://isee.github.io/iSEE/reference/FeatureAssayPlot-class.md)s,
  [SampleAssayPlot](https://isee.github.io/iSEE/reference/SampleAssayPlot-class.md)s
  and
  [ComplexHeatmapPlot](https://isee.github.io/iSEE/reference/ComplexHeatmapPlot-class.md)s.

A few options are not named after any particular slot as they affect
different slots in different subclasses:

- `ColorByNameColor`, defaults to `"red"`. This affects
  `ColorByFeatureNameColor` in
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s
  and `ColorBySampleNameColor` in
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s.

- `ColorByNameAssay`, defaults to `"logcounts"`. This affects
  `ColorByFeatureNameAssay` in
  [RowDotPlot](https://isee.github.io/iSEE/reference/RowDotPlot-class.md)s
  and `ColorBySampleNameAssay` in
  [ColumnDotPlot](https://isee.github.io/iSEE/reference/ColumnDotPlot-class.md)s.

- `SingleSelectionDynamicSource`, defaults to `FALSE`. This affects
  `ColorByFeatureDynamicSource`, `ColorBySampleDynamicSource`,
  `XAxisFeatureDynamicSource`, `YAxisFeatureDynamicSource`,
  `XAxisSampleDynamicSource` and `YAxisSampleDynamicSource` in the
  relevant panels.

- `MultipleSelectionDynamicSource`, defaults to `FALSE`. This affects
  `RowSelectionDynamicSource` and `ColumnSelectionDynamicSource`.

## For developers

Developers of Panel subclasses may add more options to this list,
typically by calling `panelDefaults` in the `.onLoad` expressions of the
package containing the subclass. We recommend prefixing any options with
the name of the package in the form of `<PACKAGE>_<OPTION>`, so as to
avoid conflicts with other options (in the base classes, or in other
downstream packages) that have the same name. Any options added in this
manner should correspond to parameters that are already present as slots
in the panel class. If this is not the case, consider using
[`registerAppOptions`](https://isee.github.io/iSEE/reference/registerAppOptions.md)
instead.

## Author

Kevin Rue-Albrecht

## Examples

``` r
old <- panelDefaults(Assay="WHEE")
getPanelDefault("Assay")
#> [1] "WHEE"

old <- panelDefaults(Assay="FOO", PointSize=5)
getPanelDefault("Assay")
#> [1] "FOO"
getPanelDefault("PointSize")
#> [1] 5

# We can also list out all options:
panelDefaults()

# Restoring the previous defaults.
panelDefaults(old)
getPanelDefault("Assay")
#> [1] "WHEE"
getPanelDefault("PointSize")
#> [1] 1
```
