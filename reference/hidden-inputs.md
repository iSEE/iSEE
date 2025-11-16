# Hidden interface elements

Returns an interface element with or without the CSS class
`shinyjs-hide`, depending on whether the element is hidden based on
[`.hideInterface`](https://isee.github.io/iSEE/reference/interface-generics.md).
This allows panels to hide interface elements that are provided by
parent classes but are not needed in the subclass.

## Usage

``` r
.selectInputHidden(x, field, ...)
```

## Arguments

- x:

  An instance of a
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- field:

  String specifying the name of the suffix of the ID of the interface
  element.

- ...:

  Further arguments to pass to the Shiny function responsible for
  producing the interface element.

## Value

The output of `FUN(id, ..)` is returned where `id` is defined by
concatenating
[`.getEncodedName`](https://isee.github.io/iSEE/reference/getEncodedName.md)`(x)`
and `field` (separated by an underscore).

## Details

`.selectInputHidden(x, field, ...)` produces a Shiny
[`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html) element
that will be hidden if `.hideInterface(x)` is `TRUE`.

## Author

Kevin Rue-Albrecht

## Examples

``` r
.selectInputHidden(ComplexHeatmapPlot(), "SelectionHistory",
  choices = c(1, 2, 3), label = "Saved selection (hidden)")
#> <div class="form-group shiny-input-container shinyjs-hide">
#>   <label class="control-label" id="ComplexHeatmapPlotNA_SelectionHistory-label" for="ComplexHeatmapPlotNA_SelectionHistory">Saved selection (hidden)</label>
#>   <div>
#>     <select id="ComplexHeatmapPlotNA_SelectionHistory" class="shiny-input-select"><option value="1" selected>1</option>
#> <option value="2">2</option>
#> <option value="3">3</option></select>
#>     <script type="application/json" data-for="ComplexHeatmapPlotNA_SelectionHistory" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>   </div>
#> </div>
  
.selectInputHidden(ReducedDimensionPlot(), "Type",
  choices = c("UMAP", "PCA"), label = "Reduced dimension type (hidden)")
#> <div class="form-group shiny-input-container">
#>   <label class="control-label" id="ReducedDimensionPlotNA_Type-label" for="ReducedDimensionPlotNA_Type">Reduced dimension type (hidden)</label>
#>   <div>
#>     <select id="ReducedDimensionPlotNA_Type" class="shiny-input-select"><option value="UMAP" selected>UMAP</option>
#> <option value="PCA">PCA</option></select>
#>     <script type="application/json" data-for="ReducedDimensionPlotNA_Type" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>   </div>
#> </div>
```
