# A collapsible box

A custom collapsible box with Shiny inputs upon collapse, more or less
stolen from shinyBS.

## Usage

``` r
collapseBox(id, title, ..., open = FALSE, style = NULL)
```

## Arguments

- id:

  String specifying the identifier for this object, to use as a field of
  the Shiny input.

- title:

  String specifying the title of the box for use in the UI.

- ...:

  Additional UI elements to show inside the box.

- open:

  Logical scalar indicating whether this box should be open upon
  initialization.

- style:

  String specifying the box style, defaults to `"default"`.

## Value

A HTML tag object containing a collapsible box.

## Details

Collapsible boxes are used to hold parameters in the “parameter boxes”
described in
[`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md).
It is recommended to format the `id` as `PANEL_SLOT` where `PANEL` is
the name of the panel associated with the box and `SLOT` is the name of
the slot that specifies whether this box should be open or not at
initialization. (See
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) for some
examples with `DataBoxOpen`.)

Do not confuse these boxes with the
[`shinydashboard::box`](https://rdrr.io/pkg/shinydashboard/man/box.html)es,
which are used to hold the plot and table panels. Adding to the
nomenclature confusion is the fact that our collapsible boxes are
implemented in Javascript using the Bootstrap “panel” classes, which in
turn has nothing to do with our
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) classes.

## Comments on shinyBS

We would have preferred to use `bsCollapse` from shinyBS. However, that
package does not seem to be under active maintenance, and there are
several aspects that make it difficult to use. Specifically, it does not
seem to behave well with conditional elements inside the box, and it
also does needs a `Depends:` relationship with shinyBS.

For these reasons, we created our own collapsible box, taking code from
`shinyBS` where appropriate. The underlying Javascript code for this
object is present in `inst/www` and is attached to the search path for
Shiny resources upon loading iSEE.

## See also

shinyBS, from which the Javascript code was derived.

[`.defineInterface`](https://isee.github.io/iSEE/reference/interface-generics.md),
which should return a list of these collapsible boxes.

## Author

Aaron Lun

## Examples

``` r
library(shiny)
collapseBox("SomePanelType1_ParamBoxOpen",
    title="Custom parameters",
    open=FALSE,
    selectInput("SomePanelType1_Thing",
        label="What thing?",
        choices=LETTERS, selected="A"
    )
)
#> <div class="isee-collapse-box panel panel-default" id="SomePanelType1_ParamBoxOpen" value="Custom parameters">
#>   <div class="panel-heading" role="tab" id="heading_collapse_SomePanelType1_ParamBoxOpen">
#>     <h4 class="panel-title">
#>       <a data-toggle="collapse" href="#collapse_SomePanelType1_ParamBoxOpen">Custom parameters</a>
#>     </h4>
#>   </div>
#>   <div id="collapse_SomePanelType1_ParamBoxOpen" class="panel-collapse collapse" role="tabpanel">
#>     <div class="panel-body">
#>       <div class="form-group shiny-input-container">
#>         <label class="control-label" id="SomePanelType1_Thing-label" for="SomePanelType1_Thing">What thing?</label>
#>         <div>
#>           <select id="SomePanelType1_Thing" class="shiny-input-select"><option value="A" selected>A</option>
#> <option value="B">B</option>
#> <option value="C">C</option>
#> <option value="D">D</option>
#> <option value="E">E</option>
#> <option value="F">F</option>
#> <option value="G">G</option>
#> <option value="H">H</option>
#> <option value="I">I</option>
#> <option value="J">J</option>
#> <option value="K">K</option>
#> <option value="L">L</option>
#> <option value="M">M</option>
#> <option value="N">N</option>
#> <option value="O">O</option>
#> <option value="P">P</option>
#> <option value="Q">Q</option>
#> <option value="R">R</option>
#> <option value="S">S</option>
#> <option value="T">T</option>
#> <option value="U">U</option>
#> <option value="V">V</option>
#> <option value="W">W</option>
#> <option value="X">X</option>
#> <option value="Y">Y</option>
#> <option value="Z">Z</option></select>
#>           <script type="application/json" data-for="SomePanelType1_Thing" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
#>         </div>
#>       </div>
#>     </div>
#>   </div>
#> </div>
```
