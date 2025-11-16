# Create custom panels

Helper functions for quick-and-dirty creation of custom panels, usually
in the context of a one-off application. This creates a new class with
specialized methods for showing content based on a user-specified
function.

## Usage

``` r
createCustomTable(
  FUN,
  restrict = NULL,
  className = "CustomTable",
  fullName = "Custom table",
  FUN.selection.details = NULL,
  where = topenv(parent.frame())
)

createCustomPlot(
  FUN,
  restrict = NULL,
  className = "CustomPlot",
  fullName = "Custom plot",
  where = topenv(parent.frame())
)
```

## Arguments

- FUN:

  A function that generates a data.frame or a
  [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html), for
  `createCustomTable` and `createCustomPlot` respectively. See Details
  for the expected arguments.

- restrict:

  Character vector of names of optional arguments in `FUN` to which the
  UI is restricted. If specified, only the listed arguments receive UI
  elements in the interface.

- className:

  String containing the name of the new
  [Panel](https://isee.github.io/iSEE/reference/Panel-class.md) class.

- fullName:

  String containing the full name of the new class.

- FUN.selection.details:

  Function generating a UI element that displays details about the
  current selection, if any.

- where:

  An environment indicating where the class and method definitions
  should be stored.

## Value

A new class and its methods are defined in the global environment. A
generator function for creating new instances of the class is returned.

## Details

`FUN` is expected to have the following first 3 arguments:

- `se`, a SummarizedExperiment object for the current dataset of
  interest.

- `rows`, a list of row selections received from the transmitting panel.
  This contains one or more character vectors of row names in active and
  saved selections. Alternatively, this may be `NULL` if no selection
  has been made in the transmitter.

- `columns`, a list of column selections received from the transmitting
  panel. This contains one or more character vectors of column names in
  active and saved selections. Alternatively, this may be `NULL` if no
  selection has been made in the transmitter.

Any number of additional named arguments may also be present in `FUN`.
All such arguments should have default values, as these are used to
automatically generate UI elements in the panel:

- Character vectors will get a
  [`selectInput`](https://rdrr.io/pkg/shiny/man/selectInput.html).

- Strings will get a
  [`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html).

- Numeric scalars will get a
  [`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html).

- Logical scalars will get a
  [`checkboxInput`](https://rdrr.io/pkg/shiny/man/checkboxInput.html).

Arguments with other types of default values are ignored. If `restrict`
is specified, arguments will only have corresponding UI elements if they
are listed in `restrict`. All user interactions with these elements will
automatically trigger regeneration of the panel contents.

Classes created via these functions are extremely limited. Only scalar
inputs are supported via the UI and all panels cannot transmit to the
rest of the app. We recommend only using these functions for one-off
applications to quickly prototype concepts; serious
[Panel](https://isee.github.io/iSEE/reference/Panel-class.md) extensions
should be done explicitly.

## Author

Aaron Lun

## Examples

``` r
library(scater)
CUSTOM_DIMRED <- function(se, rows, columns, ntop=500, scale=TRUE,
    mode=c("PCA", "TSNE", "UMAP"))
{
    if (is.null(columns)) {
        return(
            ggplot() + theme_void() + geom_text(
                aes(x, y, label=label),
                data.frame(x=0, y=0, label="No column data selected."),
                size=5)
            )
    }

    mode <- match.arg(mode)
    if (mode=="PCA") {
        calcFUN <- runPCA
    } else if (mode=="TSNE") {
        calcFUN <- runTSNE
    } else if (mode=="UMAP") {
        calcFUN <- runUMAP
    }

    kept <- se[, unique(unlist(columns))]
    kept <- calcFUN(kept, ncomponents=2, ntop=ntop,
        scale=scale, subset_row=unique(unlist(rows)))
    plotReducedDim(kept, mode)
}

GEN <- createCustomPlot(CUSTOM_DIMRED)
GEN()
#> Panel object of class CustomPlot
#>   Get or set individual parameters with '[[' 
#>   Available parameters:
#>     ColumnSelectionDynamicSource: FALSE
#>     ColumnSelectionRestrict: FALSE
#>     ColumnSelectionSource: ---
#>     DataBoxOpen: FALSE
#>     PanelHeight: 500
#>     PanelId: NA
#>     PanelWidth: 4
#>     RowSelectionDynamicSource: FALSE
#>     RowSelectionRestrict: FALSE
#>     RowSelectionSource: ---
#>     SelectionBoxOpen: FALSE
#>     SelectionHistory: 
#>     VersionInfo: list of length 1
#>     mode: PCA
#>     ntop: 500
#>     scale: TRUE

if (interactive()) {
    library(scRNAseq)
    sce <- ReprocessedAllenData("tophat_counts")
    library(scater)
    sce <- logNormCounts(sce, exprs_values="tophat_counts")

    iSEE(sce, initial=list(
        ColumnDataPlot(PanelId=1L),
        GEN(ColumnSelectionSource="ColumnDataPlot1")
    ))
}
```
