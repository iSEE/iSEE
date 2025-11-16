# Sharing information across iSEE panels

**Compiled date**: 2025-11-16

**Last edited**: 2020-04-20

**License**: MIT + file LICENSE

## Introduction

One of *[iSEE](https://bioconductor.org/packages/3.23/iSEE)*’s main
features is the ability to share information between panels. This
facilitates deeper exploration of a dataset by allowing users to
visualize relationships across multiple metrics.
[`iSEE()`](https://isee.github.io/iSEE/reference/iSEE.md) currently
supports three modes of information sharing between panels - multiple
selections, single selections and dynamic selections - which are
demonstrated in this vignette using the Allen dataset processed in the
[previous
vignette](https://bioconductor.org/packages/3.23/iSEE/vignettes/basic.html).

## Multiple selections

### Basic use

As its name suggests, this involves selecting multiple features or
samples in one panel and transmitting their identities to another panel
to affect the visualization. To demonstrate, we will create a small app
involving a single reduced dimension plot and a column metadata plot.

``` r

library(iSEE)
app <- iSEE(sce, initial=list(
    ReducedDimensionPlot(),
    ColumnDataPlot()
))
```

![](screenshots/links-multi-naive.png)

We indicate that we want one panel to “receive” a multiple selection
from another panel. This is done by specifying the selection source (for
samples/columns in this case, given the nature of the two panels) and
indicating that the column data plot is to receive a selection from the
reduced dimension plot.

![](screenshots/links-multi-unselected.png)

We can click-and-drag to select multiple points in the reduced dimension
plot with a brush, which highlights those same points in the column data
plot. This enables users to easily explore relationships between
different visualizations in the
[`iSEE()`](https://isee.github.io/iSEE/reference/iSEE.md) interface.

![](screenshots/links-multi-brushed.png)

Alternatively, a single click will lay down lasso waypoints for a
non-rectangular selection. Once closed, the points in the lasso will be
transmitted to the column data plot.

![](screenshots/links-multi-lasso.png)

### Selection effects

Transparency is the default aesthetic effect used to distinguish points
in multiple selections in receiving plots. Alternatively, we may use
color:

![](screenshots/links-multi-color.png)

Another option is to restrict the receiving plot so that it only shows
the points in the multiple selection. This effectively “gates” the
dataset on the selection in the reduced dimension plot, analogous to
identification populations of interest in flow cytometry studies.
Indeed, this gating process can be repeated as many times as desired; a
multiple selection could made on the column data plot and transmitted to
another panel, and so on. (See [this mass cytometry
tour](https://github.com/iSEE/iSEE2018/blob/master/tours/cytof/app.R)
for an example.)

![](screenshots/links-multi-restrict.png)

Of course, not all receiving panels need to be plots. If we transmit a
multiple selection to a column data table, the effect is to subset the
rows of the table corresponding to the selected points.

![](screenshots/links-multi-table.png)

As an aside, it is equally possible for a table to transmit a multiple
selection to other panels. This is achieved using the search fields to
subset the dataset to the desired selection.

![](screenshots/links-multi-table2.png)

### Saving selections

In certain panels (usually plots), multiple selections can be saved to
form disjoint selections. Receiving plots will respond to the union of
all active and saved selections:

![](screenshots/links-multi-saved.png)

The saved selection history operates on a first-in-last-out basis. Upon
saving, a snapshot is taken of the current “active” selection, i.e., the
brush or lasso that is just created. Deletion will only operate on the
last saved selection.

## Single selections

Another mode of information sharing involves transmitting a selection of
a single feature or sample. This allows users to conveniently direct
other panels to focus on a feature or sample of interest. For example,
we can transmit a single selection from a row data table to a reduced
dimension plot, instructing the latter to color points by the expression
of the chosen feature.

![](screenshots/links-single-color.png)

The same approach can be used to control what is plotted on a feature
assay plot. Clicking on a different row of the table will directly
change the axes (in this case, the y-axis) of the plot, allowing the
user to synchronise different aspects of the
[`iSEE()`](https://isee.github.io/iSEE/reference/iSEE.md) interface to
whatever is currently of interest.

![](screenshots/links-single-yaxis.png)

We can also perform single selections on sample identities. In the
example below, the reduced dimension plot highlights the location of the
sample chosen in the column data table. This is useful for checking the
behavior of specific samples of interest, e.g., during quality control.

![](screenshots/links-single-sample.png)

Furthermore, it is possible to transmit single selections from a plot
using brushes or lassos. If the brush/lasso contains multiple points,
one of them is arbitrarily chosen for the purposes of obtaining a single
selection. Below, we select a single highly variable gene to examine its
distribution of expression values across the reduced dimension plot.

![](screenshots/links-single-plot2plot.png)

## Dynamic selections

Most panels will have an option to dynamically change the choice of
transmitting panel according to the last active selection in the app.
This allows users to, for example, simply brush on any plot in the app
and have all participating receiving panels immediately use that
selection without requiring manual resetting of the transmitter. To
illustrate, let’s set up an instance with three different plots that all
represent points as samples.

![](screenshots/links-dynamic-multi.png)

Notice how we have checked the dynamic source selection option in the
selection parameter box for all panels. This means that, upon making a
selection in any one of the plots, all of the other plots will
automatically respond as if they had been manually set to receive a
transmission from that plot.

![](screenshots/links-dynamic-multi2.png)

The same logic applies for some parameters that respond to single
selections. This allows a panel to respond to an appropriate single
selection from any other panel in the interface, without requiring the
user to manually set the relationship between panels. In the example
below, users can easily define the y-axis of the feature assay plot from
a selected row in the row data table or from a selected single point in
the row data plot.

![](screenshots/links-dynamic-single.png)

## Session Info

``` r

sessionInfo()
#> R Under development (unstable) (2025-11-12 r89009)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats4    stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] iSEE_2.23.1                 SingleCellExperiment_1.33.0
#>  [3] SummarizedExperiment_1.41.0 Biobase_2.71.0             
#>  [5] GenomicRanges_1.63.0        Seqinfo_1.1.0              
#>  [7] IRanges_2.45.0              S4Vectors_0.49.0           
#>  [9] BiocGenerics_0.57.0         generics_0.1.4             
#> [11] MatrixGenerics_1.23.0       matrixStats_1.5.0          
#> [13] BiocStyle_2.39.0           
#> 
#> loaded via a namespace (and not attached):
#>  [1] rlang_1.1.6           magrittr_2.0.4        shinydashboard_0.7.3 
#>  [4] clue_0.3-66           GetoptLong_1.0.5      otel_0.2.0           
#>  [7] compiler_4.6.0        mgcv_1.9-4            png_0.1-8            
#> [10] systemfonts_1.3.1     vctrs_0.6.5           pkgconfig_2.0.3      
#> [13] shape_1.4.6.1         crayon_1.5.3          fastmap_1.2.0        
#> [16] XVector_0.51.0        fontawesome_0.5.3     promises_1.5.0       
#> [19] rmarkdown_2.30        shinyAce_0.4.4        ragg_1.5.0           
#> [22] xfun_0.54             cachem_1.1.0          jsonlite_2.0.0       
#> [25] listviewer_4.0.0      later_1.4.4           DelayedArray_0.37.0  
#> [28] parallel_4.6.0        cluster_2.1.8.1       R6_2.6.1             
#> [31] bslib_0.9.0           RColorBrewer_1.1-3    jquerylib_0.1.4      
#> [34] Rcpp_1.1.0            bookdown_0.45         iterators_1.0.14     
#> [37] knitr_1.50            httpuv_1.6.16         Matrix_1.7-4         
#> [40] splines_4.6.0         igraph_2.2.1          tidyselect_1.2.1     
#> [43] abind_1.4-8           yaml_2.3.10           doParallel_1.0.17    
#> [46] codetools_0.2-20      miniUI_0.1.2          lattice_0.22-7       
#> [49] tibble_3.3.0          shiny_1.11.1          S7_0.2.1             
#> [52] evaluate_1.0.5        desc_1.4.3            circlize_0.4.16      
#> [55] pillar_1.11.1         BiocManager_1.30.27   DT_0.34.0            
#> [58] foreach_1.5.2         shinyjs_2.1.0         ggplot2_4.0.1        
#> [61] scales_1.4.0          xtable_1.8-4          glue_1.8.0           
#> [64] tools_4.6.0           colourpicker_1.3.0    fs_1.6.6             
#> [67] grid_4.6.0            colorspace_2.1-2      nlme_3.1-168         
#> [70] vipor_0.4.7           cli_3.6.5             textshaping_1.0.4    
#> [73] viridisLite_0.4.2     S4Arrays_1.11.0       ComplexHeatmap_2.27.0
#> [76] dplyr_1.1.4           gtable_0.3.6          rintrojs_0.3.4       
#> [79] sass_0.4.10           digest_0.6.38         SparseArray_1.11.1   
#> [82] ggrepel_0.9.6         rjson_0.2.23          htmlwidgets_1.6.4    
#> [85] farver_2.1.2          memoise_2.0.1         htmltools_0.5.8.1    
#> [88] pkgdown_2.2.0         lifecycle_1.0.4       shinyWidgets_0.9.0   
#> [91] GlobalOptions_0.1.2   mime_0.13
```
