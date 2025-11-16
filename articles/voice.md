# Controlling the iSEE interface using speech recognition

**Compiled date**: 2025-11-16

**Last edited**: 2018-11-29

**License**: MIT + file LICENSE

## Feature

Using JavaScript, `iSEE` applications can leverage lightweight speech
recognition libraries that react to specific vocal commands (think “OK
Google”, “Hey Siri”) and trigger updates of the UI equivalent to one or
more mouse or keyboard interaction with the UI components (Rue-Albrecht
et al. 2018).

**Note**: As we value privacy, this feature is disabled by default:
`iSEE(..., voice=FALSE)`.

To keep the spoken commands reasonably short, only one panel may be
under voice command at any one time. All spoken commands will affect the
currently active panel, until a new panel is selected for voice command.
See section [Vocal commands available](#availableVocalCommands).

## Implementation

We use the [*annyang*](https://github.com/TalAter/annyang) lightweight
JavaScript library to handle speech recognition and update *Shiny*
reactive values in the same way as mouse and keyboard UI elements
trigger panel updates.

Note that *annyang* requires an active internet connection, as it relies
on the browser’s own speech recognition engine (see the *annyang*
[FAQ](https://github.com/TalAter/annyang/blob/master/docs/FAQ.md#can-annyang-work-offline)).
For instance, in *Google Chrome*, this engine performs the recognition
in the cloud.

## Supported web browsers

Note that the speech recognition library that we use does not work with
every web browser. We currently only validated this feature in *Google
Chrome*. Please refer to the *annyang*
[FAQ](https://github.com/TalAter/annyang/blob/master/docs/FAQ.md#which-browsers-are-supported)
for details.

## Usage

Using the `sce` object that we generated
[earlier](https://bioconductor.org/packages/3.23/iSEE/vignettes/basic.html),
enabling speech recognition is as simple as setting `voice=TRUE` below:

``` r

library(iSEE)
app <- iSEE(sce, voice=TRUE)
```

With `voice=TRUE`, the lightweight JavaScript speech recognition library
*annyang* is loaded and activated in any web browser tab that runs
`app`.

If your default browser is not compatible with the feature, or if you
work in *RStudio*, you can prevent the application from opening in the
default browser by setting `launch.browser=FALSE` as follows:

``` r

if (interactive()) {
    shiny::runApp(app, port=1234, launch.browser=FALSE)
}
```

At that point, your R console should be displaying the address and port
where `app` is running. In the example above, that would be:

    Listening on http://127.0.0.1:1234

Using a compatible browser, navigate to the indicated address and port.
Note that when the web page opens, you may be prompted to allow the web
browser to use your microphone, which you must accept to enable the
functionality.

## Vocal commands available

As a proof of concept, only a subset of spoken commands are currently
implemented, compared to the full range of interactions possible using
the mouse and keyboard.

Note that in the commands below, words in brackets are optional.

- “**Show active panel**”: shows a persistent notification displaying
  the name of the panel currently under vocal control.
- “**Create** ”: Adds a new panel of the requested type to the GUI and
  immediately takes vocal control of it.
- “**Remove \<Reduced dimension plot 1\>**”: Removes the requested panel
  from the GUI. If the panel was under vocal control, clears vocal
  control.
- “**Control \<Reduced dimension plot 1\>**”: Takes vocal control of the
  requested panel.
- “**Colour using \<Column data \| Feature name \| …\>**”: Changes the
  colouring mode of the panel under vocal control.
- “**Colour by \<…\>**”: Changes the colouring covariate (e.g. gene
  name, `colData` column name) of the panel under vocal control.
- “**Receive selection from \<Reduced dimension plot 1\>**”: Makes the
  panel under vocal control receive the point selection from the
  requested panel.
- “**Send selection to \<Reduced dimension plot 1\>**”: Makes the
  requested panel receive the point selection from the panel under vocal
  control.
- “**Good \<boy \| girl\>!**”: If the app is behaving well, throw it a
  bone!

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
# devtools::session_info()
```

## References

Rue-Albrecht, K., F. Marini, C. Soneson, and A. T. L. Lun. 2018. “iSEE:
Interactive SummarizedExperiment Explorer.” *F1000Research* 7 (June):
741.
