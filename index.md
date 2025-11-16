# *iSEE* - The interactive SummarizedExperiment Explorer

## Bioconductor release status

| Branch | R CMD check | Last updated |
|:--:|:--:|:--:|
| [*devel*](http://bioconductor.org/packages/devel/bioc/html/iSEE.md) | [![Bioconductor-devel Build Status](http://bioconductor.org/shields/build/devel/bioc/iSEE.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/iSEE) | ![](http://bioconductor.org/shields/lastcommit/devel/bioc/iSEE.svg) |
| [*release*](http://bioconductor.org/packages/release/bioc/html/iSEE.md) | [![Bioconductor-release Build Status](http://bioconductor.org/shields/build/release/bioc/iSEE.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/iSEE) | ![](http://bioconductor.org/shields/lastcommit/release/bioc/iSEE.svg) |

## Overview

The *iSEE* package provides an interactive user interface for exploring
data in objects derived from the `SummarizedExperiment` class.
Particular focus is given to single-cell data stored in the
`SingleCellExperiment` derived class. The user interface is implemented
with [RStudio](https://www.rstudio.com)’s
[*Shiny*](https://shiny.rstudio.com), with a multi-panel setup for ease
of navigation.

This initiative was proposed at the European Bioconductor Meeting in
Cambridge, 2017. Current contributors include:

- [Charlotte Soneson](https://github.com/csoneson)
- [Aaron Lun](https://github.com/LTLA)
- [Federico Marini](https://github.com/federicomarini)
- [Kévin Rue-Albrecht](https://github.com/kevinrue)

[![Figure 1. iSEE uses a customisable multi-panel
layout.](https://f1000researchdata.s3.amazonaws.com/manuscripts/16293/6bf85f9d-8352-4a78-a8da-456f05f5c4c9_figure1.gif "iSEE uses a customisable multi-panel layout")](https://f1000research.com/articles/7-741/v1)

## Installation

*iSEE* can be easily installed from Bioconductor using
[`BiocManager::install()`](https://bioconductor.github.io/BiocManager/reference/install.html):

``` r

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("iSEE")
# or also...
BiocManager::install("iSEE", dependencies = TRUE)
```

Setting `dependencies = TRUE` should ensure that all packages, including
the ones in the `Suggests:` field of the `DESCRIPTION`, are installed -
this can be essential if you want to reproduce the code in the vignette,
for example.

## Functionalities

**Click to expand the list of features available in *iSEE*
applications.**

### General

- Multiple interactive plot types with selectable points.

- Interactive tables with selectable rows.

- Coloring of samples and features by metadata or expression data.

- Zooming to a plot subregion.

- Transmission of point selections between panels to highlight, color,
  or restrict data points in the receiving panel(s).

- Lasso point selection to define complex shapes.

### Sample-level visualization

The *iSEE* user interface currently contains the following components
where each data point represents a single biological sample:

- **Reduced dimension plot**: Scatter plot of reduced dimensionality
  data.

- **Column data plot**: Adaptive plot of any one or two sample metadata.
  A scatter, violin, or square design is dynamically applied according
  to the continuous or discrete nature of the metadata.

- **Feature assay plot**: Adaptive plot of expression data across
  samples for any two features or one feature against one sample
  metadata.

- **Column data table**: Table of sample metadata.

### Feature-level visualization

The *iSEE* user interface currently contains the following components
where each data point represents a genomic feature:

- **Row data plot**: Adaptive plot of any two feature metadata. A
  scatter, violin, or square design is dynamically applied according to
  the continuous or discrete nature of the metadata.

- **Sample assay plot**: Adaptive plot of expression data across
  features for any two samples or one sample against one feature
  metadata.

- **Row data table**: Table of feature metadata.

### Integrated visualization

The *iSEE* user interface contains the following components that
integrate sample and feature information:

- **Complex heatmap plot**: Visualize multiple features across multiple
  samples annotated with sample metadata.

### Custom panels

The *iSEE* API allows users to programmatically define their own
plotting and table panels. See the section [Extending
*iSEE*](#extending-isee) further below.

### Miscellaneous

- The *iSEE* user interface continually tracks the code corresponding to
  all visible plotting panels. This code is rendered in a
  [shinyAce](https://cran.r-project.org/web/packages/shinyAce/index.html)
  text editor and can be copy-pasted into R scripts for customization
  and further use.

- Speech recognition can be enabled to control the user interface using
  voice commands.

## Want to try *iSEE*?

We set up instances of *iSEE* applications running on diverse types of
datasets at those addresses:

- <http://shiny.imbei.uni-mainz.de:3838/iSEE>
- <https://marionilab.cruk.cam.ac.uk/iSEE_allen>
- <https://marionilab.cruk.cam.ac.uk/iSEE_tcga>
- <https://marionilab.cruk.cam.ac.uk/iSEE_pbmc4k>
- <https://marionilab.cruk.cam.ac.uk/iSEE_cytof>

Please keep in mind that those public instances are for trial purposes
only; yet they demonstrate how you or your system administrator can
setup *iSEE* for analyzing or sharing your precomputed
`SummarizedExperiment`/`SingleCellExperiment` object.

## Extending *iSEE*

If you want to extend the functionality of *iSEE*, you can create custom
panels which add new possibilities to interact with your data. Custom
panels can be defined in independent R packages that include *iSEE* in
the `Imports:` sections of their DESCRIPTION file. You can find a
collection of working examples of how to do it in
[iSEEu](https://github.com/iSEE/iSEEu). Feel free to contact the
developing team, should you need some clarifications on how *iSEE* works
internally.

## Code of Conduct

Please note that the iSEE project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
