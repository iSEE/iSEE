
# `iSEE` - interactive SummarizedExperiment Explorer

<!-- TODO logo here too :) --> 

## Software status

| Platforms |  OS  | R CMD check | Coverage | 
|:----------------:|:----------------:|:----------------:|:----------------:|
| Travis CI | Linux | [![Travis CI build status](https://travis-ci.org/csoneson/iSEE.svg?branch=master)](https://travis-ci.org/csoneson/iSEE) | [![Codecov.io coverage status](https://codecov.io/github/csoneson/iSEE/coverage.svg?branch=master)](https://codecov.io/github/csoneson/iSEE) |
| Bioc ([_devel_](http://bioconductor.org/packages/devel/bioc/html/iSEE.html)) | Multiple | [![Bioconductor-devel Build Status](http://bioconductor.org/shields/build/devel/bioc/iSEE.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/iSEE) | `NA` |
| Bioc ([_release_](http://bioconductor.org/packages/release/bioc/html/iSEE.html)) | Multiple | [![Bioconductor-release Build Status](http://bioconductor.org/shields/build/release/bioc/iSEE.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/iSEE) | `NA` |

## Overview

The _iSEE_ package provides an interactive user interface for exploring data in objects derived from the `SummarizedExperiment` class.
Particular focus is given to single-cell data stored in the `SingleCellExperiment` derived class.
The user interface is implemented with [RStudio](https://www.rstudio.com)'s [_Shiny_](https://shiny.rstudio.com), with a multi-panel setup for ease of navigation.

This initiative was proposed at the European Bioconductor Meeting in Cambridge, 2017.
Current contributors include:

- [Charlotte Soneson](https://github.com/csoneson)
- [Aaron Lun](https://github.com/LTLA)
- [Federico Marini](https://github.com/federicomarini)
- [Kévin Rue-Albrecht](https://github.com/kevinrue)

[![Figure 1. _iSEE_ uses a customisable multi-panel layout.][Figure1]](https://f1000research.com/articles/7-741/v1)

## Functionalities

The user interface of `iSEE` web-applications currently offers the following features:

:white_check_mark: Multiple interactive plot types with selectable points.

:white_check_mark: Interactive tables with selectable rows.

:white_check_mark: Coloring of samples and features by metadata or expression data.

:white_check_mark: Zooming to a plot subregion.

:white_check_mark: Transmission of point selections between panels to highlight, color, or restrict data points in the receiving panel(s).

:white_check_mark: Lasso point selection to define complex shapes.

## Sample-level visualization

The `iSEE` user interface currently contains the following components where each data point represents a single biological sample:

:white_check_mark:  **Reduced dimension plot**: Scatter plot of reduced dimensionality data.

:white_check_mark: **Column data plot**: Adaptive plot of any one or two sample metadata.
A scatter, violin, or square design is dynamically applied according to the continuous or discrete nature of the metadata.

:white_check_mark: **Feature assay plot**: Adaptive plot of expression data for any two features or one feature against one sample metadata.

:white_check_mark: **Column statistics table**: Table of sample metadata.

## Feature-level visualization

The `iSEE` user interface currently contains the following components where each data point represents a genomic feature:

:white_check_mark: **Row data plot**: Adaptive plot of any two feature metadata.
A scatter, violin, or square design is dynamically applied according to the continuous or discrete nature of the metadata.

:white_check_mark: **Sample assay plot**_: Adaptive plot of expression data for any two samples or one sample against one feature metadata.

:white_check_mark: **Row statistics table**: Table of feature metadata.

## Integrated visualization

The `iSEE` user interface contains the following components that integrate sample and feature information:

:white_check_mark: **Heat map plot**: Visualize multiple features across multiple samples annotated with sample metadata.

## Custom panels

The `iSEE` user interface allows users to programmatically define their own plotting and table panels.

:white_check_mark: **Custom data plot**: Plotting panel that can be assigned any user-defined function returning a `ggplot` object.

:white_check_mark: **Custom statistics table**: Table panel that can be assigned any user-defined function returning a `data.frame` object.

## Miscellaneous

:white_check_mark: The `iSEE` user interface continually tracks the code corresponding to all visible plotting panels.
This code is rendered in a [shinyAce](https://cran.r-project.org/web/packages/shinyAce/index.html) text editor and can be copy-pasted into R scripts for customization and further use.

## Want to try `iSEE`?

We set up an instance of iSEE running on the `allen` dataset at those addresses:

- http://shiny.imbei.uni-mainz.de:3838/iSEE
- https://marionilab.cruk.cam.ac.uk/iSEE_allen
- https://marionilab.cruk.cam.ac.uk/iSEE_tcga
- https://marionilab.cruk.cam.ac.uk/iSEE_pbmc4k
- https://marionilab.cruk.cam.ac.uk/iSEE_cytof

Please keep in mind that those public instances are for trial purposes only;
yet they demonstrate how you or your system administrator can setup `iSEE` for analyzing or sharing your precomputed `SummarizedExperiment`/`SingleCellExperiment` object.

[Figure1]: https://f1000researchdata.s3.amazonaws.com/manuscripts/16293/6bf85f9d-8352-4a78-a8da-456f05f5c4c9_figure1.gif "iSEE uses a customisable multi-panel layout"
