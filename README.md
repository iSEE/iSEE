
# `iSEE` - interactive SummarizedExperiment/SingleCellExperiment Explorer

<!-- TODO logo here too :) --> 

## Software status

| Platforms |  OS  | R CMD check | Coverage | 
|:----------------:|:----------------:|:----------------:|:----------------:|
| Travis CI | Linux | [![Travis CI build status](https://travis-ci.org/csoneson/iSEE.svg?branch=master)](https://travis-ci.org/csoneson/iSEE) | [![Codecov.io coverage status](https://codecov.io/github/csoneson/iSEE/coverage.svg?branch=master)](https://codecov.io/github/csoneson/iSEE) |
| Bioc ([_devel_](http://bioconductor.org/packages/devel/bioc/html/iSEE.html)) | Multiple | [![Bioconductor-devel Build Status](http://bioconductor.org/shields/build/devel/bioc/iSEE.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/iSEE) | `NA` |

## Overview

The _iSEE_ package aims to provide an interactive user interface for exploring data in objects derived from the `SummarizedExperiment` class.
Particular focus will be given to single-cell data in the `SingleCellExperiment` derived class.
The interface is implemented with [RStudio](https://www.rstudio.com)'s [_Shiny_](https://shiny.rstudio.com), with a multi-panel setup for ease of navigation.

This initiative was proposed at the European Bioconductor Meeting in Cambridge, 2017.
Current contributors include:

- Charlotte Soneson
- Aaron Lun
- Federico Marini
- Kevin Rue-Albrecht

## Cell-based visualizations 

The interface is proposed to contain the following features in its first iteration:

:white_check_mark: multiple interactive scatter plots with selectable points

:white_check_mark: colouring of samples by metadata or expression values

:white_check_mark: zooming in to a particular subregion of the plot, if requested

:white_check_mark: scatter plots can be generated from reduced dimensionality data, or with biaxial plots of existing metadata columns.

## Gene-based visualization

The interface is proposed to contain the following features in its first iteration:

:white_check_mark: boxplots of expression values for a single gene, stratified by metadata level

:white_check_mark: heatmaps of multiple genes for groups of cells or for individual ordered cells 

:white_check_mark: integrated brushing in cell-based scatter plots with cell identities in gene-level plots

## Miscellaneous

:white_check_mark: The interface will contain a continually updated R interface that provides the R code corresponding to each user interaction.
This can be copy-pasted into R scripts for batch generation of figures.

## Want to try `iSEE`?

We set up an instance of iSEE running on the `allen` dataset at this address: http://shiny.imbei.uni-mainz.de:3838/iSEE. 
Please keep in mind this is only for trial purposes, yet it can show a quick way of how you or your system administrator can setup `r Biocpkg("iSEE")` for analyzing your `SummarizedExperiment`/`SingleCellExperiment` precomputed object.


