[![Travis build status](https://travis-ci.org/csoneson/SEE.svg?branch=master)](https://travis-ci.org/csoneson/SEE)

# iSEE/SummarizedExplorer

## Overview

The _SummarizedExplorer_ package aims to provide an interactive user interface for exploring data in `SummarizedExperiment` objects.
Particular focus will be given to single-cell data in the `SingleCellExperiment` derived class.
The interface will be implemented in _Shiny_ with a multi-tab setup for ease of navigation.

This initiative was proposed at the European Bioconductor Meeting in Cambridge, 2017.
Current contributors include:

- Charlotte Soneson
- Aaron Lun
- Federico Marini
- Kevin Rue-Albrecht

## Cell-based visualizations 

The interface is proposed to contain the following features in its first iteration:

- multiple interactive scatter plots with selectable points
- colouring of samples by metadata or expression values
- dynamic updating of cell-level metadata on hover/click
- zooming in to a particular subregion of the plot, if requested

Scatter plots can be generated from reduced dimensionality data, or with biaxial plots of existing metadata columns.

## Gene-based visualization

The interface is proposed to contain the following features in its first iteration:

- boxplots of expression values for a single gene, stratified by metadata level
- heatmaps of multiple genes for groups of cells or for individual ordered cells 
- integrated brushing in cell-based scatter plots with cell identities in gene-level plots

## Miscellaneous

The interface will contain a continually updated R interface that provides the R code corresponding to each user interaction.
This can be copy-pasted into R scripts for batch generation of figures.

