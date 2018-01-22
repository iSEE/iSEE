#' @name defaults 
#' @aliases redDimPlotDefaults
#' @aliases geneExprPlotDefaults
#'
#' @title Plot parameter defaults 
#'
#' @description Create default settings for various plots in the iSEE interface.
#'
#' @param se A SingleCellExperiment object.
#' @param max.plots An integer scalar, specifying the maximum number of 
#' plots of the corresponding type that can be added to the interface.
#'
#' @section Reduced dimension plot parameters:
#' \describe{
#' \item{\code{Type}:}{Character, what entry of \code{reducedDims(se)} should be shown?
#' By default, the first entry is shown.}
#' \item{\code{XAxis}:}{Integer, which component should be shown on the x-axis?
#' Defaults to 1.}
#' \item{\code{YAxis}:}{Integer, which component should be shown on the y-axis?
#' Defaults to 2.}
#' }
#'
#' @section Gene expression plot parameters:
#' \describe{
#' \item{\code{GeneTable}:}{Character, what gene statistic table should be used to choose a gene to display on the y-axis?
#' Defaults to an empty string, which means that the first available table will be used.}
#' \item{\code{Assay}:}{Character, what expression values should be shown on the y-axis?
#' Defaults to the name of the first assay in \code{se}.}
#' \item{\code{XAxis}:}{Character, what variable should be shown on the x-axis?
#' Defaults to \code{"None"}.}
#' \item{\code{XAxisColData}:}{Character, what column of \code{colData(se)} should be shown on the x-axis if \code{XAxis="Column data"}?
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{XAxisGeneText}:}{Character, which gene's expression should be shown on the x-axis if \code{XAxis="Gene text"}? 
#' Defaults to the name of the first row in \code{se}, using expression values specified in \code{Assay}.}
#' \item{\code{XAxisGeneTable}:}{Character, which gene statistic table should be used to choose a gene to put on the x-axis if \code{XAxis="Gene table"}? 
#' Defaults to an empty string, which means that the first available table will be used.}
#' }
#'
#' @section Column data plot parameters:
#' \describe{
#' \item{\code{YAxisColData}:}{Character, which column of \code{colData(se)} should be shown on the y-axis?
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{XAxis}:}{Character, what variable should be shown on the x-axis?
#' Defaults to \code{"None"}.}
#' \item{\code{XAxisColData}:}{Character, which column of \code{colData(se)} should be shown on the x-axis if \code{XAxis="Column data"}?
#' Defaults to the first entry of \code{colData(se)}.}
#' }
#'
#' @section Coloring parameters:
#' \describe{
#' \item{\code{ColorPanelOpen}:}{Logical, should the color parameter panel be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{ColorBy}:}{Character, what type of data should be used for coloring?
#' Defaults to \code{"None"}.}
#' \item{\code{ColorByColData}:}{Character, which column of \code{colData(se)} should be used for colouring if \code{ColorBy="Column data"}? 
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{ColorByGeneTable}:}{Character, which gene statistic table should be used to choose a gene to color by, if \code{ColorBy="Gene table"}? 
#' Defaults to an empty string, which means that the first available table will be used.}
#' \item{\code{ColorByGeneTableAssay}:}{Character, what expression values should be used for colouring if \code{ColorBy="Gene table"}? 
#' Defaults to the name of the first assay in \code{se}.}
#' \item{\code{ColorByGeneText}:}{Character, which gene should be used to choose a gene to color by, if \code{ColorBy="Gene text"}? 
#' Defaults to an empty string, which means that the first available table will be used.}
#' \item{\code{ColorByGeneTextAssay}:}{Character, what expression values should be used for colouring if \code{ColorBy="Gene text"}? 
#' Defaults to the name of the first assay in \code{se}.}
#' }
#'
#' @section Brushing parameters:
#' \describe{
#' \item{\code{BrushPanelOpen}:}{Logical, should the brushing parameter panel be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{BrushOn}:}{Logical, should the plot be transmitting its brush for use in other plots?
#' Defaults to \code{FALSE}.}
#' \item{\code{BrushByPlot}:}{Character, which other plot should be used for point selection in the current plot? 
#' Defaults to an empty string, which means that no plot is used for point selection.}
#' \item{\code{BrushEffect}:}{Character, what is the effect of receiving a brush input?
#' Can be \code{"Restrict"}, where only the brushed points are shown; \code{"Color"}, where the brushed points have a different color; 
#' or \code{"Transparent"}, where all points other than the brushed points are made transparent. Defaults to \code{"Transparent"}.}
#' \item{\code{BrushColor}:}{Character, what color should be used for the brushed points when \code{BrushEffect="Color"}?
#' Defaults to \code{"red"}.}
#' \item{\code{BrushAlpha}:}{Numeric, what level of transparency should be used for the unbrushed points whe \code{BrushEffect="Transparent"}?
#' This should lie in [0, 1], where 0 is fully transparent and 1 is fully opaque. 
#' Defaults to 0.1.}
#' }
#'
#' @section Other parameters:
#' \describe{
#' \item{\code{PlotPanelOpen}:}{Logical, should the plot parameter panel be open upon initialization?
#' Defaults to \code{FALSE}.}
#' }
#'
#' @return A DataFrame containing default settings for various 
#' parameters of reduced dimension or gene expression plots.
#'
#' @export
#'
#' @examples
#' library(scRNAseq)
#' data(allen)
#' class(allen)
#'
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' counts(sce) <- assay(sce, "tophat_counts")
#' sce <- normalize(sce)
#' sce <- runPCA(sce)
#' sce
#'
#' redDimPlotDefaults(sce, max.plots=5)
#' geneExprPlotDefaults(sce, max.plots=5)
#' colDataPlotDefaults(sce, max.plots=5)
redDimPlotDefaults <- function(se, max.plots) {
    all.assays <- assayNames(se)
    if ("logcounts" %in% all.assays) {
        def.assay <- "logcounts"
    } else {
        def.assay <- all.assays[1]
    }

    out <- DataFrame(matrix(0, max.plots, 0))
    out[[.redDimType]] <- reducedDimNames(se)[1]
    out[[.redDimXAxis]] <- 1L
    out[[.redDimYAxis]] <- 2L
    
    out <- .add_general_parameters(out, colnames(colData(se))[1], def.assay)
    return(out)
}

#' @rdname defaults 
#' @export
geneExprPlotDefaults <- function(se, max.plots) {
    all.assays <- assayNames(se)
    if ("logcounts" %in% all.assays) {
        def.assay <- "logcounts"
    } else {
        def.assay <- all.assays[1]
    }
    covariates <- colnames(colData(se))

    out <- DataFrame(matrix(0, max.plots, 0))
    out[[.geneExprAssay]] <- def.assay
    out[[.geneExprXAxis]] <- .geneExprXAxisNothingTitle
    out[[.geneExprXAxisColData]] <- covariates[1] 
    out[[.geneExprXAxisGeneText]] <- ""
    out[[.geneExprXAxisGeneTable]] <- ""
    out[[.geneExprYAxisGeneText]] <- ""
    out[[.geneExprYAxisGeneTable]] <- ""
    out[[.geneExprYAxis]] <- .geneExprYAxisGeneTableTitle

    out <- .add_general_parameters(out, covariates[1], def.assay)
    return(out)
}

#' @rdname defaults 
#' @export
colDataPlotDefaults <- function(se, max.plots) {
    all.assays <- assayNames(se)
    if ("logcounts" %in% all.assays) {
        def.assay <- "logcounts"
    } else {
        def.assay <- all.assays[1]
    }
    covariates <- colnames(colData(se))

    out <- DataFrame(matrix(0, max.plots, 0))
    out[[.colDataYAxis]] <- covariates[1]
    out[[.colDataXAxis]] <- .colDataXAxisNothingTitle
    out[[.colDataXAxisColData]] <- ifelse(length(covariates)==1L, covariates[1], covariates[2])

    out <- .add_general_parameters(out, covariates[1], def.assay)
    return(out)
}

.override_defaults <- function(def, usr) 
# Overriding the defaults with whatever the user has supplied.
{
    stopifnot(identical(nrow(def), nrow(usr)))
    for (x in colnames(usr)) {
        if (!x %in% colnames(def)) { 
            warning(sprintf("unknown field '%s' in user-specified settings", x))
            next 
        }
        def[[x]] <- usr[[x]]
    }
    return(def)
}    

.add_general_parameters <- function(incoming, defaultColData, defaultAssay) {
    incoming[[.plotParamPanelOpen]] <- FALSE
    incoming[[.colorParamPanelOpen]] <- FALSE
    incoming[[.brushParamPanelOpen]] <- FALSE

    incoming[[.colorByField]] <- .colorByNothingTitle
    incoming[[.colorByColData]] <- defaultColData
    incoming[[.colorByGeneTable]] <- "" 
    incoming[[.colorByGeneTableAssay]] <- defaultAssay
    incoming[[.colorByGeneText]] <- "" 
    incoming[[.colorByGeneTextAssay]] <- defaultAssay

    incoming[[.brushActive]] <- FALSE
    incoming[[.brushByPlot]] <- ""
    incoming[[.brushEffect]] <- .brushTransTitle
    incoming[[.brushTransAlpha]] <- 0.1
    incoming[[.brushColor]] <- "red"

    incoming[[.zoomActive]] <- TRUE
    incoming[[.zoomData]] <- rep(list(NULL), nrow(incoming))
    return(incoming)
}
