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
#' \item{\code{Active}:}{Logical, should this plot be shown upon initialization?
#' By default, this is only \code{TRUE} for the first plot.}
#' \item{\code{Type}:}{Characater, what entry of \code{reducedDims(se)} should be shown?
#' By default, the first entry is shown.}
#' \item{\code{XAxis}:}{Integer, which component should be shown on the x-axis?
#' Defaults to 1.}
#' \item{\code{YAxis}:}{Integer, which component should be shown on the y-axis?
#' Defaults to 2.}
#' \item{\code{OpenPlotPanel}:}{Logical, should the plot parameter panel be open upon initialization?
#' Defaults to \code{TRUE}.}
#' \item{\code{ColorBy}:}{Character, what type of data should be used for coloring?
#' Defaults to \code{"none"}.}
#' \item{\code{ColorByColData}:}{Character, what field of the column data should be used for colouring if \code{ColorBy="Column data"}? 
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{ColorByGeneExprs}:}{Character, which gene's expression should be used for colouring if \code{ColorBy="Gene expression"}? 
#' Defaults to the name of the first row in \code{se}.}
#' \item{\code{ColorByGeneExprsAssay}:}{Character, what expression values should be used for colouring if \code{ColorBy="Gene expression"}? 
#' Defaults to the name of the first assay in \code{se}.}
#' }
#'
#' @section Gene expression plot parameters:
#' \describe{
#' \item{\code{Active}:}{Logical, should this plot be shown upon initialization?
#' By default, this is only \code{TRUE} for the first plot.}
#' \item{\code{Gene}:}{Character, which gene's expression values should be shown on the y-axis?
#' Defaults to the name of the first row in \code{se}.}
#' \item{\code{Assay}:}{Character, what expression values should be shown on the y-axis?
#' Defaults to the name of the first assay in \code{se}.}
#' \item{\code{XAxis}:}{Character, what variable should be shown on the x-axis?
#' Defaults to \code{"none"}.}
#' \item{\code{XAxisColData}:}{Character, what field of the column data should be shown on the x-axis if \code{XAxis="Column data"}?
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{XAxisGeneExprs}:}{Character, which gene's expression should be shown on the x-axis if \code{XAxis="Gene expression"}? 
#' Defaults to the name of the first row in \code{se}, using expression values specified in \code{Assay}.}
#' \item{\code{OpenPlotPanel}:}{Logical, should the plot parameter panel be open upon initialization?
#' Defaults to \code{TRUE}.}
#' \item{\code{ColorBy}:}{Character, what type of data should be used for coloring?
#' Defaults to \code{"none"}.}
#' \item{\code{ColorByColData}:}{Character, what field of the column data should be used for colouring if \code{ColorBy="Column data"}? 
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{ColorByGeneExprs}:}{Character, which gene's expression should be used for colouring if \code{ColorBy="Gene expression"}? 
#' Defaults to the name of the first row in \code{se}, using expression values specified in \code{Assay}.}
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
redDimPlotDefaults <- function(se, max.plots) {
    activity <- logical(max.plots)
    activity[1] <- TRUE
    out <- DataFrame(Active=activity)
    all.assays <- assayNames(se)
    if ("logcounts" %in% all.assays) {
        def.assay <- "logcounts"
    } else {
        def.assay <- all.assays[1]
    }

    out[[.redDimType]] <- reducedDimNames(se)[1]
    out[[.redDimXAxis]] <- 1L
    out[[.redDimYAxis]] <- 2L
    
    out[[.redDimPlotPanel]] <- TRUE
    out[[.redDimColorBy]] <- .colorByColDataTitle
    out[[.redDimColorByColData]] <- colnames(colData(se))[1]
    out[[.redDimColorByGeneExprs]] <- rownames(se)[1]
    out[[.redDimColorByGeneExprsAssay]] <- def.assay
    return(out)
}

#' @rdname defaults 
#' @export
geneExprPlotDefaults <- function(se, max.plots) {
    activity <- logical(max.plots)
    activity[1] <- TRUE
    all.assays <- assayNames(se)
    if ("logcounts" %in% all.assays) {
        def.assay <- "logcounts"
    } else {
        def.assay <- all.assays[1]
    }
    gene.names <- rownames(se)
    covariates <- colnames(colData(se))

    out <- DataFrame(Active=activity)
    out[[.geneExprID]] <- gene.names[1]
    out[[.geneExprAssay]] <- def.assay
    out[[.geneExprXAxis]] <- .geneExprXAxisColDataTitle
    out[[.geneExprXAxisColData]] <- covariates[1] 
    out[[.geneExprXAxisGeneExprs]] <- gene.names[1]

    out[[.geneExprPlotPanel]] <- TRUE
    out[[.geneExprColorBy]] <- .colorByColDataTitle
    out[[.geneExprColorByColData]] <- covariates[1]
    out[[.geneExprColorByGeneExprs]] <- gene.names[1]
    return(out)
}

.override_defaults <- function(def, usr) {
    stopifnot(identical(nrow(def), nrow(usr)))
    for (x in colnames(usr)) {
        if (!x %in% colnames(def)) { 
            warning(sprintf("unknown field '%s' in supplied default DataFrame", x))
            next 
        }
        def[[x]] <- usr[[x]]
    }
    return(def)
}    

