#' Plot parameter defaults 
#'
#' Create default settings for various plots in the iSEE interface.
#'
#' @param se A SingleCellExperiment object.
#' @param max.plots An integer scalar, specifying the maximum number of 
#' plots that can be added to the interface.
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

    out[[.geneExprColorBy]] <- .colorByColDataTitle
    out[[.geneExprColorByColData]] <- covariates[1]
    out[[.geneExprColorByGeneExprs]] <- gene.names[1]
    out[[.geneExprPlotPanel]] <- TRUE
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

