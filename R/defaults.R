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
    DataFrame(
        Active=activity,                        
        Type=reducedDimNames(se)[1],
        Dim1=1, 
        Dim2=2,
        ColorBy=.colorByColDataTitle,
        ColorByColData=colnames(colData(se))[1],
        ColorByGeneExprs=rownames(se)[1],
        ColorByGeneExprsAssay=assayNames(se)[1],
        PlotParamPanel=TRUE
   )
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

    DataFrame(
        Active=activity,
        ID=gene.names[1],
        ExprAssay=def.assay,
        XAxis=.geneExprXAxisColDataTitle,
        XColData=covariates[1],
        XGeneExprs=gene.names[1],
        ColorBy=.colorByColDataTitle,
        ColorColData=covariates[1],
        ColorGeneExprs=gene.names[1],
        ColorByGeneExprsAssay=def.assay,
        PlotParamPanel=TRUE
   )
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

