
#' App pre-configured to link multiple feature assay plots
#'
#' This mode launches a Shiny App preconfigured with multiple chain-linked
#' feature expression plots is launched for interactive data exploration of the
#' \code{\link{SingleCellExperiment}} or \code{\link{SummarizedExperiment}}
#' object.
#'
#' @param se An object that coercible to \linkS4class{SingleCellExperiment}
#' @param features \code{data.frame} with columns named \code{x} and \code{y}
#' that define the features on the axes of the linked plots.
#' Plots are serially linked from the first row to the last.
#' @param featAssayMax Maximal number of feature assay plots in the app.
#' @param ... Additional arguments passed to \code{\link{iSEE}}.
#' @param plot_width The grid width of linked plots (numeric vector of
#' length either 1 or equal to \code{nrow(features)}
#'
#' @return A Shiny app object is returned.
#'
#' @export
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#' library(scRNAseq)
#'
#' # Example data ----
#' sce <- ReprocessedAllenData(assays="tophat_counts")
#' class(sce)
#'
#' library(scater)
#' sce <- logNormCounts(sce, exprs_values="tophat_counts")
#'
#' # Select top variable genes ----
#'
#' plot_count <- 6
#' rv <- rowVars(assay(sce, "tophat_counts"))
#' top_var <- head(order(rv, decreasing=TRUE), plot_count*2)
#' top_var_genes <- rownames(sce)[top_var]
#'
#' plot_features <- data.frame(
#'     x=head(top_var_genes, plot_count),
#'     y=tail(top_var_genes, plot_count),
#'     stringsAsFactors=FALSE
#'  )
#'
#' # launch the app itself ----
#'
#' app <- modeGating(sce, features=plot_features, featAssayMax=6)
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
modeGating <- function(
    se, features, featAssayMax=max(2, nrow(features)), ..., plot_width=4
){
    # This mode is meaningless with fewer than two featAssayPlot
    stopifnot(nrow(features) > 1)
    stopifnot(featAssayMax >= nrow(features))
    stopifnot(all(c("x", "y") %in% colnames(features)))

    featAssayArgs <- featAssayPlotDefaults(se, featAssayMax)
    # prepare featAssayArgs
    featAssayArgs[[.featAssayXAxis]] <- .featAssayXAxisFeatNameTitle
    # Y axes take all the odd-numbered feature names
    featAssayArgs[[.featAssayXAxisFeatName]] <- features[,"x"]
    # X axes take all the even-numbered feature names
    featAssayArgs[[.featAssayYAxisFeatName]] <- features[,"y"]
    featAssayArgs[[.selectByPlot]] <- c(
        "",
        sprintf("Feature assay plot %i", seq(1, nrow(features) - 1, 1)),
        rep("", nrow(featAssayArgs) - nrow(features))
    )
    featAssayArgs[[.selectEffect]] <- c(
        "",
        rep("Restrict", nrow(features) - 2),
        "Color",
        rep("", nrow(featAssayArgs) - nrow(features))
    )
    # Show only the active
    initialPanels <- DataFrame(
        Name=c(sprintf("Feature assay plot %i", seq(1, nrow(features), 1))),
        Width=plot_width
    )
    # Preconfigure an app
    app <- iSEE(
        se=se,
        redDimArgs=NULL, colDataArgs=NULL, featAssayArgs=featAssayArgs,
        rowStatArgs=NULL, rowDataArgs=NULL, sampAssayArgs=NULL,
        colStatArgs=NULL, customDataArgs=NULL, customStatArgs=NULL,
        heatMapArgs=NULL,
        redDimMax=0, colDataMax=0, featAssayMax=featAssayMax,
        rowStatMax=0, rowDataMax=0, sampAssayMax=0, colStatMax=0,
        customDataMax=0, customStatMax=0, heatMapMax=0,
        initialPanels=initialPanels,
        ...
    )

    return(app)
}

#' App pre-configured to launch with no visible panel
#'
#' This mode launches an app that does not display any panel,
#' irrespective of which panels are available.
#'
#' This mode presents the advantage to launch an interface in a minimal amount of time,
#' as it does not need to render any panel when the interface is launched.
#' Users can then use the \code{"Organize panels"} widget to select panels to display in the interface.
#'
#' @param ... Arguments passed to \code{\link{iSEE}}.
#'
#' @return A Shiny app object is returned.
#' @export
#'
#' @examples
#' example("SingleCellExperiment")
#'
#' app <- modeEmpty(sce)
#' if (interactive()) {
#'   shiny::runApp(app, port=1234)
#' }
modeEmpty <- function(...){
    # Do not show any panel
    initialPanels <- DataFrame(
        Name=character(0),
        Width=integer(0),
        Height=integer(0)
    )
    # Preconfigure an app
    app <- iSEE(initialPanels=initialPanels, ...)

    return(app)
}
