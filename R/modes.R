
#' App pre-configured to link multiple feature expression plots
#'
#' @param se An object that coercible to
#'\code{\linkS4class{SingleCellExperiment}}.
#' @param features \code{data.frame} with columns named \code{x} and \code{y}
#' that define the features on the axes of the linked plots.
#' Plots are serially linked from the first row to the last.
#' @param featExprMax Maximal number of feature expression plots in the app.
#' @param ... Additional arguments passed to \code{\link{iSEE}}.
#' @param plot_width The grid width of linked plots (numeric vector of
#' length either 1 or equal to \code{nrow(features)}
#' 
#' @return A Shiny App preconfigure with multiple chain-linked feature
#' expression plots is launched for interactive data exploration of the
#' \code{\link{SingleCellExperiment}} / \code{\link{SummarizedExperiment}}
#' object
#' 
#' @export
#'
#' @examples
#' library(scRNAseq)
#' data(allen)
#' class(allen)
#'
#' # Example data ----
#'
#' library(scater)
#' sce <- as(allen, "SingleCellExperiment")
#' counts(sce) <- assay(sce, "tophat_counts")
#' sce <- normalize(sce)
#' 
#' 
#' # Select top variable genes ----
#' 
#' plot_count <- 6
#' rv <- rowVars(logcounts(sce))
#' top_var <- head(order(rv, decreasing = TRUE), plot_count*2)
#' top_var_genes <- rownames(sce)[top_var]
#' 
#' plot_features <- data.frame(
#'     x = head(top_var_genes, plot_count),
#'     y = tail(top_var_genes, plot_count),
#'     stringsAsFactors = FALSE
#'  )
#'
#' # launch the app itself ----
#'
#' app <- mode_gating(sce, features = plot_features, featExprMax = 6)
#' if (interactive()) {
#'   shiny::runApp(app, port = 1234)
#' }

mode_gating <- function(
  se, features,
  featExprMax = max(2, nrow(features)),
  ..., plot_width = 4){
  # This mode is meaningless with fewer than two featExprPlot
  stopifnot(nrow(features) > 1)
  stopifnot(all(c("x","y") %in% colnames(features)))
  
  featExprArgs <- featExprPlotDefaults(se, featExprMax)
  # prepare featExprArgs
  featExprArgs[[iSEE:::.featExprXAxis]] <- iSEE:::.featExprXAxisFeatNameTitle
  featExprArgs[[iSEE:::.featExprYAxis]] <- iSEE:::.featExprYAxisFeatNameTitle
  # Y axes take all the odd-numbered feature names
  featExprArgs[[iSEE:::.featExprXAxisFeatName]] <- features[,"x"]
  # X axes take all the even-numbered feature names
  featExprArgs[[iSEE:::.featExprYAxisFeatName]] <- features[,"y"]
  featExprArgs[[iSEE:::.brushByPlot]] <- c(
    "",
    sprintf("Feature expression plot %i", seq(1, nrow(features) - 1, 1)),
    rep("", nrow(featExprArgs) - nrow(features))
  )
  featExprArgs[[iSEE:::.brushEffect]] <- c(
    "",
    rep("Restrict", nrow(features) - 2),
    "Color",
    rep("", nrow(featExprArgs) - nrow(features))
  )
  # Show only the active 
  initialPanels = DataFrame(
    Name = c(
      sprintf("Feature expression plot %i", seq(1, nrow(features), 1))
    ),
    Width = plot_width
  )
  # Preconfigure an app
  app <- iSEE(
    se,
    redDimArgs = NULL, colDataArgs = NULL,
    featExprArgs = featExprArgs,
    rowStatArgs = NULL, rowDataArgs = NULL, heatMapArgs = NULL,
    redDimMax = 0, colDataMax = 0,
    featExprMax = featExprMax, # possibly larger
    rowStatMax = 0, rowDataMax = 0, heatMapMax = 0,
    initialPanels = initialPanels,
    annot.orgdb = NULL, annot.keytype = "ENTREZID", annot.keyfield = NULL,
    ...
  )
  
  return(app)
}
