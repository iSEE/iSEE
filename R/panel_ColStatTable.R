#' Column statistics table panel
#'
#' Defines a \code{\link{datatable}} panel containing statistics 
#' from the \code{\link{colData}} of a \linkS4class{SummarizedExperiment}.
#' 
#' @section Constructor:
#' \code{ColStatTable()} creates an instance of a ColStatTable class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- ColStatTable()
#' x[["Selected"]] 
#' x[["Selected"]] <- 2L
#' 
#' ##################
#' # For developers #
#' ##################
#' 
#' library(scater)
#' sce <- mockSCE()
#'
#' # Spits out a NULL and a warning if no reducedDims are available.
#' sce0 <- sce[,0]
#' sce0 <- iSEE:::.set_common_info(sce0, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce0))
#' .refineParameters(x, sce0)
#' 
#' # Replaces the default with something sensible.
#' sce <- iSEE:::.set_common_info(sce, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce))
#' .refineParameters(x, sce)
#'
#' @name ColStatTable
#' @aliases ColStatTable ColStatTable-class
#' .createRenderedOutput,ColStatTable-method
#' .getEncodedName,ColStatTable-method 
NULL

#' @export
ColStatTable <- function() {
    new("ColStatTable")
}

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".createRenderedOutput", "ColStatTable", function(x, id, se, colormap, output, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    sample_data <- data.frame(colData(se), check.names=FALSE)
    rownames(sample_data) <- colnames(se)
    if (identical(ncol(sample_data), 0L)) {
        sample_data$Present <- !logical(nrow(sample_data))
    }
    sample_data_select_col <- .safe_field_name("Selected", colnames(sample_data))

    .define_table_output(mode, id, 
        tab=sample_data, select_col=sample_data_select_col,
        output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".getEncodedName", "ColStatTable", function(x) "colStatTable")
