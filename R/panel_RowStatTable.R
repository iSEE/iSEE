#' Row statistics table panel
#'
#' Defines a \code{\link{datatable}} panel containing statistics 
#' from the \code{\link{rowData}} of a \linkS4class{SummarizedExperiment}.
#' 
#' @section Constructor:
#' \code{RowStatTable()} creates an instance of a RowStatTable class.
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
#' x <- RowStatTable()
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
#' sce0 <- sce[0,]
#' sce0 <- iSEE:::.set_common_info(sce0, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce0))
#' .refineParameters(x, sce0)
#' 
#' # Replaces the default with something sensible.
#' sce <- iSEE:::.set_common_info(sce, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce))
#' .refineParameters(x, sce)
#'
#' @name RowStatTable
#' @aliases RowStatTable RowStatTable-class
#' .createRenderedOutput,RowStatTable-method
#' .getEncodedName,RowStatTable-method 
NULL

#' @export
RowStatTable <- function() {
    new("RowStatTable")
}

#' @export
setMethod(".createRenderedOutput", "RowStatTable", function(x, id, se, colormap, output, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    feature_data <- .get_common_info(x, .getEncodedName(x))$RowTable$df 
    if (identical(ncol(feature_data), 0L)) {
        feature_data$Present <- !logical(nrow(feature_data))
    }
    feature_data_select_col <- .safe_field_name("Selected", colnames(feature_data))

    .define_table_output(mode, id, 
        tab=feature_data, select_col=feature_data_select_col,
        output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".getEncodedName", "RowStatTable", function(x) "rowStatTable")
