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
#' # Sets the search columns appropriately. 
#' sce <- .cacheCommonInfo(x, sce)
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
setMethod(".createRenderedOutput", "RowStatTable", function(x, se, colormap, output, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]

    feature_data <- .get_common_info(se, "RowTable")$valid.rowData.df 
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
