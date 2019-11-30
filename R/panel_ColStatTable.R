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
#' # Search column refinement works as expected.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
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
setMethod(".createRenderedOutput", "ColStatTable", function(x, se, colormap, output, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]

    sample_data <- .get_common_info(se, "ColumnTable")$valid.colData.df
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
