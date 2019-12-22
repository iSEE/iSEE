#' Row statistics table panel
#'
#' Defines a \code{\link{datatable}} panel containing statistics
#' from the \code{\link{rowData}} of a \linkS4class{SummarizedExperiment}.
#'
#' @section Constructor:
#' \code{RowStatTable()} creates an instance of a RowStatTable class.
#'
#' @section Panel parameters:
#' \code{\link{.defineInterface}} will create parameter elements for choosing the reduced dimensions to plot.
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
#' .renderOutput,RowStatTable-method
#' .getEncodedName,RowStatTable-method
NULL

#' @export
RowStatTable <- function() {
    new("RowStatTable")
}

#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".cacheCommonInfo", "RowStatTable", function(x, se) {
    if (!is.null(.get_common_info(se, "RowStatTable"))) {
        return(se)
    }

    se <- callNextMethod()

    df <- rowData(se)
    available <- .find_atomic_fields(df)
    .set_common_info(se, "RowStatTable",
        valid.rowData.names=available)
})

#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".refineParameters", "RowStatTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    valid.names <- .get_common_info(se, "RowStatTable")$valid.rowData.names
    df <- rowData(se)

    # First, expanding out so that we cover all columns.
    search_vals <- x[[.TableColSearch]]
    N <- ncol(df)
    if (length(search_vals)!=N) {
        search_vals <- character(N)
    }

    # Then, contracting only to those columns that survived.
    keep <- match(colnames(df), valid.names)
    search_vals <- search_vals[keep]
    x[[.TableColSearch]] <- search_vals

    x
})

#' @export
setMethod(".fullName", "RowStatTable", function(x) "Row statistics table")

#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".getTableCommands", "RowStatTable", function(x, envir) {
    cmds <-"tab <- as.data.frame(rowData(se));"

    if (exists("row_selected", envir=envir, inherits=FALSE)) {
        cmds <- c(cmds, "tab <- tab[unique(unlist(row_selected)),,drop=FALSE]")
    }

    valid.names <- .get_common_info(envir$se, "RowStatTable")$valid.rowData.names
    if (!identical(colnames(rowData(envir$se)), valid.names)) {
        cmds <- c(cmds, sprintf("tab <- tab[,%s,drop=FALSE]",
            paste(deparse(valid.names), collapse="\n     ")))
    }

    cmds
})
