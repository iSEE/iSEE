#' The RowStatTable panel
#'
#' The RowStatTable is a panel class for creating a \linkS4class{ColumnTable} where the value of the table is defined as the \code{\link{rowData}} of the \linkS4class{SummarizedExperiment}.
#'
#' @section Slot overview:
#' This class inherits all slots from its parent \linkS4class{ColumnTable} and \linkS4class{Table} classes.
#'
#' @section Constructor:
#' \code{RowStatTable(...)} creates an instance of a RowStatTable class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' Note that \code{ColSearch} should be a character vector of length equal to the total number of columns in the \code{\link{rowData}}, though only the entries for the atomic fields will actually be used.
#' 
#' @section Contract description:
#' The RowStatTable will provide user interface elements and observers to change all of its slots.
#' The \code{\link{datatable}} is rendered with all atomic contents of the \code{\link{rowData}} of the SummarizedExperiment.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RedDimPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"RowStatTable"} entry containing \code{valid.rowData.names}, a character vector of names of atomic columns of the \code{\link{rowData}}.
#' This will also call the equivalent \linkS4class{ColumnTable} method.
#' \item \code{\link{.refineParameters}(x, se)} adjusts \code{ColSearch} to a character vector of length equal to the number of atomic fields in the \code{\link{rowData}}.
#' This will also call the equivalent \linkS4class{ColumnTable} method for further refinements to \code{x}.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.hideInterface}(x, field)} returns \code{TRUE} if \code{field="DataBoxOpen"}, 
#' otherwise it calls \code{\link{.hideInterface,Table-method}}
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Row statistics table"}.
#' }
#' 
#' For creating the output:
#' \itemize{
#' \item \code{\link{.generateTable}(x, envir)} will modify \code{envir} to contain the relevant data.frame for display,
#' while returning a character vector of commands required to produce that data.frame.
#' Each row of the data.frame should correspond to a row of the SummarizedExperiment.
#' }
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
#' x[["Selected"]] <- "SOME_ROW_NAME"
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
#' initialize,RowStatTable-method
#' .cacheCommonInfo,RowStatTable-method
#' .refineParameters,RowStatTable-method
#' .hideInterface,RowStatTable-method
#' .generateTable,RowStatTable-method
#' .panelColor,RowStatTable-method
#' .fullName,RowStatTable-method
#' .generateTable,RowStatTable-method
NULL

#' @export
RowStatTable <- function(...) {
    new("RowStatTable", ...)
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
    keep <- match(valid.names, colnames(df))
    search_vals <- search_vals[keep]
    x[[.TableColSearch]] <- search_vals

    x
})

#' @export
setMethod(".fullName", "RowStatTable", function(x) "Row statistics table")

#' @export
setMethod(".panelColor", "RowStatTable", function(x) "#E47E04")

#' @export
setMethod(".hideInterface", "RowStatTable", function(x, field) {
    if (field %in% .dataParamBoxOpen) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".generateTable", "RowStatTable", function(x, envir) {
    cmds <-"tab <- as.data.frame(rowData(se));"

    if (exists("row_selected", envir=envir, inherits=FALSE)) {
        cmds <- c(cmds, "tab <- tab[unique(unlist(row_selected)),,drop=FALSE]")
    }

    valid.names <- .get_common_info(envir$se, "RowStatTable")$valid.rowData.names
    if (!identical(colnames(rowData(envir$se)), valid.names)) {
        cmds <- c(cmds, sprintf("tab <- tab[,%s,drop=FALSE]",
            paste(deparse(valid.names), collapse="\n     ")))
    }

    .text_eval(cmds, envir)

    cmds
})
