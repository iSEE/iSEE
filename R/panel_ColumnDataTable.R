#' The ColumnDataTable panel
#'
#' The ColumnDataTable is a panel class for creating a \linkS4class{ColumnTable} where the value of the table is defined as the \code{\link{colData}} of the \linkS4class{SummarizedExperiment}.
#' It provides functionality to extract the \code{\link{colData}} to coerce it into an appropriate data.frame in preparation for rendering.
#'
#' @section Slot overview:
#' This class inherits all slots from its parent \linkS4class{ColumnTable} and \linkS4class{Table} classes.
#'
#' @section Constructor:
#' \code{ColumnDataTable(...)} creates an instance of a ColumnDataTable class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' Note that \code{ColSearch} should be a character vector of length equal to the total number of columns in the \code{\link{colData}}, though only the entries for the atomic fields will actually be used.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ColumnDataTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"ColumnDataTable"} entry containing \code{valid.colData.names}, a character vector of names of atomic columns of the \code{\link{colData}}.
#' This will also call the equivalent \linkS4class{ColumnTable} method.
#' \item \code{\link{.refineParameters}(x, se)} adjusts \code{ColSearch} to a character vector of length equal to the number of atomic fields in the \code{\link{colData}}.
#' This will also call the equivalent \linkS4class{ColumnTable} method for further refinements to \code{x}.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Column data table"}.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' }
#'
#' For creating the output:
#' \itemize{
#' \item \code{\link{.generateTable}(x, envir)} will modify \code{envir} to contain the relevant data.frame for display,
#' while returning a character vector of commands required to produce that data.frame.
#' Each row of the data.frame should correspond to a column of the SummarizedExperiment.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a panel-specific tour.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent class \linkS4class{Panel} are also available.
#'
#' @author Aaron Lun
#'
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- ColumnDataTable()
#' x[["Selected"]]
#' x[["Selected"]] <- "SOME_SAMPLE_NAME"
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
#' @name ColumnDataTable-class
#' @aliases ColumnDataTable ColumnDataTable-class
#' initialize,ColumnDataTable-method
#' .cacheCommonInfo,ColumnDataTable-method
#' .refineParameters,ColumnDataTable-method
#' .generateTable,ColumnDataTable-method
#' .panelColor,ColumnDataTable-method
#' .fullName,ColumnDataTable-method
#' .generateTable,ColumnDataTable-method
#' .definePanelTour,ColumnDataTable-method
NULL

#' @export
ColumnDataTable <- function(...) {
    new("ColumnDataTable", ...)
}

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".cacheCommonInfo", "ColumnDataTable", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "ColumnDataTable"))) {
        return(se)
    }

    se <- callNextMethod()

    df <- colData(se)
    available <- .findAtomicFields(df)
    .setCachedCommonInfo(se, "ColumnDataTable",
        valid.colData.names=available)
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".refineParameters", "ColumnDataTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    valid.names <- .getCachedCommonInfo(se, "ColumnDataTable")$valid.colData.names
    df <- colData(se)

    # First, expanding out so that we cover all columns.
    search_vals <- slot(x, .TableColSearch)
    search_vals <- .expand_named_colsearch(df, search_vals)

    N <- ncol(df)
    if (length(search_vals)!=N) {
        search_vals <- character(N)
    }

    # Then, contracting only to those columns that survived.
    keep <- match(valid.names, colnames(df))
    search_vals <- search_vals[keep]
    slot(x, .TableColSearch) <- search_vals

    x
})

#' @export
setMethod(".fullName", "ColumnDataTable", function(x) "Column data table")

#' @export
setMethod(".panelColor", "ColumnDataTable", function(x) "#B00258")

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".generateTable", "ColumnDataTable", function(x, envir) {
    cmds <-"tab <- as.data.frame(colData(se));"

    if (exists("col_selected", envir=envir, inherits=FALSE)) {
        cmds <- c(cmds, "tab <- tab[rownames(tab) %in% unlist(col_selected),,drop=FALSE]")
    }

    valid.names <- .getCachedCommonInfo(envir$se, "ColumnDataTable")$valid.colData.names
    if (!identical(colnames(colData(envir$se)), valid.names)) {
        cmds <- c(cmds, sprintf("tab <- tab[,%s,drop=FALSE]",
            paste(deparse(valid.names), collapse="\n     ")))
    }

    .textEval(cmds, envir)

    cmds
})

#' @export
setMethod(".definePanelTour", "ColumnDataTable", function(x) {
    rbind(
        c(paste0("#", .getEncodedName(x)), sprintf("The <font color=\"%s\">Column data table</font> panel contains a representation of the <code>colData</code> of our <code>SummarizedExperiment</code> object. Each row here corresponds to a column (i.e., sample) of the <code>SummarizedExperiment</code> while each column of the table is a column metadata variable.", .getPanelColor(x))),
        .addTourStep(x, .dataParamBoxOpen, "The <i>Data parameters</i> box shows the available parameters that can be tweaked in this table.<br/><br/><strong>Action:</strong> click on this box to open up available options."),
        callNextMethod()
    )
})
