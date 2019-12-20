#' The ColumnTable class
#'
#' The ColumnTable is a virtual class where each column in the \linkS4class{SummarizedExperiment} is represented by a row in a \code{\link{datatable}} widget.
#' It provides observers for monitoring table selection, global search and column-specific search.
#' 
#' @section Slot overview:
#' No new slots are added.
#' All slots provided in the \linkS4class{Table} parent class are available.
#'
#' @section Contract description:
#' The contract for ColumnTables is the same as that for Tables, with the added condition that each row should represent a column of the SummarizedExperiment object.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ColumnTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{Selected} with the first column name of \code{se}.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.hideInterfaceElement}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"SelectRowSource"}, \code{"SelectRowType"} and \code{"SelectRowSaved"}),
#' otherwise it dispatches to the \linkS4class{Panel} method.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createParamObservers}(x, se, input, session, pObjects, rObjects)} sets up observers to propagate changes in the \code{Selected} to linked plots.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"column"} to indicate that a column selection is being transmitted.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent classes \linkS4class{DotPlot} and \linkS4class{Panel} are also available.
#'
#' @seealso
#' \linkS4class{Table}, for the immediate parent class that contains the actual slot definitions.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases 
#' initialize,ColumnTable-method
#' .cacheCommonInfo,ColumnTable-method
#' .refineParameters,ColumnTable-method
#' .defineParamInterface,ColumnTable-method
#' .createParamObservers,ColumnTable-method
#' .hideInterfaceElement,ColumnTable-method
#' .multiSelectionDimension,ColumnTable-method
#' @name ColumnTable-class
NULL

#' @export
setMethod(".refineParameters", "ColumnTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replace_na_with_first(x, .TableSelected, colnames(se))

    x
})

#' @export
setMethod(".createParamObservers", "ColumnTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- paste0(.getEncodedName(x), x[[.organizationId]])
    .define_dimname_propagation_observer(panel_name, choices=colnames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionDimension", "ColumnTable", function(x) "column")

#' @export
setMethod(".hideInterfaceElement", "ColumnTable", function(x, field) {
    if (field %in% c(.selectRowSource, .selectRowType, .selectRowSaved)) {
        TRUE
    } else {
        callNextMethod()
    }
})
