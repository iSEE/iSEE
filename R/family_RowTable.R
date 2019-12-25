#' The RowTable class
#'
#' The RowTable is a virtual class where each row in the \linkS4class{SummarizedExperiment} is represented by a row in a \code{\link{datatable}} widget.
#' It provides observers for monitoring table selection, global search and column-specific search.
#' 
#' @section Slot overview:
#' No new slots are added.
#' All slots provided in the \linkS4class{Table} parent class are available.
#'
#' @section Contract description:
#' The contract for RowTables is the same as that for Tables, with the added condition that each row should represent a row of the SummarizedExperiment object.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{Selected} with the first row name of \code{se}.
#' This will also call the equivalent \linkS4class{DotPlot} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for column selection parameters (\code{"SelectColSource"}, \code{"SelectColType"} and \code{"SelectColSaved"}),
#' otherwise it dispatches to the \linkS4class{Panel} method.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers to propagate changes in the \code{Selected} to linked plots.
#' This will also call the equivalent \linkS4class{Table} method.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"row"} to indicate that a row selection is being transmitted.
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
#' initialize,RowTable-method
#' .refineParameters,RowTable-method
#' .defineInterface,RowTable-method
#' .createObservers,RowTable-method
#' .hideInterface,RowTable-method
#' .multiSelectionDimension,RowTable-method
#' @name RowTable-class
NULL

#' @export
setMethod(".refineParameters", "RowTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replace_na_with_first(x, .TableSelected, rownames(se))

    x
})

#' @export
setMethod(".createObservers", "RowTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    .create_dimname_propagation_observer(.getEncodedName(x), choices=rownames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".hideInterface", "RowTable", function(x, field) {
    if (field %in% c(.selectColSource, .selectColType, .selectColSaved)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".multiSelectionDimension", "RowTable", function(x) "row")
