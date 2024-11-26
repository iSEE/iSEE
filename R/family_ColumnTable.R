#' The ColumnTable class
#'
#' The ColumnTable is a virtual class where each column in the \linkS4class{SummarizedExperiment} is represented by no more than row in a \code{\link{datatable}} widget.
#' In panels of this class, single and multiple selections can only be transmitted on the samples.
#' 
#' @section Slot overview:
#' No new slots are added.
#' All slots provided in the \linkS4class{Table} parent class are available.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ColumnTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{Selected} with the first column name of \code{se}.
#' This will also call the equivalent \linkS4class{Table} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for row selection parameters (\code{"RowSelectionSource"} and \code{"RowSelectionRestrict"}),
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
#' \item \code{\link{.multiSelectionDimension}(x)} returns \code{"column"} to indicate that a column selection is being transmitted.
#' \item \code{\link{.singleSelectionDimension}(x)} returns \code{"sample"} to indicate that a sample identity is being transmitted.
#' }
#'
#' For rendering output:
#' \itemize{
#' \item \code{\link{.showSelectionDetails}(x)} returns a HTML element containing details about the selected row.
#' This requires a function to be registered by \code{\link{registerAppOptions}} under the option name \code{"ColumnTable.select.details"}.
#' The function should take a string containing the name of a feature (i.e., the current selection in the \linkS4class{ColumnTable}) and returns a HTML element.
#' If no function is registered, \code{NULL} is returned.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent classes \linkS4class{Table} and \linkS4class{Panel} are also available.
#'
#' @section Subclass expectations:
#' Subclasses are expected to implement methods for:
#' \itemize{
#' \item \code{\link{.generateTable}}
#' \item \code{\link{.fullName}}
#' \item \code{\link{.panelColor}}
#' }
#'
#' The method for \code{\link{.generateTable}} should create a \code{tab} data.frame where each row corresponds to a column in the \linkS4class{SummarizedExperiment} object.
#'
#' @seealso
#' \linkS4class{Table}, for the immediate parent class that contains the actual slot definitions.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases 
#' initialize,ColumnTable-method
#' .refineParameters,ColumnTable-method
#' .defineInterface,ColumnTable-method
#' .createObservers,ColumnTable-method
#' .hideInterface,ColumnTable-method
#' .multiSelectionDimension,ColumnTable-method
#' .multiSelectionResponsive,ColumnTable-method
#' .singleSelectionDimension,ColumnTable-method
#' .showSelectionDetails,ColumnTable-method
#' @name ColumnTable-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "ColumnTable", function(.Object, ...) {
    args <- list(...)

    # Defensive measure to avoid problems with cyclic graphs 
    # that the user doesn't have permissions to change!
    args <- .emptyDefault(args, .selectRowDynamic, FALSE)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
setMethod(".refineParameters", "ColumnTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, .TableSelected, colnames(se))

    x
})

#' @export
setMethod(".createObservers", "ColumnTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    .create_dimname_propagation_observer(.getEncodedName(x), choices=colnames(se),
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".multiSelectionDimension", "ColumnTable", function(x) "column")

#' @export
setMethod(".singleSelectionDimension", "ColumnTable", function(x) "sample")

#' @export
setMethod(".multiSelectionResponsive", "ColumnTable", function(x, dim) dim == "column")

#' @export
setMethod(".hideInterface", "ColumnTable", function(x, field) {
    if (field %in% c(.selectRowSource, .selectRowRestrict, .selectRowDynamic)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".showSelectionDetails", "ColumnTable", function(x) {
    FUN <- getAppOption("ColumnTable.select.details")
    if (!is.null(FUN)) {
        FUN(slot(x, .TableSelected))
    }
})
