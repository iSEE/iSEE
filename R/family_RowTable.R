#' The RowTable class
#'
#' The RowTable is a virtual class where each row in the \linkS4class{SummarizedExperiment} is represented by no more than one row in a \code{\link{datatable}} widget.
#' In panels of this class, single and multiple selections can only be transmitted on the features.
#' 
#' @section Slot overview:
#' No new slots are added.
#' All slots provided in the \linkS4class{Table} parent class are available.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RowTable} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{Selected} with the first row name of \code{se}.
#' This will also call the equivalent \linkS4class{Table} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.hideInterface}(x, field)} returns a logical scalar indicating whether the interface element corresponding to \code{field} should be hidden.
#' This returns \code{TRUE} for column selection parameters (\code{"ColumnSelectionSource"} and \code{"ColumnSelectionRestrict"}),
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
#' \item \code{\link{.singleSelectionDimension}(x)} returns \code{"feature"} to indicate that a feature identity is being transmitted.
#' }
#'
#' For rendering output:
#' \itemize{
#' \item \code{\link{.showSelectionDetails}(x)} returns a HTML element containing details about the selected row.
#' This requires a function to be registered by \code{\link{registerAppOptions}} under the option name \code{"RowTable.select.details"}.
#' The function should take a string containing the name of a feature (i.e., the current selection in the \linkS4class{RowTable}) and returns a HTML element.
#' If no function is registered, \code{NULL} is returned.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent classes \linkS4class{DotPlot} and \linkS4class{Panel} are also available.
#'
#' @section Subclass expectations:
#' Subclasses are expected to implement methods for:
#' \itemize{
#' \item \code{\link{.generateTable}}
#' \item \code{\link{.fullName}}
#' \item \code{\link{.panelColor}}
#' }
#'
#' The method for \code{\link{.generateTable}} should create a \code{tab} data.frame where each row corresponds to a row in the \linkS4class{SummarizedExperiment} object.
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
#' .multiSelectionResponsive,RowTable-method
#' .singleSelectionDimension,RowTable-method
#' .showSelectionDetails,RowTable-method
#' @name RowTable-class
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RowTable", function(.Object, ...) {
    args <- list(...)

    # Defensive measure to avoid problems with cyclic graphs 
    # that the user doesn't have permissions to change!
    args <- .emptyDefault(args, .selectColumnDynamic, FALSE)

    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
setMethod(".refineParameters", "RowTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, .TableSelected, rownames(se))

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
    if (field %in% c(.selectColumnSource, .selectColumnRestrict, .selectColumnDynamic)) {
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".multiSelectionDimension", "RowTable", function(x) "row")

#' @export
setMethod(".singleSelectionDimension", "RowTable", function(x) "feature")

#' @export
setMethod(".multiSelectionResponsive", "RowTable", function(x, dims = character(0)) {
    if ("row" %in% dims) {
        return(TRUE)
    }
    return(FALSE)
})

#' @export
setMethod(".showSelectionDetails", "RowTable", function(x) {
    FUN <- getAppOption("RowTable.select.details")
    if (!is.null(FUN)) {
        FUN(slot(x, .TableSelected))
    }
})
