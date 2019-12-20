#' The Table class
#'
#' The Table is a virtual class for all panels containing a \code{\link{datatable}} widget from the \pkg{DT} package, where each row \emph{usually} corresponds to a row or column of the \linkS4class{SummarizedExperiment} object.
#' It provides observers for monitoring table selection, global search and column-specific search.
#'
#' @section Slot overview:
#' The following slots control aspects of the \code{DT::datatable} interface:
#' \itemize{
#' \item \code{Selected}, a string containing the row name of the currently selected row.
#' Defaults to \code{NA}, in which case the value should be chosen by the subclass' \code{\link{.refineParameters}} method.
#' \item \code{Search}, a string containing the regular expression for the global search.
#' Defaults to \code{""}, i.e., no search.
#' \item \code{SearchColumns}, a character vector where each entry contains the search string for each column.
#' Defaults to an empty character vector, i.e., no search.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Contract description:
#' The Table will provide interface elements to create the \code{DT::datatble} widget.
#' It will also provide observers to update slots based on user interactions with the widget. 
#' Interface elements and observers are also provided for slots in its parent class \linkS4class{Panel}.
#'
#' Subclasses are expected to implement methods for (at least) \code{\link{.getTableFunction}}.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{Table} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineOutputElement}(x, id)} returns a UI element for a \code{\link[DT]{dataTableOutput}} widget.
#' }
#'
#' For defining reactive expressions:
#' \itemize{
#' \item \code{\link{.createParamObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all of the slots. 
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.createRenderedOutput}(x, se, colormap, output, pObjects, rObjects)} will add a rendered \code{\link{datatable}} object to \code{output}.
#' It will also create a rendered UI element for selection information.
#' }
#'
#' For controlling selections: 
#' \itemize{
#' \item \code{\link{.multiSelectionRestricted}(x)} returns \code{TRUE}.
#' Transmission of a selection to a Table will manifest as a subsetting of the rows.
#' \item \code{\link{.multiSelectionHasActive}(x)} returns a logical scalar indicating whether \code{x} has any active search fields.
#' \item \code{\link{.processSelection}(x, index)} returns a character vector of R expressions that - when evaluated - return a character vector of the row names of the table after applying all search filters.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent class \linkS4class{Panel} are also available.
#'
#' @author Aaron Lun
#' @seealso \linkS4class{Panel}, for the immediate parent class.
#'
#' @name Table-class
#' @aliases
#' initialize,Table-method
#' .createParamObservers,Table-method
#' .createRenderedOutput,Table-method
#' .defineOutputElement,Table-method
#' .hideInterfaceElement,Table-method
#' .multiSelectionCommands,Table-method
#' .multiSelectionHasActive,Table-method
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "Table", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .TableSelected)
    .Object <- .empty_default(.Object, .TableSearch, "")
    .Object
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("Table", function(object) {
    msg <- character(0)

    msg <- .single_string_error(msg, object, .TableSelected)

    msg <- .valid_string_error(msg, object, .TableSearch)

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod(".multiSelectionCommands", "Table", function(x, index) {
    filter_cmds <- .generate_table_filter(x, varname="transmitter")
    if (!is.null(filter_cmds)) {
        sprintf("selected <- rownames(transmitter)[%s]", filter_cmds)
    } else {
        NULL
    }
})

#' @export
setMethod(".multiSelectionHasActive", "Table", function(x) {
    x[[.TableSearch]]!="" || any(x[[.TableColSearch]]!="")
})

#' @export
setMethod(".singleSelectionValue", "Table", function(x, pObjects) {
    x[[.TableSelected]]
})

#' @export
#' @importFrom DT dataTableOutput
setMethod(".defineOutputElement", "Table", function(x, ...) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    tagList(dataTableOutput(panel_name), hr())
})

#' @export
#' @importFrom shiny observeEvent
#' @importFrom utils head
setMethod(".createParamObservers", "Table", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)

    .define_box_observers(panel_name, .selectParamBoxOpen, input, pObjects)

    .define_table_observers(panel_name, input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".createRenderedOutput", "Table", function(x, se, ..., output, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    .define_table_output(mode, id, FUN=.getTableFunction(x),
        se=se, output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".hideInterfaceElement", "Table", function(x, field) {
    if (field %in% .multiSelectHistory) {
        TRUE
    } else {
        callNextMethod()
    }
})
