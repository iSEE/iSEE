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
#' Subclasses are expected to implement methods for (at least) \code{\link{.generateTable}}.
#' See below for more details.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{Table} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineOutput}(x, id)} returns a UI element for a \code{\link[DT]{dataTableOutput}} widget.
#' }
#'
#' For defining reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all of the slots. 
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.renderOutput}(x, se, output, pObjects, rObjects)} will add a rendered \code{\link{datatable}} object to \code{output}.
#' This will also call the equivalent \linkS4class{Panel} method to render the panel information testboxes.
#' }
#'
#' For controlling selections: 
#' \itemize{
#' \item \code{\link{.multiSelectionRestricted}(x)} returns \code{TRUE}.
#' Transmission of a selection to a Table will manifest as a subsetting of the rows.
#' \item \code{\link{.multiSelectionActive}(x)} returns a list containing the contents of \code{x[["Search"]]} and \code{x[["ColumnSearch"]]}.
#' If both contain only empty strings, a \code{NULL} is returned instead.
#' \item \code{\link{.multiSelectionCommands}(x, index)} returns a character vector of R expressions that - when evaluated - return a character vector of the row names of the table after applying all search filters.
#' The value of \code{index} is ignored.
#' \item \code{\link{.singleSelectionValue}(x, pObjects)} returns the name of the row that was last selected in the \code{\link{datatable}} widget.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent class \linkS4class{Panel} are also available.
#'
#' @section Constructing the table:
#' Developers of Table subclasses do not need to explicitly define \code{\link{.generateOutput}} or \code{\link{.renderOutput}} if they instead specialize \code{\link{.generateTable}}.
#' This generic is used to construct the data.frame that is supplied to the \code{\link{datatable}} widget in \code{\link{.renderOutput,Table-method}}.
#' 
#' In \code{.generateTable(x, envir)}, the following arguments are required:
#' \itemize{
#' \item \code{x}, an instance of a Table subclass.
#' \item \code{envir}, the evaluation environment in which the data.frame is to be constructed.
#' This can be assumed to have \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset;
#' possibly \code{col_selected}, if a multiple column selection is being transmitted to \code{x};
#' and possibly \code{row_selected}, if a multiple row selection is being transmitted to \code{x}.
#' }
#'
#' In return, the method should add a \code{tab} variable in \code{envir} containing the relevant data.frame.
#' This will automatically be passed to the \code{\link{datatable}} widget as well as being stored in \code{pObjects$contents}.
#' The return value should be a character vector of commands that produces \code{tab} when evaluated in \code{envir}.
#'
#' Using a \linkS4class{RowTable} or \linkS4class{ColumnTable} will impose more constraints on the nature of the rows of the generated data.frame; refer to the relevant documentation on these classes for more details.
#'
#' The number and names of the columns of the data.frame should be fixed for all calls to \code{.generateTable}.
#' Violating this principle will result in unpredictable interactions with stored values in the \code{SearchColumns} slot.
#' 
#' Any internal variables that are generated by the commands should be prefixed with \code{.} to avoid potential clashes with reserved variable names in the rest of the application.
#' 
#' @author Aaron Lun
#' @seealso \linkS4class{Panel}, for the immediate parent class.
#'
#' @name Table-class
#' @aliases
#' initialize,Table-method
#' .createObservers,Table-method
#' .generateOutput,Table-method
#' .renderOutput,Table-method
#' .defineOutput,Table-method
#' .hideInterface,Table-method
#' .multiSelectionCommands,Table-method
#' .multiSelectionActive,Table-method
#' .singleSelectionValue,Table-method
#' .generateTable
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "Table", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .TableSelected, NA_character_)
    args <- .empty_default(args, .TableSearch, "")
    do.call(callNextMethod, c(list(.Object), args))
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
    filter_cmds <- .generate_table_filter(x, varname="contents")
    if (!is.null(filter_cmds)) {
        sprintf("selected <- rownames(contents)[%s]", filter_cmds)
    } else {
        NULL
    }
})

#' @export
setMethod(".multiSelectionActive", "Table", function(x) {
    if (x[[.TableSearch]]!="" || any(x[[.TableColSearch]]!="")) {
        list(Search=x[[.TableSearch]], ColumnSearch=x[[.TableColSearch]]) 
    } else {
        NULL
    }
})

#' @export
setMethod(".singleSelectionValue", "Table", function(x, pObjects) {
    x[[.TableSelected]]
})

#' @export
#' @importFrom DT dataTableOutput
setMethod(".defineOutput", "Table", function(x, ...) {
    tagList(dataTableOutput(.getEncodedName(x)), hr())
})

#' @export
#' @importFrom shiny observeEvent
#' @importFrom utils head
setMethod(".createObservers", "Table", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- .getEncodedName(x)

    .create_table_observers(panel_name, input=input, 
        session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
#' @importFrom SummarizedExperiment colData
setMethod(".renderOutput", "Table", function(x, se, ..., output, pObjects, rObjects) {
    .create_table_output(.getEncodedName(x), se=se, output=output, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".generateOutput", "Table", function(x, se, ..., all_memory, all_contents) {
    .define_table_commands(x, se, all_memory=all_memory, all_contents=all_contents)
})

#' @export
setMethod(".hideInterface", "Table", function(x, field) {
    if (field %in% .multiSelectHistory) {
        TRUE
    } else {
        callNextMethod()
    }
})
