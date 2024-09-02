#' The Table class
#'
#' The Table is a virtual class for all panels containing a \code{\link{datatable}} widget from the \pkg{DT} package, where each row \emph{usually} corresponds to a row or column of the \linkS4class{SummarizedExperiment} object.
#' It provides observers for rendering the table widget, monitoring single selections, and applying global and column-specific searches (which serve as multiple selections).
#'
#' @section Slot overview:
#' The following slots control aspects of the \code{DT::datatable} selection:
#' \itemize{
#' \item \code{Selected}, a string containing the name of the currently selected row of the data.frame.
#' Defaults to \code{NA}, in which case the value should be chosen by the subclass' \code{\link{.refineParameters}} method.
#' \item \code{Search}, a string containing the regular expression for the global search.
#' Defaults to \code{""}, i.e., no search.
#' \item \code{SearchColumns}, a unnamed character vector of length equal to the number of columns of the data.frame,
#' where each entry contains the search string for its corresponding column.
#' Alternatively, a character vector of variable length, containing search strings for one or more columns.
#' Defaults to an character vector of length zero, which is internally expanded to an vector of zero-length strings, i.e., no search.
#' }
#'
#' The following slots control the appearance of the table:
#' \itemize{
#' \item \code{HiddenColumns}, a character vector containing names of columns to hide.
#' Defaults to an empty vector.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{Table} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineOutput}(x)} returns a UI element for a \code{\link[DT]{dataTableOutput}} widget.
#' \item \code{\link{.defineDataInterface}(x)} will create interface elements for modifying the table,
#' namely to choose which columns to hide.
#' Note that this is populated by \code{\link{.generateOutput}} upon table rendering,
#' as we do not know the available columns before that point.
#' }
#'
#' For defining reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all of the slots.
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.renderOutput}(x, se, output, pObjects, rObjects)} will add a rendered \code{\link{datatable}} object to \code{output}.
#' This will also call the equivalent \linkS4class{Panel} method to render the panel information text boxes.
#' \item \code{\link{.generateOutput}(x, se, all_memory, all_contents)} returns a list containing \code{contents}, a data.frame with one row per point currently present in the table;
#' \code{commands}, a list of character vector containing the R commands required to generate \code{contents} and \code{plot};
#' and \code{varname}, a string specifying the name of the variable in \code{commands} used to generate \code{contents}.
#' \item \code{\link{.exportOutput}(x, se, all_memory, all_contents)} will create a CSV file containing the current table, and return a string containing the path to that file. 
#' This assumes that the \code{contents} field returned by \code{\link{.generateOutput}} is a data.frame or can be coerced into one.
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
#' \item \code{\link{.singleSelectionValue}(x, contents)} returns the name of the row that was last selected in the \code{\link{datatable}} widget.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a tour relevant to subclasses,
#' mostly describing the effect of selection from other panels and the use of row filters to transmit selections.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent class \linkS4class{Panel} are also available.
#'
#' @section Subclass expectations:
#' The Table is a rather vaguely defined class for which the only purpose is to avoid duplicating code for \linkS4class{ColumnDotPlot}s and \linkS4class{RowDotPlot}s.
#' We recommend extending those subclasses instead.
#'
#' @author Aaron Lun
#' @seealso \linkS4class{Panel}, for the immediate parent class.
#'
#' @name Table-class
#' @aliases
#' initialize,Table-method
#' .refineParameters,Table-method
#' .createObservers,Table-method
#' .generateOutput,Table-method
#' .renderOutput,Table-method
#' .defineOutput,Table-method
#' .exportOutput,Table-method
#' .hideInterface,Table-method
#' .multiSelectionCommands,Table-method
#' .multiSelectionActive,Table-method
#' .multiSelectionRestricted,Table-method
#' .singleSelectionValue,Table-method
#' .definePanelTour,Table-method
#' .defineDataInterface,Table-method
#' updateObject,Table-method
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "Table", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .TableSelected, NA_character_)
    args <- .emptyDefault(args, .TableSearch, "")
    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("Table", function(object) {
    msg <- character(0)

    msg <- .singleStringError(msg, object, .TableSelected)

    msg <- .validStringError(msg, object, .TableSearch)

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod(".refineParameters", "Table", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    # TODO: move HiddenColumn checks here.

    x
})

#' @export
setMethod(".multiSelectionCommands", "Table", function(x, index) {
    search <- slot(x, .TableSearch)
    searchcols <- slot(x, .TableColSearch)
    sprintf("selected <- rownames(contents)[iSEE::filterDT(contents, global=%s,\n    column=%s)]",
        deparse(search),
        .deparse_for_viewing(searchcols, indent=2))
})

#' @export
setMethod(".multiSelectionActive", "Table", function(x) {
    if (slot(x, .TableSearch)!="" || any(slot(x, .TableColSearch)!="")) {
        list(Search=slot(x, .TableSearch), ColumnSearch=slot(x, .TableColSearch))
    } else {
        NULL
    }
})

#' @export
setMethod(".multiSelectionRestricted", "Table", function(x) TRUE)

#' @export
setMethod(".singleSelectionValue", "Table", function(x, contents) {
    slot(x, .TableSelected)
})

#' @export
#' @importFrom DT dataTableOutput
setMethod(".defineOutput", "Table", function(x) {
    tagList(
        dataTableOutput(.getEncodedName(x)), 
        uiOutput(paste0(.getEncodedName(x), "_", .tableExtraInfo)),
        hr()
    )
})

#' @export
#' @importFrom utils head
setMethod(".createObservers", "Table", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- .getEncodedName(x)

    .create_table_observers(panel_name, input=input,
        session=session, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(.getEncodedName(x), .TableHidden, input,
        pObjects, rObjects, ignoreNULL=FALSE)
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
#' @importFrom utils write.csv
setMethod(".exportOutput", "Table", function(x, se, all_memory, all_contents) {
    contents <- .generateOutput(x, se, all_memory=all_memory, all_contents=all_contents)
    newpath <- paste0(.getEncodedName(x), ".csv")
    write.csv(file=newpath, contents$contents)
    newpath
})

#' @export
setMethod(".defineDataInterface", "Table", function(x, se, select_info) {
    hidden <- slot(x, .TableHidden)

    .addSpecificTour(class(x), .TableHidden, function(tab_name) {
        data.frame(
            element=paste0("#", tab_name, "_", .TableHidden, " + .selectize-control"),
            intro="Here, we can hide particular columns in the table.
This is helpful for hiding uninformative annotations so that we don't have to keep on scrolling left/right to see the interesting bits.
Any number of column names can be specified here."
        )
    })

    c(
        callNextMethod(),
        list(
            # At this point, we don't know the full set of column names. So,
            # 'choices' needs to be initialized with the current values, even
            # if it is updated later by observers upon table initialization.
            .selectInput.iSEE(x, .TableHidden,
                choices=hidden, selected=hidden,
                label="Hidden columns:", multiple=TRUE)
        )
    )
})

#' @export
setMethod(".hideInterface", "Table", function(x, field) {
    if (field %in% c(.multiSelectHistory, .selectColumnRestrict, .selectRowRestrict)) {
        # Tables always restrict.
        TRUE
    } else {
        callNextMethod()
    }
})

#' @export
setMethod(".definePanelTour", "Table", function(x) {
    mdim <- .multiSelectionDimension(x)
    rbind(
        callNextMethod(),
        c(paste0("#", .getEncodedName(x)), sprintf("At the other end of the spectrum, we can apply filters to the table to select rows corresponding to %ss of the <code>SummarizedExperiment</code> object; these will be transmitted to other panels that choose this one as their selection source.<br/><br/>We can filter by individual columns of the table and/or with a regular expression search to any matching string in the table.<br/><br/>We can also click on individual rows of the table to transmit a single %s selection to other panels.", mdim, .singleSelectionDimension(x)))
    )
})

#' @export
#' @importFrom BiocGenerics updateObject
setMethod("updateObject", "Table", function(object, ..., verbose=FALSE) {
    if (!.is_latest_version(object)) {
        # nocov start

        # Do this before 'callNextMethod()', which fills in the Restrict.
        update.2.1 <- is(try(slot(object, .plotHoverInfo), silent=TRUE), "try-error")

        # NOTE: it is crucial that updateObject does not contain '[[' or '[[<-'
        # calls, lest we get sucked into infinite recursion with the calls to
        # 'updateObject' from '[['.
        object <- callNextMethod()

        # Backwards compatibility for new slots (added 3.12).
        if (update.2.1){ 
            .Deprecated(msg=sprintf("detected outdated '%s' instance, run 'updateObject(<%s>)'", class(object)[1], class(object)[1]))
            object[[.TableHidden]] <- character(0)
        }
        # nocov end
    }

    object
})
