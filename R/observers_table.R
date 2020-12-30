#' Define table-related observers
#'
#' Define observers for responding to table selection and search events.
#'
#' @param panel_name String containing the name of the panel.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @details
#' This needs to plug into the various propagation observers in \code{\link{.create_child_propagation_observer}}
#' and \code{\link{.create_dimname_propagation_observer}} when the search or single selection changes, respectively.
#' Note that the \pkg{iSEE} multiple selection concept is not the same as the DataTable multiple selection;
#' rather, our multiple selections correspond to the search filter.
#'
#' @author Aaron Lun
#' @importFrom shiny observe observeEvent updateSelectInput
#' @rdname INTERNAL_table_observers
.create_table_observers <- function(panel_name, input, session, pObjects, rObjects) {
    # Note that '.int' variables already have underscores, so these are not necessary.
    select_field <- paste0(panel_name, .int_statTableSelected)

    # nocov start
    observeEvent(input[[select_field]], {
        chosen <- input[[select_field]]
        if (length(chosen)==0L) {
            return(NULL)
        }

        tab <- pObjects$contents[[panel_name]]
        if (chosen > nrow(tab)) { 
            # It occasionally happens that the input does not update fast
            # enough when DT changes, see iSEE/iSEE#400. This clause prevents
            # an ugly crash and allows the app to recover.
            return(NULL)
        }

        chosen <- rownames(tab)[chosen]
        previous <- slot(pObjects$memory[[panel_name]], .TableSelected)
        if (chosen==previous) {
            return(NULL)
        }
        slot(pObjects$memory[[panel_name]], .TableSelected) <- chosen

        .safe_reactive_bump(rObjects, paste0(panel_name, "_", .propagateDimnames))
    }, ignoreInit=TRUE)
    # nocov end

    search_field <- paste0(panel_name, .int_statTableSearch)

    # nocov start
    observeEvent(input[[search_field]], {
        search <- input[[search_field]]
        if (identical(search, slot(pObjects$memory[[panel_name]], .TableSearch))) {
            return(NULL)
        }

        slot(pObjects$memory[[panel_name]], .TableSearch) <- search
        .requestActiveSelectionUpdate(panel_name, session, pObjects, rObjects, update_output=FALSE)
     }, ignoreInit=TRUE)
     # nocov end

    colsearch_field <- paste0(panel_name, .int_statTableColSearch)

    # nocov start
    observeEvent(input[[colsearch_field]], {
        search <- input[[colsearch_field]]
        past <- slot(pObjects$memory[[panel_name]], .TableColSearch)
        if (identical(search, past)) {
            return(NULL)
        }

        slot(pObjects$memory[[panel_name]], .TableColSearch) <- search

        if (all(search=="") && all(past=="")) {
            # No update in cases with variable numbers of columns where no
            # selection was performed (assuming rows were the same).
            return(NULL)
        }

        .requestActiveSelectionUpdate(panel_name, session, pObjects, rObjects, update_output=FALSE)
    }, ignoreInit=TRUE)
    # nocov end

    tabupdate_field <- paste0(panel_name, "_", .flagTableUpdate)
    .safe_reactive_init(rObjects, tabupdate_field)

    # nocov start
    observeEvent(rObjects[[tabupdate_field]], {
        updateSelectInput(session, paste0(panel_name, "_", .TableHidden),
            selected=slot(pObjects$memory[[panel_name]], .TableHidden),
            choices=colnames(pObjects$contents[[panel_name]]))
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}
