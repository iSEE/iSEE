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
#' @importFrom shiny observe observeEvent 
#' @rdname INTERNAL_table_observers
.create_table_observers <- function(panel_name, input, session, pObjects, rObjects) {
    # Note that '.int' variables already have underscores, so these are not necessary.
    select_field <- paste0(panel_name, .int_statTableSelected)
    observeEvent(input[[select_field]], {
        chosen <- input[[select_field]]
        if (length(chosen)==0L) {
            return(NULL)
        }

        chosen <- rownames(pObjects$contents[[panel_name]])[chosen]
        previous <- pObjects$memory[[panel_name]][[.TableSelected]]
        if (chosen==previous) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[.TableSelected]] <- chosen

        .safe_reactive_bump(rObjects, paste0(panel_name, "_", .propagateDimnames))
    })

    search_field <- paste0(panel_name, .int_statTableSearch)
    gen_name <- paste0(panel_name, "_", .panelGeneralInfo)
    observeEvent(input[[search_field]], {
        search <- input[[search_field]]
        if (identical(search, pObjects$memory[[panel_name]][[.TableSearch]])) {
            return(NULL)
        }

        pObjects$memory[[panel_name]][[.TableSearch]] <- search
        .mark_panel_as_modified(panel_name, c(.panelNorender, .panelReactivated), rObjects)
        .safe_reactive_bump(rObjects, gen_name)
    })

    colsearch_field <- paste0(panel_name, .int_statTableColSearch)
    observeEvent(input[[colsearch_field]], {
        search <- input[[colsearch_field]]
        if (identical(search, pObjects$memory[[panel_name]][[.TableColSearch]])) {
            return(NULL)
        }

        pObjects$memory[[panel_name]][[.TableColSearch]] <- search
        .mark_panel_as_modified(panel_name, c(.panelNorender, .panelReactivated), rObjects)
        .safe_reactive_bump(rObjects, gen_name)
    })
}
