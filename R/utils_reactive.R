#' Reactive manipulations for Panel output
#'
#' Respond to or request a re-rendering of the \linkS4class{Panel} output via reactive variables.
#'
#' @param se A \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param panel_name String containing the panel name.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param update_output A logical scalar indicating whether to call \code{.requestUpdate} as well.
#'
#' @return
#' \code{.retrieveOutput} will return the output of running \code{\link{.generateOutput}} for the current panel.
#'
#' \code{.requestUpdate} will modify \code{rObjects} to request a re-rendering of the specified panel.
#' \code{.requestCleanUpdate} will also remove all active/saved selections in the chosen panel.
#'
#' \code{.requestActiveSelectionUpdate} will modify \code{rObjects} to indicate that the active multiple selection for \code{panel_name} has changed.
#' If \code{update_output=TRUE}, it will also call request a re-rendering of the panel.
#' 
#' All \code{.request*} functions will invisibly return \code{NULL}.
#'
#' @details
#' \code{.retrieveOutput} should be used in the expression for rendering output, e.g., in \code{\link{.renderOutput}}.
#' This takes care of a number of house-keeping tasks required to satisfy \code{\link{.renderOutput}}'s requirements
#' with respect to updating various fields in \code{pObjects}.
#' It also improves efficiency by retrieving cached outputs that were used elsewhere in the app.
#'
#' \code{.requestUpdate} should be used in various observers to request a re-rendering of the panel,
#' usually in response to user-driven parameter changes in \code{\link{.createObservers}}.
#'
#' \code{.requestCleanUpdate} is used for changes to protected parameters that invalidate existing multiple selections,
#' e.g., if the coordinates change in a \linkS4class{DotPlot}, existing brushes and lassos are usually not applicable.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{.createProtectedParameterObservers}}, for examples where the update-requesting functions are used.
#'
#' @export
#' @rdname retrieveOutput
.retrieveOutput <- function(panel_name, se, pObjects, rObjects) {
    .trackUpdate(panel_name, rObjects)

    if (length(pObjects$cached[[panel_name]])!=0L) {
        output <- pObjects$cached[[panel_name]]
        pObjects$cached[panel_name] <- list(NULL)
    } else {
        output <- .generateOutput(pObjects$memory[[panel_name]], se, 
            all_memory=pObjects$memory, all_contents=pObjects$contents)
    }

    pObjects$commands[[panel_name]] <- output$commands
    pObjects$contents[[panel_name]] <- output$contents

    output
}

#' @export
#' @rdname retrieveOutput
.requestUpdate <- function(panel_name, rObjects) {
    .mark_panel_as_modified(panel_name, character(0), rObjects)
    invisible(NULL)
}

#' @export
#' @rdname retrieveOutput
.requestCleanUpdate <- function(panel_name, pObjects, rObjects) {
    accumulated <- character(0)
    if (.multiSelectionHasActive(pObjects$memory[[panel_name]])) {
        pObjects$memory[[panel_name]] <- .multiSelectionClear(pObjects$memory[[panel_name]])
        accumulated <- c(accumulated, .panelReactivated)
    }
    if (.any_saved_selection(pObjects$memory[[panel_name]])) {
        pObjects$memory[[panel_name]][[.multiSelectHistory]] <- list()
        accumulated <- c(accumulated, .panelResaved)
    }
    .mark_panel_as_modified(panel_name, accumulated, rObjects)
}

#' @export
#' @rdname retrieveOutput
#' @importFrom igraph adjacent_vertices get.edge.ids E
#' @importFrom shiny updateSelectInput
.requestActiveSelectionUpdate <- function(panel_name, session, pObjects, rObjects, update_output=TRUE) {
    .safe_reactive_bump(rObjects, paste0(panel_name, "_", .flagMultiSelect))
    .mark_panel_as_modified(panel_name,
        if (update_output) .panelReactivated else c(.panelNorender, .panelReactivated), 
        rObjects)

    # Handling the panels that respond to global selections.
    target <- pObjects$memory[[panel_name]]
    dim <- .multiSelectionDimension(target)
    src <- if (dim=="row") .selectRowGlobal else .selectColGlobal

    if (target[[src]]) {
        all_affected <- pObjects$global_panels[[dim]]
        field <- if (dim=="row") .selectRowSource else .selectColSource
        
        if (!is.null(session)) { # put here to avoid attempting to test it.
            # nocov start
            for (i in setdiff(all_affected, panel_name)) {
                updateSelectInput(session=session, inputId=paste0(i, "_", field), selected=panel_name)
            }
            updateSelectInput(session=session, inputId=paste0(panel_name, "_", field), selected=.noSelection)
            # nocov end
        }
    }

    invisible(NULL)
}

#' Track internal events
#'
#' Utility functions to track internal events for a panel by monitoring the status of reactive variables in \code{rObjects}.
#'
#' @param panel_name String containing the panel name.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' All functions will cause the current reactive context to respond to the designated event.
#' \code{NULL} is returned invisibly.
#'
#' @details
#' \code{.trackUpdate} will track whether an update has been requested to the current panel
#' (see also \code{\link{.requestUpdate}}).
#'
#' \code{.trackSingleSelection} will track whether the single selection in the current panel has changed.
#' Note that this will not cause a reaction if the change involves cancelling a single selection.
#'
#' \code{.trackMultiSelection} will track whether the multiple selections in the current panel have changed.
#' This will respond for both active and saved selections.
#' 
#' \code{.trackRelinkedSelection} will track whether the single or multiple selection sources have changed.
#' 
#' @author Aaron Lun
#'
#' @name track-utils
NULL

#' @export
#' @rdname track-utils
.trackUpdate <- function(panel_name, rObjects) {
    force(rObjects[[paste0(panel_name, "_", .flagOutputUpdate)]])
}

#' @export
#' @rdname track-utils
.trackSingleSelection <- function(panel_name, rObjects) {
    force(rObjects[[paste0(panel_name, "_", .flagSingleSelect)]])
}

#' @export
#' @rdname track-utils
.trackMultiSelection <- function(panel_name, rObjects) {
    force(rObjects[[paste0(panel_name, "_", .flagMultiSelect)]])
}

#' @export
#' @rdname track-utils
.trackRelinkedSelection <- function(panel_name, rObjects) {
    force(rObjects[[paste0(panel_name, "_", .flagRelinkedSelect)]])
}

#' Safely use reactive values
#'
#' Initialize and bump reactive variables in a manner that avoids errors if they were not already present in \code{rObjects}.
#' Also avoids creation of links in the Shiny reactive graph when we are only writing to these reactive variables.
#'
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param field String containing the name of the reactive variable.
#' @param value Integer scalar containing the initial value of the variable.
#' @param max Integer scalar specifying the maximum value of the variable, see \code{\link{.increment_counter}}.
#'
#' @return \code{.safe_reactive_init} will add \code{field} to \code{rObjects} with value \code{value},
#' but only if it was not already present; otherwise this is a no-op.
#' It returns \code{rObjects} invisibly.
#'
#' \code{.safe_reactive_bump} will increment \code{field} in \code{rObjects}, initializing it if it was not already present.
#' It returns the incremented value invisibly.
#' 
#' @author Aaron Lun
#' 
#' @rdname INTERNAL_safe_reactive
#' @importFrom shiny isolate
.safe_reactive_init <- function(rObjects, field, value=1L) {
    if (!field %in% isolate(names(rObjects))) {
        rObjects[[field]] <- value
    }
    invisible(rObjects)
}

#' @rdname INTERNAL_safe_reactive
#' @importFrom shiny isolate
.safe_reactive_bump <- function(rObjects, field, max=10000L) {
    .safe_reactive_init(rObjects, field)
    counter <- isolate(rObjects[[field]]) + 1L
    if (counter >= max) {
        counter <- 0L
    }
    rObjects[[field]] <- counter
    invisible(counter)
}

#' Increment a counter
#'
#' Increments the counter for a reactive value to trigger downstream updates.
#'
#' @param counter An integer scalar, usually an isolated reactive variable.
#' @param max An integer scalar specifying the upper bound for the increment.
#'
#' @return Integer scalar of value equal to \code{counter+1L} if this is not greater than \code{max}; zero otherwise.
#'
#' @details
#' This function is primarily designed to increment reactive values to trigger downstream observers, conductors or UI endpoints.
#' The use of \code{max} avoids an integer overflow in (very!) long-running apps.
#'
#' Technically we could have flipped a logical flag instead.
#' The initial worry was that if one observer flips the flag and another observer flips it back, there wouldn't be any net change to trigger downstream events.
#' This is probably not the case, as Shiny links get invalidated upon any change to a reactive value, but nonetheless, here we are.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_increment_counter
#' @seealso
#' \code{\link{iSEE}}
.increment_counter <- function(counter, max=10000L) {
    counter <- counter + 1L
    if (counter >= max) {
        counter <- 0L
    }
    return(counter)
}
