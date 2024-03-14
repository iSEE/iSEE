#' Retrieve the panel output
#'
#' Retrieve the results of a previous \code{\link{.generateOutput}} call on this panel.
#'
#' @param se A \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param panel_name String containing the panel name.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' The output of running \code{\link{.generateOutput}} for the current panel.
#' Several fields in \code{pObjects} are also modified as a side-effect.
#'
#' @details
#' This function should be used in the rendering expression in \code{\link{.renderOutput}}.
#' It takes care of a number of house-keeping tasks required to satisfy \code{\link{.renderOutput}}'s requirements, e.g., responding to \code{\link{.requestUpdate}} modifications to \code{rObjects}, setting the \code{commands} and \code{contents} and \code{varname} in \code{pObjects}.
#'
#' This function will attempt to retrieve the cached output of \code{\link{.generateOutput}} if it was used elsewhere in the app.
#' After retrieval, the cached value is wiped to ensure that it does not go stale.
#' If no cached value is found, \code{\link{.generateOutput}} is called directly.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{.renderOutput}}, where this function should be called.
#'
#' \code{\link{.generateOutput}}, which is called by this function.
#'
#' @export
#' @rdname retrieveOutput
.retrieveOutput <- function(panel_name, se, pObjects, rObjects) {
    .trackUpdate(panel_name, rObjects)

    if (length(pObjects$cached[[panel_name]])!=0L) {
        output <- pObjects$cached[[panel_name]]
        pObjects$cached[panel_name] <- list(NULL)
    } else {
        curpanel <- pObjects$memory[[panel_name]]
        output <- .generateOutput(curpanel, se, 
            all_memory=pObjects$memory, all_contents=pObjects$contents)
    }

    pObjects$commands[[panel_name]] <- output$commands

    # These can be NULL for non-transmitting panels, hence the single bracket assignment.
    pObjects$contents[panel_name] <- list(output$contents)
    pObjects$varname[panel_name] <- list(output$varname) 

    output
}

#' Request Panel updates
#'
#' Request a re-rendering of the \linkS4class{Panel} output via reactive variables.
#'
#' @param panel_name String containing the panel name.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param update_output A logical scalar indicating whether to call \code{.requestUpdate} as well.
#'
#' @return
#' \code{.requestUpdate} will modify \code{rObjects} to request a re-rendering of the specified panel.
#'
#' \code{.requestCleanUpdate} will also remove all active/saved selections in the chosen panel.
#'
#' \code{.requestActiveSelectionUpdate} will modify \code{rObjects} to indicate that the active multiple selection for \code{panel_name} has changed.
#' If \code{update_output=TRUE}, it will also request a re-rendering of the panel.
#' 
#' All functions will invisibly return \code{NULL}.
#'
#' @details
#' \code{.requestUpdate} should be used in various observers to request a re-rendering of the panel,
#' usually in response to user-driven parameter changes in \code{\link{.createObservers}}.
#'
#' \code{.requestCleanUpdate} is used for changes to protected parameters that invalidate existing multiple selections,
#' e.g., if the coordinates change in a \linkS4class{DotPlot}, existing brushes and lassos are usually not applicable.
#'
#' \code{.requestActiveSelectionUpdate} should be used in the observer expression that implements the panel's multiple selection mechanism.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{.createProtectedParameterObservers}}, for examples where the update-requesting functions are used.
#'
#' @export
#' @rdname requestUpdate
.requestUpdate <- function(panel_name, rObjects) {
    .mark_panel_as_modified(panel_name, character(0), rObjects)
    invisible(NULL)
}

#' @export
#' @rdname requestUpdate
.requestCleanUpdate <- function(panel_name, pObjects, rObjects) {
    accumulated <- character(0)
    if (.multiSelectionHasActive(pObjects$memory[[panel_name]])) {
        pObjects$memory[[panel_name]] <- .multiSelectionClear(pObjects$memory[[panel_name]])
        accumulated <- c(accumulated, .panelReactivated)
    }
    if (.any_saved_selection(pObjects$memory[[panel_name]])) {
        slot(pObjects$memory[[panel_name]], .multiSelectHistory) <- list()
        accumulated <- c(accumulated, .panelResaved)
    }
    .mark_panel_as_modified(panel_name, accumulated, rObjects)
}


#' @export
#' @rdname requestUpdate
.requestActiveSelectionUpdate <- function(panel_name, session, pObjects, rObjects, update_output=TRUE) {
    .safe_reactive_bump(rObjects, paste0(panel_name, "_", .flagMultiSelect))

    modes <- if (update_output) .panelReactivated else c(.panelNorender, .panelReactivated)
    .mark_panel_as_modified(panel_name, modes, rObjects) 

    .update_dynamic_selection_source_panels(panel_name, session, pObjects)
}

#' Updating dynamic selection sources
#'
#' Update all panels that have enabled dynamic multiple selection sources.
#'
#' @param panel_name String containing the name of the current panel.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#'
#' @return
#' Affected panels have their selection sources changed to the current \code{panel_name}.
#' A \code{NULL} is invisibly returned.
#'
#' @details
#' All panels using dynamic sources are modified so that they receive their multiple selections from the current panel.
#' This involves updating the \code{session} to change the chosen source in the UI,
#' which is a reasonably transparent way of plugging into the rest of the reactive machinery for handling multiple selections.
#'
#' However, it requires some work to avoid problems associated with circularity.
#' The general scenario is that the current panel is receiving a multiple selection from another panel that uses a dynamic source.
#' Making a selection on the current panel causes it to become the transmitter for the second panel, leading to circularity.
#' In fact, this is inevitable in the common use case where the current panel is also using a dynamic source.
#'
#' To avoid this phenomenon in the common case, we reset the current panel's source to \code{"---"}. 
#' We also manually edit \code{pObjects$selection_links} so that the current panel is no longer linked to the second panel.
#' This is necessary because the observers for the selection source will respond in arbitrary order to the session update;
#' it is possible for the current panel to still be linked to the previous source when observers for the second panel fire.
#'
#' In short, we delete any existing link before control leaves this function.
#' Doing so here hopefully should not be a problem as \code{\link{.create_multi_selection_choice_observer}}
#' (the observer responding to the change in the source choice)
#' doesn't make any other decisions based on \code{pObjects$selection_links} anyway.
#' 
#' Nothing smart is done to protect against circularity in the more general case.
#' For example, if panel A is dependent on panel B that is dependent on panel C,
#' and C is using a dynamic selection source, any selection made on A will cause a circular dependency.
#' There is no obvious way to break this circularity that is intuitive to the user,
#' so we just pass the buck and rely on downstream warnings to throw warnings.
#'
#' @author Aaron Lun
#' @importFrom shiny updateSelectInput
#' @rdname INTERNAL_update_dynamic_selection_source_panels
.update_dynamic_selection_source_panels <- function(panel_name, session, pObjects) {
    target <- pObjects$memory[[panel_name]]
    dim <- .multiSelectionDimension(target)
    all_affected <- names(pObjects$dynamic_multi_selections[[dim]])
    field <- if (dim=="row") .selectRowSource else .selectColSource
        
    # nocov start
    if (!is.null(session)) {
        if (panel_name %in% all_affected) {
            updateSelectInput(session=session, inputId=paste0(panel_name, "_", field), selected=.noSelection)
            pObjects$selection_links <- .delete_interpanel_link(pObjects$selection_links,
                panel_name, parent_name=slot(target, field), field=field)
            all_affected <- setdiff(all_affected, panel_name)
        }

        for (i in all_affected) {
            updateSelectInput(session=session, inputId=paste0(i, "_", field), selected=panel_name)
        }
    }
    # nocov end

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
#' \code{.trackUpdate} will track whether an update has been requested to the current panel via \code{\link{.requestUpdate}}.
#'
#' \code{.trackSingleSelection} will track whether the single selection in the current panel has changed.
#' Note that this will not cause a reaction if the change involves cancelling a single selection.
#'
#' \code{.trackMultiSelection} will track whether the multiple selections in the current panel have changed.
#' This will respond for both active and saved selections.
#' 
#' \code{.trackRelinkedSelection} will track whether the single or multiple selection sources have changed.
#'
#' These functions should be called within observer or rendering expressions to trigger their evaluation upon panel updates.
#' It is only safe to call these functions within expressions for the same panel, e.g., to synchronize multiple output elements.
#' Calling them with another \code{panel_name} would be unusual, not least because communication between panels is managed by the \code{\link{iSEE}} framework and is outside of the scope of the per-panel observers.
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
