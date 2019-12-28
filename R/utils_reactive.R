#' Reactive manipulations for Panel output
#'
#' Respond to or request a re-rendering of the \linkS4class{Panel} output via reactive variables.
#'
#' @param panel_name String containing the panel name.
#' @param se A \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param pObjects An environment containing \code{memory}, a list of \linkS4class{Panel}s containing parameters for each panel;
#' \code{contents}, a list of panel-specific contents to be used to determine the selection values;
#' and \code{cached}, a list of panel-specific cached values to be used during rendering.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' \code{.respondPanelOutput} will return the output of running \code{\link{.generateOutput}} for the current panel.
#' It will also register the use of the \code{panel_name} reactive variable in \code{rObjects}.
#'
#' \code{.refreshPanelOutput} will bump the \code{panel_name} reactive variable in \code{rObjects}.
#' \code{.refreshPanelOutputUnselected} will also remove all selections in the chosen panel.
#'
#' Both functions will invisibly return \code{NULL}.
#'
#' @details
#' \code{.respondPanelOutput} should be used in the expression for rendering output, e.g., in \code{\link{.renderOutput}}.
#' This ensures that this expression is re-evaluated upon requested re-rendering of the panel
#'
#' \code{.refreshPanelOutput} should be used in various observers to request a re-rendering of the panel,
#' usually in response to user-driven parameter changes in \code{\link{.createObservers}}.
#'
#' \code{.refreshPanelOutputUnselected} is usually desirable for parameter changes that invalidate previous selections,
#' e.g., if the coordinates change in a \linkS4class{DotPlot}, existing brushes and lassos are usually not applicable.
#'
#' @author Aaron Lun
#'
#' @export
#' @rdname respondPanelOutput
.respondPanelOutput <- function(panel_name, se, pObjects, rObjects) {
    force(rObjects[[panel_name]])

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
#' @rdname respondPanelOutput
.refreshPanelOutput <- function(panel_name, rObjects) {
    .mark_panel_as_modified(panel_name, character(0), rObjects)
    invisible(NULL)
}

#' @export
#' @rdname respondPanelOutput
.refreshPanelOutputUnselected <- function(panel_name, pObjects, rObjects) {
    has_active <- .multiSelectionHasActive(pObjects$memory[[panel_name]])
    has_saved <- .any_saved_selection(pObjects$memory[[panel_name]])

    accumulated <- character(0)
    if (has_active) {
        pObjects$memory[[panel_name]] <- .multiSelectionClear(pObjects$memory[[panel_name]])
        accumulated <- c(accumulated, .panelReactivated)
    }
    if (has_saved) {
        pObjects$memory[[panel_name]][[.multiSelectHistory]] <- list()
        accumulated <- c(accumulated, .panelResaved)
    }

    .mark_panel_as_modified(panel_name, accumulated, rObjects)
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
