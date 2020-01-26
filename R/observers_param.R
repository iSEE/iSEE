#' Define parameter box observers
#'
#' Define a series of observers to track the opening and closing of parameter boxes for a given panel type.
#'
#' @param panel_name String containing the name of the panel.
#' @param box_types Character vector specifying all available box types for the current panel type.
#' @param input The Shiny input object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#'
#' @return
#' Observers are set up to record the opening and closing of boxes.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_box_observer
#' @importFrom shiny observeEvent
.create_box_observers <- function(panel_name, box_types, input, pObjects) {
    for (box in box_types) {
        local({
            box0 <- box
            open_field <- paste0(panel_name, "_", box0)
            # nocov start
            observeEvent(input[[open_field]], {
                pObjects$memory[[panel_name]][[box0]] <- input[[open_field]]
            })
            # nocov end
        })
    }

    invisible(NULL)
}

#' Define visual parameter choice observer
#'
#' Define an observer to track the checkboxes in the visual parameter box.
#'
#' @inheritParams .create_box_observers
#'
#' @return
#' An observer is set up to record the checking of visual parameter options.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_visual_parameter_choice_observer
#' @importFrom shiny observeEvent
.create_visual_parameter_choice_observer  <- function(panel_name, input, pObjects) {
    cur_field <- paste0(panel_name, "_", .visualParamChoice)
    # nocov start
    observeEvent(input[[cur_field]], {
        existing <- pObjects$memory[[panel_name]][[.visualParamChoice]]
        incoming <- as(input[[cur_field]], typeof(existing))
        if (identical(incoming, existing)) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[.visualParamChoice]] <- incoming
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
    # nocov end
    invisible(NULL)
}

#' Define parameter observers
#'
#' Define a series of observers to track \dQuote{protected} or \dQuote{unprotected} parameters for a given panel.
#' These will register input changes to each specified parameter in the app's memory
#' and request an update to the output of the affected panel.
#'
#' @inheritParams .create_box_observers
#' @param fields Character vector of names of parameters for which to set up observers.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param ignoreInit,ignoreNULL Further arguments to pass to \code{\link{observeEvent}}.
#'
#' @return
#' Observers are set up to monitor the UI elements that can change the protected and non-fundamental parameters.
#' A \code{NULL} is invisibly returned.
#'
#' @details
#' A protected parameter is one that breaks existing multiple selections, e.g., by changing the actual data being plotted.
#' Alterations to protected parameters will clear all active and saved selections in the panel,
#' as those existing selections are assumed to not make any sense in the context of the modified output of that panel.
#'
#' By comparison, an unprotected parameter only changes the aesthetics and will not clear existing selections.
#'
#' @seealso
#' \code{\link{.requestUpdate}} and \code{\link{.requestCleanUpdate}},
#' used to trigger updates to the panel output.
#'
#' @author Aaron Lun
#'
#' @export
#' @rdname createProtectedParameterObservers
#' @importFrom shiny observeEvent
.createUnprotectedParameterObservers <- function(panel_name, fields, input, pObjects, rObjects, ignoreInit=TRUE, ignoreNULL=TRUE) {
    for (field in fields) {
        local({
            field0 <- field
            cur_field <- paste0(panel_name, "_", field0)
            # nocov start
            observeEvent(input[[cur_field]], {
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[panel_name]][[field0]]))
                if (identical(matched_input, pObjects$memory[[panel_name]][[field0]])) {
                    return(NULL)
                }
                pObjects$memory[[panel_name]][[field0]] <- matched_input
                .requestUpdate(panel_name, rObjects)
            }, ignoreInit=ignoreInit, ignoreNULL=ignoreNULL)
            # nocov end
        })
    }
    invisible(NULL)
}

#' @export
#' @rdname createProtectedParameterObservers
#' @importFrom shiny observeEvent
.createProtectedParameterObservers <- function(panel_name, fields, input, pObjects, rObjects, ignoreInit=TRUE, ignoreNULL=TRUE) {
    for (field in fields) {
        local({
            field0 <- field
            cur_field <- paste0(panel_name, "_", field0)
            # nocov start
            observeEvent(input[[cur_field]], {
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[panel_name]][[field0]]))
                if (identical(matched_input, pObjects$memory[[panel_name]][[field0]])) {
                    return(NULL)
                }
                pObjects$memory[[panel_name]][[field0]] <- matched_input
                .requestCleanUpdate(panel_name, pObjects, rObjects)
            }, ignoreInit=ignoreInit, ignoreNULL=ignoreNULL)
            # nocov end
        })
    }
    invisible(NULL)
}
