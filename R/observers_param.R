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
            observeEvent(input[[open_field]], {
                pObjects$memory[[panel_name]][[box0]] <- input[[open_field]]
            })
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

    observeEvent(input[[cur_field]], {
        existing <- pObjects$memory[[panel_name]][[.visualParamChoice]]
        incoming <- as(input[[cur_field]], typeof(existing))
        if (identical(incoming, existing)) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[.visualParamChoice]] <- incoming
    }, ignoreInit=TRUE, ignoreNULL=FALSE)

    invisible(NULL)
}

#' Define plot parameter observers
#' 
#' Define a series of observers to track \dQuote{protected} or \dQuote{non-fundamental} parameters for a given panel.
#' 
#' @inheritParams .create_box_observers
#' @param fields Character vector of names of parameters for which to set up observers.
#' @param session The Shiny session object from the server function.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' @param ignoreInit,ignoreNULL Further arguments to pass to \code{\link{observeEvent}}.
#' 
#' @return
#' Observers are set up to monitor the UI elements that can change the protected and non-fundamental parameters.
#' A \code{NULL} is invisibly returned.
#' 
#' @details
#' A protected parameter is one that breaks existing multiple selections, usually by changing the actual data being plotted.
#' Alterations to protected parameters should clear all active and saved selections in the panel.
#' By comparison, a non-fundamental parameter only changes the aesthetics.
#' 
#' @author Aaron Lun
#'
#' @rdname INTERNAL_plot_parameter_observers
#' @importFrom shiny observeEvent
.create_nonfundamental_parameter_observers <- function(panel_name, fields, se, input, session, pObjects, rObjects,
    ignoreInit=TRUE, ignoreNULL=TRUE) 
{
    for (field in fields) {
        local({
            field0 <- field
            cur_field <- paste0(panel_name, "_", field0)

            observeEvent(input[[cur_field]], {
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[panel_name]][[field0]]))
                if (identical(matched_input, pObjects$memory[[panel_name]][[field0]])) {
                    return(NULL)
                }
                pObjects$memory[[panel_name]][[field0]] <- matched_input
                .refreshPanelOutput(panel_name, se, pObjects, rObjects)
            }, ignoreInit=ignoreInit, ignoreNULL=ignoreNULL)
        })
    }

    invisible(NULL)
}

#' @rdname INTERNAL_plot_parameter_observers
#' @importFrom shiny observeEvent
.create_protected_parameter_observers <- function(panel_name, fields, se, input, session, pObjects, rObjects,
    ignoreInit=TRUE, ignoreNULL=TRUE) 
{
    for (field in fields) {
        local({
            field0 <- field
            cur_field <- paste0(panel_name, "_", field0)

            observeEvent(input[[cur_field]], {
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[panel_name]][[field0]]))
                if (identical(matched_input, pObjects$memory[[panel_name]][[field0]])) {
                    return(NULL)
                }
                pObjects$memory[[panel_name]][[field0]] <- matched_input
                .refreshPanelOutputUnselected(panel_name, se, pObjects, rObjects)
            }, ignoreInit=ignoreInit, ignoreNULL=ignoreNULL)
        })
    }

    invisible(NULL)
}
