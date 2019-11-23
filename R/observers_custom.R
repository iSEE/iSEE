#' Define parameter box observers
#'
#' Define a series of observers to track the opening and closing of parameter boxes for a given panel type.
#' 
#' @param mode String specifying the encoded panel type (e.g., \code{"redDimPlot"}).
#' @param id Integer specifying the index of the current panel.
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
#' @rdname INTERNAL_define_box_observer
#' @importFrom shiny observeEvent
.define_box_observers <- function(mode, id, box_types, input, pObjects) {
    for (box in box_types) {
        local({
            box0 <- box
            open_field <- paste0(mode, id, "_", box0)
            observeEvent(input[[open_field]], {
                pObjects$memory[[mode]][[box0]][id] <- input[[open_field]]
            })
        })
    }
    invisible(NULL)
}

#' Define visual parameter choice observer
#'
#' Define an observer to track the checkboxes in the visual parameter box.
#'
#' @inheritParams .define_box_observers
#'
#' @return
#' An observer is set up to record the checking of visual parameter options.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_define_visual_param_choice_observer
#' @importFrom shiny observeEvent
.define_visual_param_choice_observer  <- function(mode, id, pObjects) {
    plot_name <- paste0(mode, id)
    cur_field <- paste0(plot_name, "_", .visualParamChoice)

    observeEvent(input[[cur_field]], {
        existing <- pObjects$memory[[mode]][,.visualParamChoice][[id]]
        incoming <- as(input[[cur_field]], typeof(existing))
        if (identical(incoming, existing)) {
            return(NULL)
        }
        pObjects$memory[[mode]] <- .update_list_element(pObjects$memory[[mode]], id, .visualParamChoice, incoming)
    }, ignoreInit=TRUE, ignoreNULL=FALSE)

    invisible(NULL)
}

#' Define plot parameter observers
#' 
#' Define a series of observers to track \dQuote{protected} and \dQuote{non-fundamental} parameters for a given panel.
#' 
#' @inheritParams .define_box_observers
#' @param protected Character vector containing the identifies of the UI elements corresponding to protected parameters.
#' @param nonfundamental Character vector containing the identifies of the UI elements corresponding to non-fundamental parameters.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#' 
#' @return
#' Observers are set up to monitor the UI elements that can change the protected and non-fundamental parameters.
#' A \code{NULL} is invisibly returned.
#' 
#' @details
#' A protected parameter is one that breaks brushes and lassos, usually by changing the actual data being plotted.
#' By comparison, a non-fundamental parameter only changes the aesthetics.
#' 
#' @author Aaron Lun
#'
#' @rdname INTERNAL_define_plot_parameter_observers
#' @importFrom shiny observeEvent
.define_plot_parameter_observers <- function(mode, id, protected, nonfundamental,
    input, output, session, pObjects, rObjects) 
{
    # Observers for the non-fundamental parameter options.
    for (field in nonfundamental) {
        local({
            field0 <- field
            plot_name <- paste0(mode, id)
            cur_field <- paste0(plot_name, "_", field0)

            observeEvent(input[[cur_field]], {
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode]][[field0]]))
                if (identical(matched_input, pObjects$memory[[mode]][[field0]][id])) {
                    return(NULL)
                }
                pObjects$memory[[mode]][[field0]][id] <- matched_input
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            }, ignoreInit=TRUE)
        })
    }

    # Observers for the fundamental plot parameters.
    for (field in protected) {
        local({
            field0 <- field
            plot_name <- paste0(mode, id)
            cur_field <- paste0(plot_name, "_", field0)

            observeEvent(input[[cur_field]], {
                matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode]][[field0]]))
                if (identical(matched_input, pObjects$memory[[mode]][[field0]][id])) {
                    return(NULL)
                }
                pObjects$memory[[mode]][[field0]][id] <- matched_input
                .regenerate_unselected_plot(mode, id, pObjects, rObjects)
             }, ignoreInit=TRUE)
        })
    }

    invisible(NULL)
}

#' Define dimension name observer
#'
#' Define an observer to track changes to fields involving the dimension name.
#' 
#' @inheritParams .define_plot_parameter_observers
#' @param name_field String specifying the name of the parameter that uses the dimension name as input.
#' @param choices Character vector containing the all possible (unique) choices for the dimension name.
#' @param in_use_field String specifying the parameter field of \code{pObjects$memory} that indicates whether the panel is currently depending on the dimension name for its plot.
#' @param in_use_value String specifying the value of the parameter field that indicates whether the panel is currently depending on the dimension name for its plot.
#' @param is_protected Logical scalar indicating if the dimension name is a protected parameter (see \code{\link{.define_plot_parameter_observers}}.
#' @param table_field String specifying the parameter field of \code{pObjects$memory} that specifies the transmitting table panel for a selection of the dimension name.
#' @param link_type String specifying the link type for the current parameter to the transmitting table in \code{table_field}, same as \code{param=} in \code{\link{.setup_table_observer}}.
#'
#' @return
#' An observer is set up to track changes to the dimension name, possibly triggering a regeneration of the plot.
#' Another observer is set up to detect changes in the transmitting panels.
#' 
#' @details
#' This is handled separately from the other observers because:
#' \itemize{
#' \item The \code{\link{selectizeInput}} element used for the dimension names are typically updated server-side,
#' requiring some care to defend against empty inputs before the \code{\link{updateSelectizeInput}} runs.
#' \item The dimension name can change (due to the linked table) without directly affecting the plot,
#' if the dimension name is not currently in use.
#' In such cases, we want to avoid needless re-rendering.
#' }
#' 
#' @author Aaron Lun
#'
#' @rdname INTERNAL_define_dim_name_observer
#' @importFrom shiny observeEvent
.define_dim_name_observer <- function(mode, id, name_field, choices, 
    in_use_field, in_use_value, is_protected, 
    table_field, link_type,
    pObjects, rObjects, session) 
{
    choice_names <- choices
    choices <- seq_along(choices)
    names(choices) <- choice_names

    plot_name <- paste0(mode, id)
    name_input <- paste0(plot_name, "_", name_field)
    
    observeEvent(input[[name_input]], {
        # Required to defend against empty strings before updateSelectizeInput runs upon re-render.
        req(input[[name_input]])

        matched_input <- as(input[[name_input]], typeof(pObjects$memory[[mode]][[name_field]]))
        if (identical(matched_input, pObjects$memory[[mode]][[name_field]][id])) {
            return(NULL)
        }
        pObjects$memory[[mode]][[name_field]][id] <- matched_input

        # Only regenerating if the current parameter is actually in use.
        if (pObjects$memory[[mode]][id,in_use_field]==in_use_value) {
            if (!is_protected) {
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            } else {
                .regenerate_unselected_plot(mode, id, pObjects, rObjects)
            }
        }
    }, ignoreInit=TRUE)

    # Observers for the linked color by feature name. This also updates the table_links information.
    observe({
        replot <- .setup_table_observer(mode, id, pObjects, rObjects, input, session,
            by_field=in_use_field, title=in_use_value,
            select_field=name_field, tab_field=table_field,
            select_choices=choices, param=link_type)

        if (replot) {
            if (!is_protected) {
                rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
            } else {
                .regenerate_unselected_plot(mode, id, pObjects, rObjects)
            }
        }
    })

    invisible(NULL)
}
