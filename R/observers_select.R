#' Selection parameter observers
#'
#' A function to set up observers for selection parameter observers used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent isolate
#' showNotification updateSelectInput updateRadioButtons
#' @importFrom igraph is_dag simplify
#' @rdname INTERNAL_selection_parameter_observers
.define_selection_choice_observer <- function(panel_name, input, session, pObjects, rObjects) {
    select_panel_field <- paste0(panel_name, "_", .selectByPlot)
    repop_field <- paste0(panel_name, "_repopulated")
    can_transmit <- .can_transmit(pObjects$memory[[panel_anem]])
    .safe_reactive_init(rObjects, paste0(mode, id, "_", .selectMultiSaved))

    observeEvent(input[[select_panel_field]], {
        old_transmitter <- pObjects$memory[[panel_name]][[.selectByPlot]]
        new_transmitter <- input[[select_panel_field]]
        if (old_transmitter==new_transmitter) {
            return(NULL)
        }

        tmp <- .choose_new_selection_source(pObjects$selection_links, panel_name, new_transmitter, old_transmitter)

        # Trying to update the graph, but breaking if it's not a DAG.
        # We also break if users try to self-select in restrict mode.
        # These concerns are only relevant for transmitting panels (i.e., point plots).
        if (can_transmit) {
            daggy <- is_dag(simplify(tmp, remove.loops=TRUE))
            self_restrict <- new_transmitter==panel_name &&
                new_transmitter!=.noSelection &&
                pObjects$memory[[panel_name]][[.selectEffect]]==.selectRestrictTitle

            if (!daggy || self_restrict) {
                if (!daggy) {
                    showNotification("point selection relationships cannot be cyclic", type="error")
                } else if (self_restrict){
                    showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                }
                updateSelectInput(session, select_panel_field, selected=old_transmitter)
                return(NULL)
            }
        }

        pObjects$selection_links <- tmp
        pObjects$memory[[panel_name]][[.selectByPlot]] <- new_transmitter

        # Update the elements reporting the links between panels.
        for (relinked in setdiff(c(old_transmitter, new_transmitter, panel_name), .noSelection)) {
            relink_field <- paste0(relinked, "_", .panelLinkInfo)
            .safe_reactive_bump(rObjects, relink_field)
        }

        # Update the multi-selection selectize.
        saved_select_name <- paste0(panel_name, "_", .selectMultiSaved)
        .safe_reactive_bump(rObjects, saved_select_name)

        saved_val <- pObjects$memory[[panel_name]][[.selectMultiSaved]]
        if (saved_val!=0L && new_transmitter!=.noSelection) {
            if (saved_val > length(pObjects$memory[[new_transmitter]][[.multiSelectHistory]])) {
                pObjects$memory[[panel_name]][[.selectMultiSaved]] <- 0L
            }
        }

        # Checking if there were active/saved selections in either the new or old transmitters.
        no_old_selection <- !.transmitted_selection(old_transmitter, pObjects$memory, panel_name)
        no_new_selection <- !.transmitted_selection(new_transmitter, pObjects$memory, panel_name)
        if (no_old_selection && no_new_selection) {
            return(NULL)
        }

        .safe_reactive_bump(rObjects, panel_name)

        # Updating children, if the current panel is set to restrict
        # (and thus the point population changes with a new transmitted selection).
        if (can_transmit && pObjects$memory[[panel_name]][[.selectEffect]]==.selectRestrictTitle) {
            .safe_reactive_bump(rObjects, repop_field)
        }
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#' @importFrom shiny showNotification updateRadioButtons observeEvent
.define_selection_effect_observer <- function(plot_name, input, session, pObjects, rObjects) {
    select_effect_field <- paste0(plot_name, "_", .selectEffect)
    repop_field <- paste0(plot_name, "_repopulated")
    can_transmit <- .can_transmit(pObjects$memory[[plot_name]])

    observeEvent(input[[select_effect_field]], {
        cur_effect <- input[[select_effect_field]]
        old_effect <- pObjects$memory[[plot_name]][[.selectEffect]]

        # Storing the new choice into memory, unless self-selecting to restrict.
        # In which case, we trigger an error and reset to the previous choice.
        if (can_transmit) {
            if (cur_effect == .selectRestrictTitle && pObjects$memory[[plot_name]][[.selectByPlot]]==plot_name) {
                showNotification("selecting to self is not compatible with 'Restrict'", type="error")
                updateRadioButtons(session, select_effect_field, selected=old_effect)
                return(NULL)
            }
        }
        pObjects$memory[[plot_name]][[.selectEffect]] <- cur_effect

        # Avoiding replotting if there was no transmitting selection.
        transmitter <- pObjects$memory[[plot_name]][[.selectByPlot]]
        if (!.transmitted_selection(transmitter, pObjects$memory, plot_name)) {
            return(NULL)
        }

        .safe_reactive_bump(rObjects, plot_name)

        # Updating children if the selection in the current plot changes due to gain/loss of Restrict.
        if (cur_effect==.selectRestrictTitle || old_effect==.selectRestrictTitle) {
            .safe_reactive_bump(rObjects, repop_field)
        }
    }, ignoreInit=TRUE)

    invisible(NULL)
}


#' Multi-select parameter observers
#'
#' Observers for the multiple (i.e., saved) selection parameter choices.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny observeEvent observe updateSelectInput isolate
#' @rdname INTERNAL_multiselect_param_observers
.define_saved_selection_choice_observers <- function(panel_name, input, session, pObjects, rObjects) {
    repop_field <- paste0(panel_name, "_repopulated")

    ## Type field observers. ---
    type_field <- paste0(panel_name, "_", .selectMultiType)
    observeEvent(input[[type_field]], {
        old_type <- pObjects$memory[[panel_name]][[.selectMultiType]]
        new_type <- as(input[[type_field]], typeof(old_type))
        if (identical(new_type, old_type)) {
            return(NULL)
        }
        pObjects$memory[[mode0]][[.selectMultiType]] <- new_type

        # Skipping if neither the old or new types were relevant.
        transmitter <- pObjects$memory[[panel_name]][[.selectByPlot]]
        no_old_selection <- !.transmitted_selection(transmitter, pObjects$memory, select_type=old_type, panel_name)
        no_new_selection <- !.transmitted_selection(transmitter, pObjects$memory, panel_name)
        if (no_old_selection && no_new_selection) {
            return(NULL)
        }

        .safe_reactive_bump(rObjects, panel_name)
        if (pObjects$memory[[panel_name]][[.selectEffect]]==.selectRestrictTitle) {
            .safe_reactive_bump(rObjects, repop_field)
        }
    }, ignoreInit=TRUE)

    ## Saved field observers. ---
    saved_select <- paste0(panel_name, "_", .selectMultiSaved)
    observeEvent(input[[saved_select]], {
        # Required to defend against empty strings before updateSelectizeInput runs.
        req(input[[saved_select]]) 

        matched_input <- as(input[[saved_select]], typeof(pObjects$memory[[panel_name]][[.selectMultiSaved]]))
        if (identical(matched_input, pObjects$memory[[panel_name]][[.selectMultiSaved]])) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[.selectMultiSaved]] <- matched_input

        transmitter <- pObjects$memory[[panel_name]][[.selectByPlot]]
        if (transmitter==.noSelection) {
            return(NULL)
        }

        # Switch of 'Saved' will ALWAYS change the current plot, so no need for other checks.
        .safe_reactive_bump(rObjects, panel_name)
        if (pObjects$memory[[panel_name]][[.selectEffect]]==.selectRestrictTitle) {
            .safe_reactive_bump(rObjects, repop_field)
        }
    }, ignoreInit=TRUE)

    ## Selectize observer. ---
    # Do NOT be tempted to centralize code by setting .selectMultiSaved in the above observer.
    # This needs to be done in a separate observer that actually executes to set the 
    # the field to something upon initialization of the panel.
    observe({
        .safe_reactive_init(rObjects, saved_select)
        force(rObjects[[saved_select]])
        force(rObjects$rerendered)

        transmitter <- pObjects$memory[[panel_name]][[.selectByPlot]]
        if (transmitter==.noSelection) {
            available_choices <- integer(0)
        } else {
            trans <- .encode_panel_name(transmitter)
            N <- length(pObjects$memory[[trans$Type]][,.multiSelectHistory][[trans$ID]])
            available_choices <- seq_len(N)
            names(available_choices) <- available_choices
        }

        no_choice <- 0L
        names(no_choice) <- .noSelection
        available_choices <- c(no_choice, available_choices)
        updateSelectizeInput(session, saved_select, choices=available_choices, server=TRUE,
            selected=pObjects$memory[[panel_name]][[.selectMultiSaved]])
    })

    invisible(NULL)
}
