.updateSavedChoices <- "INTERNAL_saved_choices"

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
#' @rdname INTERNAL_selection_parameter_observers
#' @importFrom shiny observeEvent isolate
#' showNotification updateSelectInput updateRadioButtons
#' @importFrom igraph is_dag simplify
.define_selection_choice_observer <- function(panel_name, 
    by_field, saved_field, input, session, pObjects, rObjects) 
{
    repop_name <- paste0(panel_name, "_", .panelRepopulated)
    .safe_reactive_init(rObjects, repop_name)
    saved_select_name <- paste0(panel_name, "_", .updateSavedChoices)
    .safe_reactive_init(rObjects, saved_select_name)

    select_panel_field <- paste0(panel_name, "_", by_field)
    observeEvent(input[[select_panel_field]], {
        old_transmitter <- pObjects$memory[[panel_name]][[by_field]]
        new_transmitter <- input[[select_panel_field]]
        if (old_transmitter==new_transmitter) {
            return(NULL)
        }

        tmp <- .choose_new_parent(pObjects$selection_links, panel_name, 
            new_parent_name=new_transmitter, old_parent_name=old_transmitter,
            field=by_field)

        # Trying to update the graph, but breaking if it's not a DAG.
        # We also break if users try to self-select in restrict mode.
        # These concerns are only relevant for transmitting panels (i.e., point plots).
        daggy <- is_dag(simplify(tmp, remove.loops=TRUE))
        self_restrict <- new_transmitter==panel_name && .restrictsSelection(pObjects$memory[[panel_name]])

        if (!daggy || self_restrict) {
            if (!daggy) {
                showNotification("point selection relationships cannot be cyclic", type="error")
            } else if (self_restrict){
                showNotification("selecting to self is not compatible with 'Restrict'", type="error")
            }
            updateSelectInput(session, select_panel_field, selected=old_transmitter)
            return(NULL)
        }

        pObjects$selection_links <- tmp
        pObjects$memory[[panel_name]][[by_field]] <- new_transmitter

        # Update the elements reporting the links between panels.
        for (relinked in setdiff(c(old_transmitter, new_transmitter, panel_name), .noSelection)) {
            relink_name <- paste0(relinked, "_", .panelLinkInfo)
            .safe_reactive_bump(rObjects, relink_name)
        }

        # Update the saved selection choice selectize.
        .safe_reactive_bump(rObjects, saved_select_name)

        saved_val <- pObjects$memory[[panel_name]][[saved_field]]
        if (saved_val!=0L && new_transmitter!=.noSelection) {
            if (saved_val > any_saved_selection(pObjects$memory[[new_transmitter]], count=TRUE)) {
                pObjects$memory[[panel_name]][[saved_field]] <- 0L
            }
        }

        # Checking if there were active/saved selections in either the new or
        # old transmitters. This requires some protection when this observer
        # is triggered because the old transmitter was deleted.
        if (old_transmitter %in% c(.noSelection, names(pObjects$memory))) {
            select_type <- pObjects$memory[[panel_name]][[type_field]]
            select_saved <- pObjects$memory[[panel_name]][[saved_field]]

            no_old_selection <- !.transmitted_selection(panel_name, old_transmitter, pObjects$memory,
                select_type=select_type, select_saved=select_saved)
            no_new_selection <- !.transmitted_selection(panel_name, new_transmitter, pObjects$memory,
                select_type=select_type, select_saved=select_saved)

            if (no_old_selection && no_new_selection) {
                return(NULL)
            }
        }

        .safe_reactive_bump(rObjects, panel_name)

        # Updating children, if the current panel is set to restrict
        # (and thus the point population changes with a new transmitted selection).
        if (.restrictsSelection(pObjects$memory[[panel_name]])) {
            .safe_reactive_bump(rObjects, repop_name)
        }
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#' @importFrom shiny showNotification updateRadioButtons observeEvent
.define_selection_effect_observer <- function(plot_name, 
    by_field, type_field, saved_field, 
    input, session, pObjects, rObjects) 
{
    repop_name <- paste0(panel_name, "_", .panelRepopulated)
    .safe_reactive_init(rObjects, repop_name)

    select_effect_field <- paste0(plot_name, "_", .selectEffect)
    observeEvent(input[[select_effect_field]], {
        cur_effect <- input[[select_effect_field]]
        old_effect <- pObjects$memory[[plot_name]][[.selectEffect]]

        # Storing the new choice into memory, unless self-selecting to restrict.
        # In which case, we trigger an error and reset to the previous choice.
        if (cur_effect == .selectRestrictTitle && pObjects$memory[[plot_name]][[by_field]]==plot_name) {
            showNotification("selecting to self is not compatible with 'Restrict'", type="error")
            updateRadioButtons(session, select_effect_field, selected=old_effect)
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.selectEffect]] <- cur_effect

        # Avoiding replotting if there was no transmitting selection.
        if (!.transmitted_selection(plot_name, 
            pObjects$memory[[plot_name]][[by_field]],
            all_memory=pObjects$memory,
            select_type=pObjects$memory[[plot_name]][[type_field]],
            select_saved=pObjects$memory[[plot_name]][[saved_field]])) 
        {
            return(NULL)
        }

        .safe_reactive_bump(rObjects, plot_name)

        # Updating children if the selection in the current plot changes due to gain/loss of Restrict.
        if (cur_effect==.selectRestrictTitle || old_effect==.selectRestrictTitle) {
            .safe_reactive_bump(rObjects, repop_name)
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
.define_saved_selection_choice_observers <- function(panel_name, 
    by_field, type_field, saved_field,
    input, session, pObjects, rObjects) 
{
    repop_name <- paste0(panel_name, "_", .panelRepopulated)
    .safe_reactive_init(rObjects, repop_name)

    ## Type field observers. ---
    type_field <- paste0(panel_name, "_", type_field)
    observeEvent(input[[type_field]], {
        old_type <- pObjects$memory[[panel_name]][[type_field]]
        new_type <- as(input[[type_field]], typeof(old_type))
        if (identical(new_type, old_type)) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[type_field]] <- new_type

        # Skipping if neither the old or new types were relevant.
        transmitter <- pObjects$memory[[panel_name]][[by_field]]
        select_saved <- pObjects$memory[[panel_name]][[saved_field]]

        no_old_selection <- !.transmitted_selection(panel_name, transmitter, pObjects$memory, 
            select_type=old_type, select_saved=select_saved)
        no_new_selection <- !.transmitted_selection(panel_name, transmitter, pObjects$memory, 
            select_type=new_type, select_saved=select_saved)

        if (no_old_selection && no_new_selection) {
            return(NULL)
        }

        .safe_reactive_bump(rObjects, panel_name)
        if (.restrictsSelection(pObjects$memory[[panel_name]])) {
            .safe_reactive_bump(rObjects, repop_name)
        }
    }, ignoreInit=TRUE)

    ## Saved field observers. ---
    saved_select_field <- paste0(panel_name, "_", saved_field)
    observeEvent(input[[saved_select_field]], {
        # Required to defend against empty strings before updateSelectizeInput runs.
        req(input[[saved_select_field]]) 

        matched_input <- as(input[[saved_select_field]], 
            typeof(pObjects$memory[[panel_name]][[saved_field]]))
        if (identical(matched_input, pObjects$memory[[panel_name]][[saved_field]])) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[saved_field]] <- matched_input

        transmitter <- pObjects$memory[[panel_name]][[by_field]]
        if (transmitter==.noSelection) {
            return(NULL)
        }

        # Switch of 'Saved' will ALWAYS change the current plot, so no need for other checks.
        .safe_reactive_bump(rObjects, panel_name)
        if (.restrictsSelection(pObjects$memory[[panel_name]])) {
            .safe_reactive_bump(rObjects, repop_name)
        }
    }, ignoreInit=TRUE)

    ## Selectize observer. ---
    # Do NOT be tempted to centralize code by setting 'saved_field' in the above observer.
    # This needs to be done in a separate observer that actually executes to set the 
    # the field to something upon initialization of the panel.
    saved_choice_name <- paste0(panel_name, "_", .updateSavedChoices)
    .safe_reactive_init(rObjects, saved_choice_name)

    observe({
        force(rObjects[[saved_choice_name]])
        force(rObjects$rerendered)

        # Protect against re-rendering after deleting a panel.
        if (!panel_name %in% names(pObjects$memory)) {
            return(NULL)
        }

        transmitter <- pObjects$memory[[panel_name]][[by_field]]
        if (transmitter==.noSelection) {
            available_choices <- integer(0)
        } else {
            N <- length(pObjects$memory[[transmitter]][[.multiSelectHistory]])
            available_choices <- seq_len(N)
            names(available_choices) <- available_choices
        }

        no_choice <- 0L
        names(no_choice) <- .noSelection
        available_choices <- c(no_choice, available_choices)
        updateSelectizeInput(session, saved_select_field, choices=available_choices, server=TRUE,
            selected=pObjects$memory[[panel_name]][[saved_field]])
    })

    invisible(NULL)
}
