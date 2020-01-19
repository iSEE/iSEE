.updateSavedChoices <- "INTERNAL_saved_choices"

#' Selection parameter observers
#'
#' A function to set up observers for the choice and visual effect of transmitting panels for multiple selections.
#' Seperate functions are used for the choice and effect observers as the latter is only relevant to plots.
#'
#' @param panel_name,plot_name String containing the name of the panel.
#' @param by_field String specifying the name of the slot containing the identity of the panel transmitting to the current panel.
#' @param type_field String specifying the name of the slot containing the type of multiple selection to use from the transmitter.
#' @param saved_field String specifying the name of the slot containing the index of the saved selection to use from the transmitter.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which these functions are called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{.renderOutput,Panel-method}}, where \code{.create_multi_selection_choice_observer} is called.
#'
#' \code{\link{.renderOutput,DotPlot-method}}, where \code{.create_multi_selection_effect_observer} is called.
#'
#' @rdname INTERNAL_selection_parameter_observers
#' @importFrom shiny observeEvent showNotification updateSelectInput 
#' @importFrom igraph is_dag simplify
.create_multi_selection_choice_observer <- function(panel_name,  
    by_field, type_field, saved_field, input, session, pObjects, rObjects) 
{
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
        if (!is_dag(simplify(tmp, remove.loops=TRUE))) {
            showNotification("point selection relationships cannot be cyclic", type="error")
            updateSelectInput(session, select_panel_field, selected=old_transmitter)
            return(NULL)
        }

        pObjects$selection_links <- tmp
        pObjects$memory[[panel_name]][[by_field]] <- new_transmitter

        # Update the elements reporting the links between panels.
        for (relinked in setdiff(c(old_transmitter, new_transmitter, panel_name), .noSelection)) {
            relink_name <- paste0(relinked, "_", .flagRelinkedSelect)
            .safe_reactive_bump(rObjects, relink_name)
        }

        # Update the saved selection choice selectize.
        .safe_reactive_bump(rObjects, saved_select_name)

        saved_val <- pObjects$memory[[panel_name]][[saved_field]]
        if (saved_val!=0L && new_transmitter!=.noSelection) {
            if (saved_val > .any_saved_selection(pObjects$memory[[new_transmitter]], count=TRUE)) {
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

        # Updating children, if the current panel is set to restrict
        # (and thus the point population changes with a new transmitted selection).
        if (.multiSelectionRestricted(pObjects$memory[[panel_name]])) {
            .mark_panel_as_modified(panel_name, .panelRepopulated, rObjects)
        } else {
            .requestUpdate(panel_name, rObjects)
        }
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#' @rdname INTERNAL_selection_parameter_observers
#' @importFrom shiny showNotification observeEvent
.create_multi_selection_effect_observer <- function(plot_name, 
    by_field, type_field, saved_field, 
    input, session, pObjects, rObjects) 
{
    select_effect_field <- paste0(plot_name, "_", .selectEffect)
    observeEvent(input[[select_effect_field]], {
        cur_effect <- input[[select_effect_field]]
        old_effect <- pObjects$memory[[plot_name]][[.selectEffect]]
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

        # Updating children if the selection in the current plot changes due to gain/loss of Restrict.
        if (cur_effect==.selectRestrictTitle || old_effect==.selectRestrictTitle) {
            .mark_panel_as_modified(plot_name, .panelRepopulated, rObjects)
        } else {
            .requestUpdate(plot_name, rObjects)
        }
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#' @importFrom shiny observeEvent observe updateSelectInput req
#' @rdname INTERNAL_selection_parameter_observers
.create_multi_selection_type_observers <- function(panel_name, 
    by_field, type_field, saved_field,
    input, session, pObjects, rObjects) 
{
    ## Type field observers. ---
    select_type_field <- paste0(panel_name, "_", type_field)
    observeEvent(input[[select_type_field]], {
        old_type <- pObjects$memory[[panel_name]][[type_field]]
        new_type <- as(input[[select_type_field]], typeof(old_type))
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

        if (.multiSelectionRestricted(pObjects$memory[[panel_name]])) {
            .mark_panel_as_modified(panel_name, .panelRepopulated, rObjects)
        } else {
            .requestUpdate(panel_name, rObjects)
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

        # Switch of 'Saved' will ALWAYS change the current plot, as it's not
        # possible to do so without being on the "Saved" choice in the first
        # place; so there's no need for other checks.

        if (.multiSelectionRestricted(pObjects$memory[[panel_name]])) {
            .mark_panel_as_modified(panel_name, .panelRepopulated, rObjects)
        } else {
            .requestUpdate(panel_name, rObjects)
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

#' Multiple selection observers
#'
#' Observers to change the multiple selections by saving the active selection or deleting existing saved selections.
#' This differs from \code{\link{.create_multi_selection_type_observers}}, which just involves using existing saved selections.
#'
#' @param panel_name String containing the name of the plot..
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
#' @importFrom shiny observeEvent 
#' @rdname INTERNAL_multiple_select_observers
.create_multi_selection_history_observers <- function(panel_name, input, session, pObjects, rObjects) {
    save_field <- paste0(panel_name, "_", .multiSelectSave)
    del_field <- paste0(panel_name, "_", .multiSelectDelete)
    multi_name <- paste0(panel_name, "_", .flagMultiSelect)

    ## Save selection observer. ---
    observeEvent(input[[save_field]], {
        instance <- pObjects$memory[[panel_name]]
        current <- instance[[.multiSelectHistory]]
        to_store <- .multiSelectionActive(instance)
        if (is.null(to_store)) {
            return(NULL)
        }

        pObjects$memory[[panel_name]][[.multiSelectHistory]] <- c(current, list(to_store))

        .safe_reactive_bump(rObjects, multi_name)

        # Updating self (replot to get number), and updating children's selectize's.
        .mark_panel_as_modified(panel_name, .panelResaved, rObjects)

        .disableButtonIf(
            del_field,
            FALSE,
            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
        )
    })

    ## Deleted selection observer. ---
    observeEvent(input[[del_field]], {
        instance <- pObjects$memory[[panel_name]]
        current <- instance[[.multiSelectHistory]]
        current <- head(current, -1)
        pObjects$memory[[panel_name]][[.multiSelectHistory]] <- current

        .safe_reactive_bump(rObjects, multi_name)

        # Updating self and children's selectize's.
        .mark_panel_as_modified(panel_name, .panelResaved, rObjects)

        .disableButtonIf(
            del_field,
            length(current)==0,
            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
        )
    })

    invisible(NULL)
}
