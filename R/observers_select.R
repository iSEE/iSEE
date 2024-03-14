.updateSavedChoices <- "INTERNAL_saved_choices"

#' Selection parameter observers
#'
#' A function to set up observers for the choice and visual effect of transmitting panels for multiple selections.
#' Separate functions are used for the choice and effect observers as the latter is only relevant to plots.
#'
#' @param panel_name String containing the name of the panel.
#' @param by_field String specifying the name of the slot containing the identity of the panel transmitting to the current panel.
#' @param res_field String specifying the name of the slot indicating whether to restrict to the multiple selection.
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
#' \code{\link{.createObservers,Panel-method}}, where these functions are called.
#'
#' @rdname INTERNAL_selection_parameter_observers
#' @importFrom shiny observeEvent showNotification updateSelectInput
#' @importFrom igraph is_dag simplify
.create_multi_selection_choice_observer <- function(panel_name,
    by_field, input, session, pObjects, rObjects)
{
    saved_select_name <- paste0(panel_name, "_", .updateSavedChoices)
    .safe_reactive_init(rObjects, saved_select_name)

    select_panel_field <- paste0(panel_name, "_", by_field)

    # nocov start
    observeEvent(input[[select_panel_field]], {
        old_transmitter <- slot(pObjects$memory[[panel_name]], by_field)
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
        slot(pObjects$memory[[panel_name]], by_field) <- new_transmitter

        # Update the elements reporting the links between panels.
        for (relinked in setdiff(c(old_transmitter, new_transmitter, panel_name), .noSelection)) {
            relink_name <- paste0(relinked, "_", .flagRelinkedSelect)
            .safe_reactive_bump(rObjects, relink_name)
        }

        # Checking if there were active/saved selections in either the new or
        # old transmitters. This requires some protection when this observer
        # is triggered because the old transmitter was deleted.
        if (old_transmitter %in% c(.noSelection, names(pObjects$memory))) {
            no_old_selection <- !.transmitted_selection(old_transmitter, pObjects$memory)
            no_new_selection <- !.transmitted_selection(new_transmitter, pObjects$memory)
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
    # nocov end

    invisible(NULL)
}

#' @rdname INTERNAL_selection_parameter_observers
#' @importFrom shiny observeEvent
.create_multi_selection_restrict_observer <- function(panel_name,
    by_field, res_field, input, session, pObjects, rObjects)
{
    select_restrict_field <- paste0(panel_name, "_", res_field)

    # nocov start
    observeEvent(input[[select_restrict_field]], {
        cur_restrict <- input[[select_restrict_field]]
        old_restrict <- slot(pObjects$memory[[panel_name]], res_field)
        if (cur_restrict==old_restrict) {
            return(NULL)
        }

        slot(pObjects$memory[[panel_name]], res_field) <- cur_restrict

        # Avoiding replotting if there was no transmitting selection from the upstream panel.
        if (!.transmitted_selection(slot(pObjects$memory[[panel_name]], by_field), all_memory=pObjects$memory)) {
            return(NULL)
        }

        # Updating self as the selection in the current panel changes due to gain/loss of Restrict.
        # Potentially also updating children, as any selection transmitted to those children will change.
        .mark_panel_as_modified(panel_name, .panelRepopulated, rObjects)
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}

#' Multiple selection observers
#'
#' Observers to change the multiple selections by saving the active selection or deleting existing saved selections.
#'
#' @param panel_name String containing the name of the plot.
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

    # nocov start
    observeEvent(input[[save_field]], {
        instance <- pObjects$memory[[panel_name]]
        current <- slot(instance, .multiSelectHistory)
        to_store <- .multiSelectionActive(instance)
        if (is.null(to_store)) {
            return(NULL)
        }

        slot(pObjects$memory[[panel_name]], .multiSelectHistory) <- c(current, list(to_store))

        .safe_reactive_bump(rObjects, multi_name)

        # Updating self (replot to get number), and updating children's selectize's.
        .mark_panel_as_modified(panel_name, .panelResaved, rObjects)

        .disableButtonIf(
            del_field,
            FALSE,
            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
        )
    }, ignoreInit=TRUE)
    # nocov end

    ## Deleted selection observer. ---

    # nocov start
    observeEvent(input[[del_field]], {
        current <- slot(pObjects$memory[[panel_name]], .multiSelectHistory)
        current <- head(current, -1)
        slot(pObjects$memory[[panel_name]], .multiSelectHistory) <- current

        .safe_reactive_bump(rObjects, multi_name)

        # Updating self and children's selectize's.
        .mark_panel_as_modified(panel_name, .panelResaved, rObjects)

        .disableButtonIf(
            del_field,
            length(current)==0,
            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
        )
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}

#' Dynamic multiple selection source observer
#'
#' Create an observer for (un)checking of the dynamic multiple selection source option.
#'
#' @param panel_name String containing the name of the plot.
#' @param dyn_field String containing the name of the slot determining whether a dynamic source is to be used.
#' @param by_field String containing the name of the slot controlling the multiple selection source.
#' @param source_type String specifying whether the observer is to monitor multiple \code{"row"} or \code{"column"} selections.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return An observer is created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_create_dynamic_multi_selection_source_observer
.create_dynamic_multi_selection_source_observer <- function(panel_name, 
    dyn_field, by_field, source_type, input, session, pObjects, rObjects) 
{
    .create_dynamic_selection_source_observer(panel_name,
        dyn_field=dyn_field, by_field=by_field, source_type=source_type,
        object_name="dynamic_multi_selections",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
}

#' @importFrom shiny observeEvent
.create_dynamic_selection_source_observer <- function(panel_name, 
    dyn_field, by_field, source_type, object_name,
    input, session, pObjects, rObjects) 
{
    select_dyn_field <- paste0(panel_name, "_", dyn_field)
    force(by_field)
    force(object_name)
    force(source_type)

    # nocov start
    observeEvent(input[[select_dyn_field]], {
        current <- slot(pObjects$memory[[panel_name]], dyn_field)
        matched_input <- as(input[[select_dyn_field]], typeof(current))
        if (identical(matched_input, current)) {
            return(NULL)
        }
        slot(pObjects$memory[[panel_name]], dyn_field) <- matched_input

        if (matched_input) {
            FUN <- .add_panel_to_dynamic_sources 
        } else {
            FUN <- .delete_panel_from_dynamic_sources
        }

        pObjects[[object_name]] <- FUN(pObjects[[object_name]], 
            panel_name=panel_name, source_type=source_type, field=by_field)
    }, ignoreInit=TRUE)
    # nocov end
    
    invisible(NULL)
}
