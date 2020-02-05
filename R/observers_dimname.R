.propagateDimnames <- "INTERNAL_dimnames"

#' Set up a dimname propagation observer
#'
#' Set up an observer to re-transmit a single selection to the dimension names of all child plots.
#' This differs from \code{\link{.setup_dimname_source_observer}} in that the current panel is the transmitter, not the receiver.
#'
#' @param panel_name String containing the name of the current transmitting panel.
#' @param choices Character vector containing all of possible choices for the transmitted dimension names.
#' Usually set to the row or column names of the provided \linkS4class{SummarizedExperiment} object.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @details
#' The specification of \code{choices} is necessary because we run \code{\link{updateSelectizeInput}} to update the choice of dimension name in the child panels;
#' however, for a server-side selectize, it seems that the possible choices are forgotten unless they are explicitly provided during the update.
#'
#' We do not need to explicitly trigger re-rendering of child panels, as this should be handled by each child's observer on the affected field.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{.createObservers,RowTable-method}} for an example of a transmitter that needs to call this function.
#'
#' @rdname INTERNAL_dimname_prop
#' @importFrom shiny eventReactive updateSelectizeInput
.create_dimname_propagation_observer <-  function(panel_name, choices, session, pObjects, rObjects) {
    dimname_name <- paste0(panel_name, "_", .propagateDimnames)
    .safe_reactive_init(rObjects, dimname_name)
    single_name <- paste0(panel_name, "_", .flagSingleSelect)
    # nocov start
    observeEvent(rObjects[[dimname_name]], {
        instance <- pObjects$memory[[panel_name]]

        chosen <- .singleSelectionValue(instance, pObjects)
        if (is.null(chosen)) {
            return(NULL)
        }
        .safe_reactive_bump(rObjects, single_name)

        dependents <- .get_direct_children(pObjects$aesthetics_links, panel_name)
        for (kid in names(dependents)) {
            all_fields <- dependents[[kid]]

            # There is a possibility that this would cause multi-rendering as they
            # trigger different observers. Oh well.
            for (field in all_fields) {
                updateSelectizeInput(session, paste0(kid, "_", field), server=TRUE,
                    choices=choices, selected=chosen)
            }
        }
    })
    # nocov end
    invisible(NULL)
}

#' Set up a dimname choice observer
#'
#' Set up the actions of an observer for a parameter choice in a panel that may receive a single selection from another panel.
#'
#' @inheritParams .create_dimname_observers
#'
#' @return
#' \code{pObjects} and \code{rObjects} are modified and selectize elements are possibly updated.
#' A logical scalar is returned indicating whether the current panel should be re-rendered
#' based on whether the choice of usage mode has changed.
#' (Changes in the transmitted single selection are handled implicitly via the selectize update.)
#'
#' @details
#' This function has a number of side-effects, relying on the pass-by-reference behaviour of \code{pObjects}, \code{rObjects} and \code{session} to perform its role.
#' \itemize{
#' \item New values of the fields \code{use_mode_field} and \code{tab_field} in \code{input} overwrite values in \code{pObjects$memory} for the current panel.
#' \item \code{pObjects$aesthetics_links} is modified for the based on the newly linked panel in \code{input[[tab_field]]}.
#' However, it may also remove an edge if \code{input[[use_mode]]} is altered so that it is no longer equal to \code{use_value}.
#' \item Link information panels are updated if the new linked panel differs from the old linked panel.
#' \item The selectize UI element corresponding to \code{name_field} is updated with the current selection in the linked table, if a new linked table was chosen.
#' }
#'
#' Note that \code{use_mode_field} and \code{use_value} are ignored if the former is \code{NA}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_setup_dimname_source_observer
#' @seealso
#' \code{\link{.singleSelectionSlots}}, which is used to set up this observer in \code{\link{.createObservers,Panel-method}}.
#'
#' @importFrom shiny updateSelectizeInput
#' @importFrom methods as
.setup_dimname_source_observer <- function(panel_name, use_mode_field, use_value, name_field, tab_field, choices,
    input, session, pObjects, rObjects)
{
    .input_FUN <- function(field) paste0(panel_name, "_", field)
    uses_use_mode_field <- !is.na(use_mode_field)

    # Checking that all the incoming arguments are non-NULL.
    if (uses_use_mode_field) {
        choice <- input[[.input_FUN(use_mode_field)]]
    } else {
        choice <- use_value <- ""
    }
    tab <- input[[.input_FUN(tab_field)]]
    if (is.null(choice) || is.null(tab)) {
        return(FALSE)
    }

    # Obtaining the old parameter choices, enforcing type and updating memory.
    # The new values should persist due to environment's pass-by-reference.
    if (uses_use_mode_field) {
        old_choice <- pObjects$memory[[panel_name]][[use_mode_field]]
        choice <- as(choice, typeof(old_choice))
        pObjects$memory[[panel_name]][[use_mode_field]] <- choice
    } else {
        old_choice <- choice
    }

    old_tab <- pObjects$memory[[panel_name]][[tab_field]]
    tab <- as(tab, typeof(old_tab))
    pObjects$memory[[panel_name]][[tab_field]] <- tab

    update_info <- FALSE
    if (choice==use_value) {
        if (old_tab!=tab) {
            pObjects$aesthetics_links <- .choose_new_parent(pObjects$aesthetics_links,
                panel_name, tab, old_tab, field=name_field)
            update_info <- TRUE

            # Updating the selection, based on the currently selected row.
            if (tab!=.noSelection) {
                old_selected <- pObjects$memory[[panel_name]][[name_field]]
                new_selected <- .singleSelectionValue(pObjects$memory[[tab]])

                if (!is.null(new_selected) && new_selected != old_selected) {
                    all_choices <- rownames(pObjects$contents[[tab]])
                    updateSelectizeInput(session, .input_FUN(name_field), label = NULL,
                        choices = choices, server = TRUE, selected = new_selected) # nocov
                }
            }
        }
    } else {
        if (old_choice==use_value) {
            pObjects$aesthetics_links <- .delete_interpanel_link(pObjects$aesthetics_links,
                panel_name, old_tab, field=name_field)
            update_info <- TRUE
        }
    }

    if (update_info) {
        tab_names <- setdiff(union(old_tab, tab), .noSelection)
        for (relinked in c(panel_name, tab_names)) {
            .safe_reactive_bump(rObjects, paste0(relinked, "_", .flagRelinkedSelect))
        }
    }

    !identical(old_choice, choice)
}

#' Define dimension name observers
#'
#' Define observers to track changes to fields involving the dimension name.
#'
#' @param panel_name String containing the name of the current panel.
#' @param pObjects An environment containing \code{aesthetics_links}, a graph produced by \code{\link{.spawn_single_selection_graph}};
#' and \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @param rObjects A reactive list containing incrementable counters for all panels,
#' @param input A Shiny list of inputs, generated by the server.
#' @param session A \code{session} object from a Shiny server.
#' @param use_mode_field String specifying the name of the slot that contains a string indicating the usage mode, see \code{\link{.singleSelectionSlots}} for more details.
#' @param use_value String specifying the value of the \code{use_mode_field} slot that indicates that the parameter of interest should respond to the single-selection transmitter.
#' @param name_field String with the name of the slot containing the chosen row/column for this parameter.
#' @param tab_field String with the name of the slot containing the name of the single-selection transmitter for the current panel.
#' @param choices Vector of possible choices for the slot named \code{name_field}.
#' @param protected Logical scalar indicating whether the slot specified by \code{name_field} is a protected parameter for \code{x}, see \code{?\link{.createProtectedParameterObservers}} for details.
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
#' @rdname INTERNAL_dimname_observer
#' @importFrom shiny observeEvent observe updateSelectizeInput req
.create_dimname_observers <- function(panel_name, name_field, choices,
    use_mode_field, use_value, protected, tab_field,
    input, session, pObjects, rObjects)
{
    name_input <- paste0(panel_name, "_", name_field)
    always_in_use <- is.na(use_mode_field)

    # Forcibly evaluating to ensure that the correct values are used in observer expressions.
    force(use_value)
    force(choices)
    force(protected)
    force(tab_field)
    # nocov start
    observeEvent(input[[name_input]], {
        # Required to defend against empty strings before updateSelectizeInput runs upon re-render.
        req(input[[name_input]])

        matched_input <- as(input[[name_input]], typeof(pObjects$memory[[panel_name]][[name_field]]))
        if (identical(matched_input, pObjects$memory[[panel_name]][[name_field]])) {
            return(NULL)
        }
        pObjects$memory[[panel_name]][[name_field]] <- matched_input

        # Only regenerating if the current parameter is actually in use.
        if (always_in_use || pObjects$memory[[panel_name]][[use_mode_field]]==use_value) {
            if (!protected) {
                .requestUpdate(panel_name, rObjects)
            } else {
                .requestCleanUpdate(panel_name, pObjects, rObjects)
            }
        }
    }, ignoreInit=TRUE)
    # nocov end

    # Observer for the linked panel that controls the dimname selection.
    # This either triggers a re-rendering indirectly via the updateSelectizeInput inside (if the choice of transmitter changed)
    # or it triggers a re-rendering explicitly here.
    # nocov start
    observe({
        replot <- .setup_dimname_source_observer(panel_name,
            use_mode_field=use_mode_field, use_value=use_value,
            name_field=name_field, tab_field=tab_field,
            choices=choices,
            input=input, session=session,
            pObjects=pObjects, rObjects=rObjects)

        if (replot) {
            if (!protected) {
                .requestUpdate(panel_name, rObjects)
            } else {
                .requestCleanUpdate(panel_name, pObjects, rObjects)
            }
        }
    })
    # nocov end

    # nocov start
    observe({
        force(rObjects$rerendered)

        # Protect against re-rendering after deleting a panel.
        if (panel_name %in% names(pObjects$memory)) {
            updateSelectizeInput(session, paste0(panel_name, "_", name_field),
                choices=choices, selected=pObjects$memory[[panel_name]][[name_field]], server=TRUE)
        }
    })
    # nocov end

    invisible(NULL)
}
