.panelRepopulated <- "INTERNAL_repopulated"
.panelReactivated <- "INTERNAL_reactivated"
.panelResaved <- "INTERNAL_resaved"

#' Child propagating observer
#'
#' These observers decide whether child panels need to be regenerated based on changes in the transmitting (parent) panel for multiple selections.
#'
#' @param panel_name String containing the name of the panel transmitting a multiple selection.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @details
#' We expect \code{rObjects} to contain \code{X_INTERNAL_repopulated}, 
#' \code{X_INTERNAL_reactivated} and \code{X_INTERNAL_resaved} for each panel \code{X}.
#' These are simply integer counters that get triggered every time \code{X} changes.
#'
#' \code{X_repopulated} is bumped when the population of points changes in \code{X}.
#' This refers to changes in the points retained by restriction of selections from transmitters upstream of \code{X}.
#' Bumping will trigger replotting of the children of \code{X}, based on whether they are receiving the active or saved selection.
#' It will also bump the children's \code{X_repopulated} if they are selecting by restriction.
#'
#' \code{X_reactivated} is bumped when the current selection of points in \code{X} changes.
#' \code{X_resaved} is bumped when the saved selection of points in \code{X} changes.
#' These use separate observers from \code{X_repopulated} because they only trigger replotting if
#' the children are receiving active or saved selections, respectively.
#'
#' @author Aaron Lun
#'
#' @importFrom shiny isolate observe
#' @rdname INTERNAL_child_propagation_observers
.create_child_propagation_observers <- function(panel_name, session, pObjects, rObjects) {
    # Reactive to regenerate children when the point population of the current panel changes.
    repop_name <- paste0(panel_name, "_", .panelRepopulated)
    .safe_reactive_init(rObjects, repop_name)

    observeEvent(rObjects[[repop_name]], {
        instance <- pObjects$memory[[panel_name]]
        has_active <- .multiSelectionHasActive(instance)
        has_saved <- .any_saved_selection(instance)
        transmit_dim <- .multiSelectionDimension(instance)
        if (transmit_dim=="row") {
            type_field <- .selectRowType
            saved_field <- .selectRowSaved
        } else if (transmit_dim=="column") {
            type_field <- .selectColType
            saved_field <- .selectColSaved
        } else {
            return(NULL)
        }

        children <- names(.get_direct_children(pObjects$selection_links, panel_name))
        for (child_plot in children) {
            child_instance <- pObjects$memory[[child_plot]]

            # To warrant replotting of the child, there needs to be
            # Active or Saved selections in the current panel.
            # Any point population changes in the current panel will
            # result in new subsets if those selections are available.
            replot <- FALSE
            select_mode <- child_instance[[type_field]]
            if (select_mode==.selectMultiActiveTitle) {
                if (has_active) replot <- TRUE
            } else if (select_mode==.selectMultiSavedTitle && child_instance[[saved_field]]!=0L) {
                if (has_saved) replot <- TRUE
            } else if (select_mode==.selectMultiUnionTitle) {
                if (has_saved || has_active) replot <- TRUE
            }

            if (replot) {
                .safe_reactive_bump(rObjects, child_plot)

                # To warrant replotting of the grandchildren, the child must itself be restricted.
                if (.multiSelectionRestricted(child_instance)) {
                    .safe_reactive_bump(rObjects, paste0(child_plot, "_", .panelRepopulated))
                }
            }
        }
    })
 
    # Reactive to regenerate children when the active selection of the current panel changes.
    act_name <- paste0(panel_name, "_", .panelReactivated)
    .safe_reactive_init(rObjects, act_name)

    dimprop_name <- paste0(panel_name, "_", .propagateDimnames)
    .safe_reactive_init(rObjects, dimprop_name)

    observeEvent(rObjects[[act_name]], {
        instance <- pObjects$memory[[panel_name]]
        transmit_dim <- .multiSelectionDimension(instance)
        if (transmit_dim=="row") {
            type_field <- .selectRowType
            saved_field <- .selectRowSaved
        } else if (transmit_dim=="column") {
            type_field <- .selectColType
            saved_field <- .selectColSaved
        } else {
            return(NULL)
        }

        children <- names(.get_direct_children(pObjects$selection_links, panel_name))
        for (child_plot in children) {
            child_instance <- pObjects$memory[[child_plot]]

            select_mode <- child_instance[[type_field]]
            if (select_mode==.selectMultiActiveTitle || select_mode==.selectMultiUnionTitle) {
                .safe_reactive_bump(rObjects, child_plot)

                # To warrant replotting of the grandchildren, the child must itself be restricted.
                if (.multiSelectionRestricted(child_instance)) {
                    .safe_reactive_bump(rObjects, paste0(child_plot, "_", .panelRepopulated))
                }
            }
        }
    })

    # Reactive to regenerate children when the saved selection of the current panel changes.
    save_name <- paste0(panel_name, "_", .panelResaved)
    .safe_reactive_init(rObjects, save_name)

    observeEvent(rObjects[[save_name]], {
        instance <- pObjects$memory[[panel_name]]
        Nsaved <- length(instance[[.multiSelectHistory]])
        transmit_dim <- .multiSelectionDimension(instance)
        if (transmit_dim=="row") {
            type_field <- .selectRowType
            saved_field <- .selectRowSaved
        } else if (transmit_dim=="column") {
            type_field <- .selectColType
            saved_field <- .selectColSaved
        } else {
            return(NULL)
        }

        children <- names(.get_direct_children(pObjects$selection_links, panel_name))
        for (child_plot in children) {
            child_instance <- pObjects$memory[[child_plot]]

            reset <- child_instance[[saved_field]] > Nsaved
            if (reset) {
                pObjects$memory[[child_plot]][[saved_field]] <- 0L
            }

            child_select_type <- child_instance[[type_field]]
            if (child_select_type==.selectMultiUnionTitle || (child_select_type==.selectMultiSavedTitle && reset)) {
                .safe_reactive_bump(rObjects, child_plot)

                # To warrant replotting of the grandchildren, the child must itself be restricted.
                if (.multiSelectionRestricted(child_instance)) {
                    .safe_reactive_bump(rObjects, paste0(child_plot, "_", .panelRepopulated))
                }
            }

            # Updating the selectize as well.
            child_saved <- paste0(child_plot, "_", .updateSavedChoices)
            .safe_reactive_bump(rObjects, child_saved)
        }
    })

    invisible(NULL)
}
