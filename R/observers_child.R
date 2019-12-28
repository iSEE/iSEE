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
#' @rdname INTERNAL_child_propagation_observers
#'
#' @importFrom shiny observeEvent
#' @importFrom igraph topo_sort adjacent_vertices
.create_child_propagation_observer <- function(se, pObjects, rObjects) {
    observeEvent(rObjects$modified, {
        if (!isTRUE(pObjects$initialized)) { # Avoid running this on app start and double-generating output.
            pObjects$initialized <- TRUE
            return(NULL)
        }

        modified <- rObjects$modified
        if (length(modified)==0L) { # Avoid recursion from the wiping.
            return(NULL)
        }
        rObjects$modified <- list()

        # Looping over panels in topological order, accumulating changes so that
        # we only ever call .generateOutput once. Note that we must loop over
        # 'ordering' rather than 'modified' to ensure we only call this thing once.
        graph <- pObjects$selection_links
        ordering <- names(topo_sort(graph, mode="out"))

        for (idx in seq_along(ordering)) {
            current_panel_name <- ordering[idx]
            if (!current_panel_name %in% names(modified)) {
                next
            }
            instance <- pObjects$memory[[current_panel_name]]

            # Generating self and marking it for re-rendering.
            .safe_reactive_bump(rObjects, current_panel_name)
            p.out <- .generateOutput(instance, se, all_memory=pObjects$memory, all_contents=pObjects$contents)
            pObjects$contents[[current_panel_name]] <- p.out$contents
            pObjects$cached[[current_panel_name]] <- p.out

            # Setting up various parameters to decide how to deal with children.
            status <- modified[[current_panel_name]]
            re_populated <- .panelRepopulated %in% status
            re_active <- .panelReactivated %in% status
            re_saved <- .panelResaved %in% status
            if (!re_populated && !re_active && !re_saved) {
                next
            }

            children <- names(adjacent_vertices(graph, v=current_panel_name, mode="out"))
            if (!length(children)) {
                next
            }

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

            has_active <- .multiSelectionHasActive(instance)
            n_saved <- .any_saved_selection(instance, count=TRUE)
            has_saved <- n_saved > 0L

            # Looping over children and deciding whether they need to be
            # regenerated. This depends on the combination of what has changed in
            # 'current_panel' + what the child was using (active, saved or union).
            for (child in children) {
                child_instance <- pObjects$memory[[child]]
                select_mode <- child_instance[[type_field]]

                regenerate <- FALSE
                if (select_mode==.selectMultiActiveTitle) {
                    if (re_populated && has_active) {
                        regenerate <- TRUE
                    } else if (re_active) {
                        regenerate <- TRUE
                    }
                } else if (select_mode==.selectMultiSavedTitle) {
                    if (re_populated && has_saved) {
                        regenerate <- TRUE
                    } else if (re_saved) {
                        if (child_instance[[saved_field]] > n_saved) {
                            pObjects$memory[[child]][[saved_field]] <- 0L
                            regenerate <- TRUE
                        }
                    }
                } else if (select_mode==.selectMultiUnionTitle) {
                    if (re_populated && (has_active || has_saved)) {
                        regenerate <- TRUE
                    } else if (re_saved || re_active) {
                        regenerate <- TRUE
                    }
                }

                if (regenerate) {
                    # Implicit convertion to character(0), so as to trigger
                    # the call to .generateOutput later.
                    previous <- as.character(modified[[child]])

                    if (.multiSelectionRestricted(child_instance)) {
                        previous <- union(previous, .panelRepopulated)
                    }

                    # Wiping out selections in the child if receiving a new
                    # selection from the parent invalidates its own selections.
                    if (.multiSelectionInvalidated(child_instance)) {
                        if (.multiSelectionHasActive(child_instance)) {
                            pObjects$memory[[child]] <- .multiSelectionClear(pObjects$memory[[child]])
                            previous <- union(previous, .panelReactivated)
                        }
                        if (.any_saved_selection(child_instance)) {
                            pObjects$memory[[child]][[.multiSelectHistory]] <- list()
                            previous <- union(previous, .panelResaved)
                        }
                    }
                    modified[[child]] <- previous
                }

                # Updating the saved choice selectize for the child.  Note that
                # this is purely for the user, we've already updated the memory
                # if the change invalidated anything. 
                if (re_saved) {
                    .safe_reactive_bump(rObjects, paste0(child, "_", .updateSavedChoices))
                } 
            }
        }
    }, priority=-1L, ignoreInit=TRUE)

    invisible(NULL)
}

.mark_panel_as_modified <- function(panel_name, status, rObjects) {
    rObjects$modified[[panel_name]] <- union(isolate(rObjects$modified[[panel_name]]), status)
    invisible(NULL)
}
