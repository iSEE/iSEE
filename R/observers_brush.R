#' Brush observers
#'
#' A function to set up observers for brushing on point-based plots, as used in the app.
#'
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @details
#' There are three "phases" of a Shiny brush:
#' \itemize{
#' \item the Javascript (JS) brush, which is what the user draws and the observer responds to.
#'   This is eliminated upon replotting for various consistency reasons.
#' \item the active brush, which is what is stored in the \code{.brushData} field of the memory.
#' \item the saved brush(es), stored in the \code{.multiSelectHistory} field of the memory.
#' }
#' This particular observer only deals with the first and second elements, updating them as necessary.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @rdname INTERNAL_brush_observers
#' @author Aaron Lun
#' @importFrom shiny observeEvent
.define_brush_observer <- function(plot_name, input, session, pObjects, rObjects) {
    act_name <- paste0(plot_name, "_", .panelReactivated)
    save_field <- paste0(plot_name, "_", .multiSelectSave)

    brush_field <- paste0(plot_name, "_", .brushField)
    observeEvent(input[[brush_field]], {
        cur_brush <- input[[brush_field]]
        old_brush <- pObjects$memory[[plot_name]][[.brushData]]
        pObjects$memory[[plot_name]][[.brushData]] <- cur_brush

        # If the Shiny brushes have the same coordinates, we don't bother replotting.
        if (.identical_brushes(cur_brush, old_brush)) {
            return(NULL)
        }

        .disableButtonIf(
            save_field,
            is.null(cur_brush),
            .buttonNoSelectionLabel, .buttonSaveLabel, session
        )

        .safe_reactive_bump(rObjects, plot_name)
        .safe_reactive_bump(rObjects, act_name)
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#' Lasso selection observers
#'
#' Observers for the lasso selection.
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
#' @rdname INTERNAL_lasso_observers
.define_lasso_observer <- function(plot_name, input, session, pObjects, rObjects) {
    click_field <- paste0(plot_name, "_", .lassoClick)
    brush_field <- paste0(plot_name, "_", .brushField)
    act_name <- paste0(plot_name, "_", .panelReactivated)
    save_field <- paste0(plot_name, "_", .multiSelectSave)

    observeEvent(input[[click_field]], {
        # Hack to resolve https://github.com/rstudio/shiny/issues/947.
        # By luck, the triggering of the click field seems to be delayed enough
        # that input data is sent to the brush field first. Thus, we can
        # check the brush field for a non-NULL value avoid action if
        # the user had brushed rather than clicked. A separate click should
        # continue past this point, as any Shiny brush would be wiped upon
        # replotting and thus would not have any value in the input.
        if (!is.null(input[[brush_field]])) {
            return(NULL)
        }

        # Don't add to waypoints if a Shiny brush exists in memory, but instead, destroy the brush.
        # Also destroy any closed lassos, or update open lassos.
        reactivated <- FALSE
        prev_lasso <- pObjects$memory[[plot_name]][[.brushData]]
        if (.is_brush(prev_lasso)) {
            new_lasso <- list()
            reactivated <- TRUE
        } else {
            was_closed <- prev_lasso$closed
            if (is.null(was_closed)) {
                was_closed <- FALSE
            }

            if (was_closed) {
                new_lasso <- list()
                reactivated <- TRUE
            } else {
                new_lasso <- .update_lasso(input[[click_field]], prev_lasso)
                if (new_lasso$closed) {
                    reactivated <- TRUE
                }
            }

        }

        pObjects$memory[[plot_name]][[.brushData]] <- new_lasso

        .disableButtonIf(
            save_field,
            !isTRUE(new_lasso$closed),
            .buttonNoSelectionLabel, .buttonSaveLabel, session
        )

        .safe_reactive_bump(rObjects, plot_name)

        if (reactivated) {
            .safe_reactive_bump(rObjects, act_name)
        }
    })

    invisible(NULL)
}

#' Multiple selection observers
#'
#' Observers to change the multiple selections by saving the active selection or deleting existing saved selections.
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
#' @rdname INTERNAL_multiple_select_observers
.define_saved_selection_observers <- function(plot_name, input, session, pObjects, rObjects) {
    save_field <- paste0(plot_name, "_", .multiSelectSave)
    del_field <- paste0(plot_name, "_", .multiSelectDelete)
    info_name <- paste0(plot_name, "_", .panelGeneralInfo)
    saved_select_name <- paste0(plot_name, "_", .updateSavedChoices)
    resaved_name <- paste0(plot_name, "_", .panelResaved)

    ## Save selection observer. ---
    observeEvent(input[[save_field]], {
        instance <- pObjects$memory[[plot_name]]
        current <- instance[[.multiSelectHistory]]
        to_store <- instance[[.brushData]]
        if (!length(to_store) || (!.is_brush(to_store) && !to_store$closed)) {
            return(NULL)
        }

        pObjects$memory[[plot_name]][[.multiSelectHistory]] <- c(current, list(to_store))

        # Updating self (replot to get number).
        .safe_reactive_bump(rObjects, info_name)
        .safe_reactive_bump(rObjects, plot_name)

        trans_row <- .transmittedSelection(instance)=="row"
        by_field <- if (trans_row) .selectRowSource else .selectColSource
        if (instance[[by_field]]==plot_name) {
            .safe_reactive_bump(rObjects, saved_select_name)
        }

        # Updating children.
        .safe_reactive_bump(rObjects, resaved_name)

        .disableButtonIf(
            del_field,
            FALSE,
            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
        )
    })

    ## Deleted selection observer. ---
    observeEvent(input[[del_field]], {
        instance <- pObjects$memory[[plot_name]]
        current <- instance[[.multiSelectHistory]]
        current <- head(current, -1)
        pObjects$memory[[plot_name]][[.multiSelectHistory]] <- current

        # Updating self.
        .safe_reactive_bump(rObjects, info_name)
        .safe_reactive_bump(rObjects, plot_name)

        trans_row <- .transmittedSelection(instance)=="row"
        by_field <- if (trans_row) .selectRowSource else .selectColSource
        if (instance[[by_field]]==plot_name) {
            .safe_reactive_bump(rObjects, saved_select_name)

            saved_field <- if (trans_row) .selectRowSaved else .selectColSaved
            if (instance[[saved_field]] > length(current)) {
                pObjects$memory[[plot_name]][[saved_field]] <- 0L
            }
        }

        # Updating children.
        .save_reactive_bump(rObjects, resaved_name)

        .disableButtonIf(
            del_field,
            length(current)==0,
            .buttonEmptyHistoryLabel, .buttonDeleteLabel, session
        )
    })

    invisible(NULL)
}
