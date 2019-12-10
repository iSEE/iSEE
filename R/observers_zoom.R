#' Zoom observers
#'
#' Observers for the zoom functionality.
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
#' @importFrom shiny observeEvent
#' @rdname INTERNAL_zoom_observers
.define_zoom_observer <- function(plot_name, input, session, pObjects, rObjects) {
    dblclick_field <- paste0(plot_name, "_", .zoomClick)
    act_field <- paste0(plot_name, "_reactivated")
    save_field <- paste0(plot_name, "_", .multiSelectSave)

    observeEvent(input[[dblclick_field]], {
        existing_brush <- pObjects$memory[[plot_name]][[.brushData]]

        # Zooming destroys all active brushes or lassos. 
        pObjects$memory[[plot_name]][[.brushData]] <- list()

        new_coords <- numeric(0)
        if (.is_brush(existing_brush)) {
            dblclick_vals <- input[[dblclick_field]]
            if (dblclick_vals$x >= existing_brush$xmin
                    && dblclick_vals$x <= existing_brush$xmax
                    && dblclick_vals$y >= existing_brush$ymin
                    && dblclick_vals$y <= existing_brush$ymax) {

                # Panels are either NULL or not.
                if (identical(dblclick_vals$panelvar1, existing_brush$panelvar1)
                        && identical(dblclick_vals$panelvar2, existing_brush$panelvar2)) {
                    new_coords <- c(xmin=existing_brush$xmin, xmax=existing_brush$xmax,
                        ymin=existing_brush$ymin, ymax=existing_brush$ymax)
                }
            }
            .disableButtonIf(
                save_field,
                TRUE,
                .buttonNoSelectionLabel, .buttonSaveLabel, session
            )
        }

        pObjects$memory[[plot_name]][[.zoomData]] <- new_coords
        .safe_reactive_bump(rObjects, plot_name)

        # While re-creating the plot clears the brush, it doesn't 
        # re-trigger the observer as the observer ignores NULLs.
        # So we have to manually retrigger the downstream effects.
        if (.is_brush(existing_brush) || (.is_lasso(existing_brush) && existing_brush$closed)) {
            .safe_reactive_bump(rObjects, act_field)
        }
    })
}
