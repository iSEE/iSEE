#' Brush observer
#'
#' A function to set up observers for brushing on \linkS4class{DotPlot}s.
#'
#' @param plot_name String containing the name of the plot panel containing the brush.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @details
#' There are three "phases" of a Shiny brush:
#' \itemize{
#' \item the Javascript (JS) brush, which is what the user draws and the observer responds to.
#' This is eliminated upon replotting for various consistency reasons.
#' \item the active brush, which is what is stored in the \code{.brushData} field of the memory.
#' \item the saved brush(es), stored in the \code{.multiSelectHistory} field of the memory.
#' }
#' This particular observer only deals with the first and second elements, updating the latter with the former as necessary.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @seealso
#' \code{\link{.createObservers,DotPlot-method}}, where this function is called.
#' @rdname INTERNAL_brush_observers
#' @author Aaron Lun
#' @importFrom shiny observeEvent
.create_brush_observer <- function(plot_name, se, input, session, pObjects, rObjects) {
    act_name <- paste0(plot_name, "_", .panelReactivated)
    save_field <- paste0(plot_name, "_", .multiSelectSave)
    dimprop_name <- paste0(plot_name, "_", .propagateDimnames)

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

        .refreshPanelOutput(plot_name, se, pObjects, rObjects)
        .safe_reactive_bump(rObjects, act_name)
        .safe_reactive_bump(rObjects, dimprop_name)
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#' Lasso selection observers
#'
#' Observers for the lasso selection feature of \linkS4class{DotPlot}s.
#'
#' @param plot_name String containing the name of the plot panel containing the brush.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @details
#' Unlike Shiny brushing, the lasso involves some work to check whether the click event closes the lasso.
#' Only a closed lasso will result in rendering the children of \code{plot_name}; 
#' before that, no selection is considered to have been made.
#'
#' Like brushing, the lasso structure itself is stored in the \code{.brushData} slot.
#' Both lassos and Shiny brushes are considered to be specializations of the \dQuote{brush} concept.
#' Practically, we re-use this slot to make it clear that we can only have one brush or lasso at any given time.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{.createObservers,DotPlot-method}}, where this function is called.
#' @importFrom shiny observeEvent isolate
#' @rdname INTERNAL_lasso_observers
.create_lasso_observer <- function(plot_name, se, input, session, pObjects, rObjects) {
    click_field <- paste0(plot_name, "_", .lassoClick)
    brush_field <- paste0(plot_name, "_", .brushField)
    act_name <- paste0(plot_name, "_", .panelReactivated)
    dimprop_name <- paste0(plot_name, "_", .propagateDimnames)
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
        } else if (.is_closed_lasso(prev_lasso)) {
            new_lasso <- list()
            reactivated <- TRUE
        } else {
            new_lasso <- .update_lasso(input[[click_field]], prev_lasso)
            if (new_lasso$closed) {
                reactivated <- TRUE
            }
        }

        pObjects$memory[[plot_name]][[.brushData]] <- new_lasso
        .refreshPanelOutput(plot_name, se, pObjects, rObjects)

        .disableButtonIf(
            save_field,
            !isTRUE(new_lasso$closed),
            .buttonNoSelectionLabel, .buttonSaveLabel, session
        )

        if (reactivated) {
            .safe_reactive_bump(rObjects, act_name)
            .safe_reactive_bump(rObjects, dimprop_name)
        }
    })

    invisible(NULL)
}
