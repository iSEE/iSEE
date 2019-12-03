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
    act_field <- paste0(plot_name, "_reactivated")
    save_field <- paste0(plot_name, "_", .multiSelectSave)
    brush_id <- paste0(plot_name, "_", .brushField)
    observeEvent(input[[brush_id]], {
        cur_brush <- input[[brush_id]]
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

        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))
        rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
    }, ignoreInit=TRUE)

    invisible(NULL)
}

#' Test if Shiny brushes are identical
#' 
#' Check whether brush coordinates have actually changed between two Shiny brush objects.
#'
#' @param old_brush A Shiny brush object with the elements \code{xmin}, \code{xmax}, \code{ymin} and \code{ymax}.
#' @param new_brush Another  Shiny brush object for the same plot.
#'
#' @details
#' This function checks whether there are any significant differences in the rectangular regions defined by two Shiny brushes.
#' If there is no change, there is no need to waste time updating the plot.
#'
#' The tolerance is defined as one millionth of the x- and y-axis range for \code{xmin}/\code{xmax} and \code{ymin}/\code{ymax}, respectively.
#' We do not use \code{all.equal(old_brush, new_brush)}, as the plot domain can sometimes change without affecting the actual brush coordinates.
#'
#' @return A logical scalar indicating whether the brushes are identical.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_identical_brushes
#' @seealso
#' \code{\link{iSEE}}
.identical_brushes <- function(old_brush, new_brush) {
    old_null <- length(old_brush)==0L
    new_null <- length(new_brush)==0L
    if (old_null || new_null) {
        return(old_null==new_null)
    }

    if (!.is_brush(old_brush) || !.is_brush(new_brush)) {
        return(FALSE)
    }

    xspan <- old_brush$xmax - old_brush$xmin
    tol <- xspan * 1e-6
    if (abs(old_brush$xmin - new_brush$xmin) > tol
        || abs(old_brush$xmax - new_brush$xmax) > tol) {
      return(FALSE)
    }

    yspan <- old_brush$ymax - old_brush$ymin
    tol <- yspan * 1e-6
    if (abs(old_brush$ymin - new_brush$ymin) > tol
        || abs(old_brush$ymax - new_brush$ymax) > tol) {
      return(FALSE)
    }

    TRUE
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
    act_field <- paste0(plot_name, "_reactivated")
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
                new_lasso <- NULL
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

        rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

        if (reactivated) {
            rObjects[[act_field]] <- .increment_counter(isolate(rObjects[[act_field]]))
        }
    })

    invisible(NULL)
}


