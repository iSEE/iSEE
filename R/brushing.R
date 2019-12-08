.is_brush <- function(x) {
    length(x) && is.null(x$closed)
}

#' Checks if any points are selected
#'
#' Checks if any points are actively selected via a Shiny brush or closed lasso in a transmitting plot,
#' or if there are any saved selections in the memory of the transmitting plot.
#'
#' @param mode String specifying the (encoded) panel type for the current (transmitting) panel.
#' @param id Integer scalar specifying the ID of the current panel of the specified type.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#'
#' @return A logical scalar specifying whether the specified panel contains an active or saved Shiny brush or a closed lasso.
#' @author Aaron Lun
#' @rdname INTERNAL_any_point_selection
#' @seealso
#' \code{\link{.transmitted_selection}},
#' \code{\link{iSEE}}
.any_active_selection <- function(panel) {
    if (is(panel, "DotPlot")) {
        length(panel[[.brushData]]) > 0L
    } else if (is(panel, "Table")) {
        panel[[.TableSearch]]!="" || any(panel[[.TableColSearch]]!="")
    } else {
        FALSE
    }
}

#' @rdname INTERNAL_any_point_selection
.any_saved_selection <- function(panel, count=TRUE) {
    if (is(panel, "DotPlot")) {
        n <- length(panel[[.multiSelectHistory]]) 
    } else {
        n <- 0L
    }

    if (count) {
        n
    } else {
        n > 0L
    }
}
