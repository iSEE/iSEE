#' Checks if there is a relevant selection
#'
#' Checks whether there is a Shiny brush or lasso selection from a transmitter, in the active selection or in the saved selection history,
#' that is relevant to the current panel.
#'
#' @param transmitter String containing the name of the transmitting panel.
#' By default, an encoded panel name is expected.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param select_type String specifying whether the current panel is receiving the \code{"Active"}, \code{"Union"} or \code{"Saved"} selections.
#' @param select_saved Integer specifying which saved selection is received by the current panel when \code{select_type="Saved"}.
#' @param mode String specifying the (encoded) panel type of the current panel.
#' @param id Integer scalar specifying the ID of the current panel of the specified type.
#' @param encoded Logical scalar specifying whether \code{transmitter} is an encoded panel name.
#'
#' @return A logical scalar specifying whether there is a relevant selection.
#'
#' @details
#' This will look for saved or active selections (or both) depending on the value of \code{select_type}.
#' An active selection will not be relevant when \code{select_type="Saved"}, and vice versa.
#'
#' \code{mode} and \code{id} will be used to retrieve the select type and saved index from \code{memory},
#' only if \code{select_type} and \code{select_saved} are not specified.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_transmitted_selection
#' @seealso
#' \code{\link{.any_active_selection}},
#' \code{\link{.any_saved_selection}},
#' \code{\link{iSEE}}
.transmitted_selection <- function(panel_name, parent_name, all_memory, select_type, select_saved) {
    if (parent_name==.noSelection) {
        return(FALSE)
    }
    panel <- all_memory[[panel_name]]
    transmitter <- all_memory[[parent_name]]

    changed <- FALSE
    if (select_type==.selectMultiActiveTitle || select_type==.selectMultiUnionTitle) {
        if (.multiSelectionHasActive(transmitter)) {
            changed <- TRUE
        }

        if (select_type==.selectMultiUnionTitle) {
            if (.any_saved_selection(transmitter)) {
                changed <- TRUE
            }
        }
    } else {
        # In principle, we don't have to check the transmitter options here, because
        # the saved index should always be valid. In practice, the saved index might
        # not be valid if this function is called after the transmitter is changed
        # but before the .selectMultiSaved selectize is updated. However, if it was
        # non-zero and invalid, then the update would cause it to be zero, which
        # would set changed=TRUE anyway.
        if (select_saved!=0L) {
            changed <- TRUE
        }
    }

    changed
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
.any_saved_selection <- function(panel, count=TRUE) {
    n <- length(panel[[.multiSelectHistory]])
    if (count) {
        n
    } else {
        n > 0L
    }
}
