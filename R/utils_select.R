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

#' Choose a linked panel
#'
#' Chooses a linked panel from those available, forcing a valid choice if required.
#'
#' @param chosen String specifying the proposed choice, usually a decoded panel name.
#' @param available Character vector containing the valid choices, usually decoded panel names.
#' @param force_default Logical scalar indicating whether a non-empty default should be returned if \code{chosen} is not valid.
#'
#' @return A string containing a valid choice, or an empty string.
#'
#' @details
#' If \code{chosen} is in \code{available}, it will be directly returned.
#' If not, and if \code{force_default=TRUE} and \code{available} is not empty, the first element of \code{available} is returned.
#' Otherwise, an empty string is returned.
#'
#' Setting \code{force_default=TRUE} is required for panels linking to row statistics tables, where an empty choice would result in an invalid plot.
#' However, a default choice is not necessary for point selection transmission, where no selection is perfectly valid.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_choose_link
#' @seealso
#' \code{\link{.panel_generation}}
.choose_link <- function(chosen, available, force_default=FALSE) {
    if (!chosen %in% available) {
        if (force_default && length(available)) {
            return(available[1])
        }
        return("")
    }
    return(chosen)
}

#' Process point selection choice
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param all_memory A list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param self_source A logical scalar indicating whether it is allowable to select points based on coordinates in \code{plot.data}.
#'
#' @return A list containing:
#' \itemize{
#' \item \code{cmds}, a character vector of commands that results in the addition of a \code{SelectBy} covariate column in the \code{plot.data} data.frame.
#' If no selection should be applied, this is set to \code{NULL}.
#' \item \code{transmitter}, a list containing the encoded plot name and ID of the transmitting plot for the current panel (see \code{\link{.encode_panel_name}}).
#' If no selection should be applied, this is set to \code{NULL}.
#' }
#'
#' @details
#' For the current panel, this function identifies the transmitting panel, i.e., from which the current panel is receiving a selection of data points.
#' It then generates the commands necessary to identify the points selected in the transmitter, to add as \code{SelectBy} in the current panel.
#' This requires extraction of the Shiny brush or lasso waypoints in the transmitter, which are used during evaluation to define the selection.
#'
#' If selecting to restrict, an extra \code{plot.data.all} variable will be generated in the evaluation environment.
#' This will be used in \code{\link{.scatter_plot}} and \code{\link{.violin_plot}} to define the boundaries of the plot based on the full data.
#' In this manner, the boundaries of the plot are kept consistent even when only a subset of the data are used to generate the ggplot object.
#'
#' Setting \code{self_source=FALSE} is used in \code{\link{.get_selected_points}} to force it to use existing coordinates to define \code{SelectBy}.
#'
#' Evaluation of the output commands require:
#' \itemize{
#' \item a list object called \code{all_brushes} where each entry is named by the plot name.
#' The entry corresponding to the transmitting plot should contain the contents of \code{.brushData} in its parameter set in \code{all_memory}.
#' \item a list object called \code{all_lassos} where each entry is named by the plot name.
#' The entry corresponding to the transmitting plot should contain the contents of \code{.lassoData} in its parameter set in \code{all_memory}.
#' \item a list object called \code{all_select_histories} where each entry is named by the plot name.
#' The entry corresponding to the transmitting plot should contain the contents of \code{.multiSelectHistory} in its parameter set in \code{all_memory}.
#' }
#' All of these objects should exist in the environment in which the commands are evaluated.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_process_selectby_choice
#' @seealso
#' \code{\link{.extract_plotting_data}},
#' \code{\link{brushedPoints}},
#' \code{\link{in.out}}
#'
#' @importFrom mgcv in.out
#' @importFrom shiny brushedPoints
.process_selectby_choice <- function(param_choices, by_field, type_field, saved_field, all_memory, var_name="selected_pts") {
    transmitter <- param_choices[[by_field]]
    cmds <- list()

    if (!identical(transmitter, .noSelection)) {
        init_cmd <- sprintf("contents <- all_contents[['%s']]", transmitter)

        transmit_param <- all_memory[[transmitter]]
        cur_choice <- param_choices[[type_field]]
        if (cur_choice == .selectMultiUnionTitle) {
            select_sources <- c(NA_integer_, seq_along(transmit_param[[.multiSelectHistory]]))
        } else if (cur_choice == .selectMultiActiveTitle) {
            select_sources <- NA_integer_
        } else {
            select_sources <- param_choices[[saved_field]]
            if (select_sources == 0L) {
                # '0' selection in memory means no selection.
                select_sources <- integer(0)
            }
        }

        if (any(is.na(select_sources))) {
            init_cmd <- c(init_cmd, sprintf("active <- all_active[['%s']]", transmitter))
        }
        if (any(!is.na(select_sources))) {
            init_cmd <- c(init_cmd, )
        }

        for (i in select_sources) {
            cur_cmds <- .multiSelectionCommands(transmit_param, i)
            if (is.null(cur_cmds)) {
                next
            }

            if (is.na(i)) {
                cur_cmds <- c(sprintf("select <- all_active[['%s']]", transmitter), cur_cmds)
            } else {
                cur_cmds <- c(sprintf("select <- all_saved[['%s']]", transmitter), cur_cmds)
            }

            outname <- if (is.na(i)) "active" else paste0("saved", i)
            cmds[[outname]] <- c(cur_cmds, sprintf("%s[[%s]] <- selected;", var_name, deparse(outname)))
        }

        if (length(cmds)) {
            cmds <- c(init=c(init_cmd, paste(var_name, "<- list();")), cmds)
        }
    }
    unlist(cmds)
}

#' Populate selection structures
#'
#' Populate the environment with data structures required for selection.
#'
#' @param param_choices A single-row DataFrame that contains all the input settings for the current panel.
#' @param envir An environment produced by \code{\link{.extract_plotting_data}}.
#'
#' @details
#' This function provides a convenient wrapper to add Shiny brush and lasso objects from any panel to the evaluation environment.
#' These are needed for certain commands to be executed (see the \dQuote{See also} section below).
#'
#' @return \code{all_brushes}, \code{all_lassos} and \code{all_select_histories} are added to \code{envir} using pass-by-reference behaviour of environments.
#' A\code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun.
#' @rdname INTERNAL_populate_selection_environment
#' @seealso
#' \code{\link{.process_selectby_choice}},
#' \code{\link{.self_brush_box}},
#' \code{\link{.self_lasso_path}}
.populate_selection_environment <- function(x, envir) {
    envir$all_active <- list(.multiSelectionActive(x))
    envir$all_saved <- list(x[[.multiSelectHistory]])
    names(envir$all_active) <- names(envir$all_saved) <- .getEncodedName(x)
    invisible(NULL)
}

.multiSelectionHasActive <- function(x) {
    !is.null(.multiSelectionActive(x))
}
