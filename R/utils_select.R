#' Checks if there is a relevant selection
#'
#' Checks whether there is a multiple selection from a transmitter, 
#' either active or in the saved history, that is relevant to the current panel.
#'
#' @param panel_name String containing the name of the current panel.
#' @param parent_name String containing the name of the transmitting panel.
#' @param all_memory A named list of \linkS4class{Panel}s representing the current state of the application.
#' @param select_type String specifying whether the current panel is receiving the \code{"Active"}, \code{"Union"} or \code{"Saved"} selections.
#' @param select_saved Integer specifying which saved selection is received by the current panel when \code{select_type="Saved"}.
#'
#' @return A logical scalar specifying whether there is a relevant selection.
#'
#' @details
#' This will look for saved or active selections (or both) depending on the value of \code{select_type}.
#' An active selection will not be relevant when \code{select_type="Saved"}, and vice versa.
#'
#' Technically, \code{select_type} and \code{select_saved} could be pulled from \code{all_memory} for \code{panel_name}.
#' However, we require them as arguments here as there are cases where we need to check against an old version of those values;
#' see the relevant observer in \code{\link{.create_multi_selection_type_observers}} for more details.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_transmitted_selection
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


#' Checks if there are saved selections
#'
#' Checks if there are any saved multiple selections in a panel.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param count Logical scalar indicating whether the number of saved selections should be returned.
#'
#' @return If \code{count=TRUE}, an integer scalar specifying the number of saved selections.
#'
#' Otherwise, a logical scalar indicating if there are non-zero saved selections.
#' @author Aaron Lun
#' @rdname INTERNAL_any_saved_selection
.any_saved_selection <- function(x, count=TRUE) {
    n <- length(x[[.multiSelectHistory]])
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
#' @param chosen String specifying the proposed choice, usually a panel name.
#' @param available Character vector containing the valid choices, usually panel names.
#'
#' @return A string containing a valid choice, or an empty string.
#'
#' @details
#' If \code{chosen} is in \code{available}, it will be directly returned.
#' If not, and if \code{force_default=TRUE} and \code{available} is not empty, the first element of \code{available} is returned.
#' Otherwise, an empty string is returned.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_choose_link
#' @seealso
#' \code{\link{.panel_generation}}
.choose_link <- function(chosen, available) {
    if (!chosen %in% available) {
        if (length(available)) {
            return(available[1])
        }
        return("")
    }
    return(chosen)
}

#' Process multiple selections
#'
#' Generate and execute commands to process multiple selections,
#' creating variables in the evaluation environment with the identity of the selected rows or columns.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param all_memory A named list of \linkS4class{Panel} instances containing parameters for the current app state.
#' @param all_contents A named list of arbitrary contents with one entry per panel.
#' @param envir The evaluation environment.
#' This is assumed to already contain \code{se}, the \linkS4class{SummarizedExperiment} object for the current dataset.
#'
#' @return 
#' \code{envir} is populated with one, none or both of \code{col_selected} and/or \code{row_selected},
#' depending on whether \code{x} is receiving a multiple selection on the rows and/or columns.
#' The return value is the  character vector of commands required to construct those variables.
#'
#' @details
#' This function is primarily intended for use by developers of new panels.
#' It should be called inside \code{\link{.generateOutput}} to easily process row/column multiple selections.
#' Developers can check whether \code{row_selected} or \code{col_selected} exists in \code{envir}
#' to determine whether any row or column selection was performed (and adjust the behavior of \code{.generateOutput} accordingly).
#'
#' @author Aaron Lun
#'
#' @seealso
#' \code{\link{.generateOutput}} and its related generic \code{\link{.renderOutput}}, where this function should generally be used.
#'
#' @rdname processMultiSelections
#' @export
.processMultiSelections <- function(x, all_memory, all_contents, envir) {
    # Defining the row and column selections, and hoping that the
    # plot-generating functions know what to do with them.
    select_cmds <- list()
    row_select_cmds <- .process_selectby_choice(x,
        by_field=.selectRowSource, type_field=.selectRowType, saved_field=.selectRowSaved,
        all_memory=all_memory, varname="row_selected")

    if (!is.null(row_select_cmds)) {
        transmitter <- x[[.selectRowSource]]
        .populate_selection_environment(all_memory[[transmitter]], envir)
        envir$all_contents <- all_contents
        .textEval(row_select_cmds, envir)
        select_cmds[["row"]] <- row_select_cmds
    }

    col_select_cmds <- .process_selectby_choice(x,
        by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
        all_memory=all_memory, varname="col_selected")

    if (!is.null(col_select_cmds)) {
        transmitter <- x[[.selectColSource]]
        .populate_selection_environment(all_memory[[transmitter]], envir)
        envir$all_contents <- all_contents
        .textEval(col_select_cmds, envir)
        select_cmds[["col"]] <- col_select_cmds
    }

    if (length(select_cmds)) {
        unlist(select_cmds)
    } else {
        character(0)
    }
}

#' Process multiple selection choice
#'
#' Return appropriate commands to generate a multiple selection from a transmitter.
#'
#' @param x An instance of a \linkS4class{Panel} class that is \emph{receiving} a transmission.
#' @param by_field String specifying the name of the slot containing the identity of the transmitting panel.
#' @param type_field String specifying the name of the slot containing the type of multiple selection to use from the transmitter.
#' @param saved_field String specifying the name of the slot containing the index of the saved selection to use from the transmitter.
#' @param all_memory A list of \linkS4class{Panel} objects that represents the current state of the application.
#' @param varname String containing the name of the variable that contains all selections.
#'
#' @return A character vector of commands that, when executed, results in a list named \code{varname}.
#' Each entry of the list is a character vector that contains the identities of points in the active and/or saved selections.
#'
#' List names are set to \code{"active"}, \code{"saved1"}, \code{"saved2"}, etc.
#' Whether the active and/or saved selections are returned depends on the values of \code{type_field} and \code{saved_field} in \code{x}.
#'
#' If no selection should be applied, \code{NULL} is returned instead.
#'
#' @details
#' For the current panel, this function identifies the transmitting panel, i.e., from which the current panel is receiving a selection of data points.
#' It then generates the commands necessary to identify the points selected in the transmitter, to add as \code{SelectBy} in the current panel.
#'
#' Evaluation of the output commands require:
#' \itemize{
#' \item a list object called \code{all_active} where each entry is named by the panel name.
#' The entry corresponding to the transmitting plot should contain the active selection structure defined by \code{\link{.multiSelectionActive}}.
#' \item a list object called \code{all_saved} where each entry is named by the panel name.
#' The entry corresponding to the transmitting plot should contain the contents of \code{.multiSelectHistory} in its parameter set in \code{all_memory}.
#' }
#' All of these objects should exist in the environment in which the commands are evaluated.
#'
#' @author Kevin Rue-Albrecht, Aaron Lun.
#' @rdname INTERNAL_process_selectby_choice
#' @seealso
#' \code{\link{.processMultiSelections}},
#'
#' @importFrom mgcv in.out
#' @importFrom shiny brushedPoints
.process_selectby_choice <- function(x, by_field, type_field, saved_field, all_memory, varname="selected_pts") {
    transmitter <- x[[by_field]]
    cmds <- list()

    if (!identical(transmitter, .noSelection)) {
        init_cmd <- sprintf("contents <- all_contents[['%s']]", transmitter)

        transmit_param <- all_memory[[transmitter]]
        cur_choice <- x[[type_field]]

        if (.multiSelectionHasActive(transmit_param) && 
            cur_choice %in% c(.selectMultiUnionTitle, .selectMultiActiveTitle)) 
        {
            select_sources <- NA_integer_
        } else {
            select_sources <- integer(0)
        }

        if (cur_choice == .selectMultiUnionTitle) {
            select_sources <- c(select_sources, seq_along(transmit_param[[.multiSelectHistory]]))
        } else if (cur_choice == .selectMultiSavedTitle) {
            select_sources <- x[[saved_field]]
            if (select_sources == 0L) {
                # '0' selection in memory means no selection.
                select_sources <- integer(0)
            }
        }

        for (i in select_sources) {
            cur_cmds <- .multiSelectionCommands(transmit_param, i)

            if (is.na(i)) {
                cur_cmds <- c(sprintf("select <- all_active[['%s']]", transmitter), cur_cmds)
            } else {
                cur_cmds <- c(sprintf("select <- all_saved[['%s']][[%i]]", transmitter, i), cur_cmds)
            }

            outname <- if (is.na(i)) "active" else paste0("saved", i)
            cmds[[outname]] <- c(cur_cmds, sprintf("%s[[%s]] <- selected;", varname, deparse(outname)))
        }

        if (length(cmds)) {
            cmds <- c(init=c(init_cmd, paste(varname, "<- list();")), cmds)
        }
    }
    unlist(cmds)
}

#' Populate selection structures
#'
#' Populate the environment with data structures required for multiple selection.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param envir An environment in which multiple selection commands are to be evaluated. 
#'
#' @details
#' This function provides a convenient wrapper to add active selection structures 
#' (e.g., Shiny brush and lasso objects) from any panel to the evaluation environment.
#' These are needed for certain commands to be executed (see the \dQuote{See also} section below).
#'
#' @return \code{all_active} and \code{all_saved} are added to \code{envir} using pass-by-reference behaviour of environments.
#' A\code{NULL} value is invisibly returned.
#'
#' @author Aaron Lun.
#' @rdname INTERNAL_populate_selection_environment
#' @seealso
#' \code{\link{.process_selectby_choice}}
.populate_selection_environment <- function(x, envir) {
    envir$all_active <- list(.multiSelectionActive(x))
    envir$all_saved <- list(x[[.multiSelectHistory]])
    names(envir$all_active) <- names(envir$all_saved) <- .getEncodedName(x)
    invisible(NULL)
}

.multiSelectionHasActive <- function(x) {
    !is.null(.multiSelectionActive(x))
}

#' Get all selection sources
#'
#' Find all acceptable sources of single or multiple selections in each dimension.
#'
#' @param all_memory A named list of \linkS4class{Panel}s representing the current state of the application.
#' @param all_names Character vector of encoded panel names.
#' @param multiple Logical vector indicating whether sources for multiple selections should be returned.
#' If \code{FALSE}, single selection sources are returned.
#'
#' @return A list containing \code{"row"} and \code{"column"}, 
#' character vectors containing the names of the acceptable panels that can transmit on the corresponding dimension.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_get_multi_sources
.get_selection_sources <- function(all_memory, all_names, multiple=TRUE) {
    if (multiple) {
        FUN <- .multiSelectionDimension
        choices <- c("row", "column")
    } else {
        FUN <- .singleSelectionDimension
        choices <- c("feature", "sample")
    }

    dims <- vapply(all_memory, FUN=FUN, "")
    list(
        row=c(.noSelection, all_names[dims==choices[1]]),
        column=c(.noSelection, all_names[dims==choices[2]])
    )
}
