#' Checks if there is a relevant selection
#'
#' Checks whether there is a multiple selection from a transmitter, 
#' either active or in the saved history.
#'
#' @param parent_name String containing the name of the transmitting panel.
#' @param all_memory A named list of \linkS4class{Panel}s representing the current state of the application.
#'
#' @return A logical scalar specifying whether there is a relevant selection.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_transmitted_selection
.transmitted_selection <- function(parent_name, all_memory) {
    if (parent_name==.noSelection) {
        FALSE
    } else {
        transmitter <- all_memory[[parent_name]]
        .multiSelectionHasActive(transmitter) || .any_saved_selection(transmitter)
    }
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
    row_select_cmds <- .process_selectby_choice(x, by_field=.selectRowSource, 
        all_memory=all_memory, varname="row_selected")

    if (!is.null(row_select_cmds)) {
        transmitter <- x[[.selectRowSource]]
        .populate_selection_environment(all_memory[[transmitter]], envir)
        envir$all_contents <- all_contents
        .textEval(row_select_cmds, envir)
        select_cmds[["row"]] <- row_select_cmds
    }

    col_select_cmds <- .process_selectby_choice(x, by_field=.selectColumnSource, 
        all_memory=all_memory, varname="col_selected")

    if (!is.null(col_select_cmds)) {
        transmitter <- x[[.selectColumnSource]]
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
#' @param all_memory A list of \linkS4class{Panel} objects that represents the current state of the application.
#' @param varname String containing the name of the variable that contains all selections.
#'
#' @return A character vector of commands that, when executed, results in a list named \code{varname}.
#' Each entry of the list is a character vector that contains the identities of points in the active and/or saved selections.
#' List names are set to \code{"active"}, \code{"saved1"}, \code{"saved2"}, etc.
#' If no selection should be applied, \code{NULL} is returned instead.
#'
#' @details
#' For the current panel, this function identifies the transmitting panel, i.e., from which the current panel is receiving a selection of data points.
#' It then generates the commands necessary to identify the points selected in the transmitter, to add as \code{SelectBy} in the current panel.
#'
#' Evaluation of the output commands require:
#' \itemize{
#' \item a list object called \code{all_contents} where each entry is named by the panel name.
#' The entry corresponding to the transmitting plot should contain some values corresponding to the displayed data, as described in \code{?\link{.renderOutput}}.
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
.process_selectby_choice <- function(x, by_field, all_memory, varname="selected_pts") {
    transmitter <- x[[by_field]]
    cmds <- list()

    if (!identical(transmitter, .noSelection)) {
        init_cmd <- sprintf("contents <- all_contents[['%s']];", transmitter)
        transmit_param <- all_memory[[transmitter]]

        if (.multiSelectionHasActive(transmit_param)) {
            cmds$active <- c(
                sprintf("select <- all_active[['%s']];", transmitter), 
                .multiSelectionCommands(transmit_param, NA_integer_),
                sprintf("%s[[\"active\"]] <- selected;", varname)
            )
        }

        for (i in seq_along(transmit_param[[.multiSelectHistory]])) {
            outname <- paste0("saved", i)
            cmds[[outname]] <- c(
                sprintf("select <- all_saved[['%s']][[%i]];", transmitter, i),
                .multiSelectionCommands(transmit_param, i),
                sprintf("%s[[%s]] <- selected;", varname, deparse(outname))
            )
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
#' @return A list of two character vectors containing the names of the acceptable panels that can transmit on the corresponding dimension.
#' If \code{multiple=TRUE}, these vectors are named \code{"row"} and \code{"column"};
#' otherwise they are named \code{"feature"} and \code{"sample"}.
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
    output <- vector("list", length(choices))
    names(output) <- choices
    for (i in choices) {
        output[[i]] <- c(.noSelection, all_names[dims==i])
    }

    output
}
