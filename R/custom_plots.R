#' Make a custom column plot
#'
#' Make a custom plot of column (i.e., sample) data, using a user-specified function to generate coordinates.
#'
#' @param id Integer scalar specifying the index of the current custom column plot.
#' @param all_memory list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param all_coordinates A list of data.frames that contain the coordinates and covariates of data points visible in each of the plots.
#' @param se A SingleCellExperiment object.
#' @param select_all Logical scalar specifying whether both active and saved selections should be passed to the custom plotting function. 
#'
#' @return A list containing \code{cmd_list}, \code{xy} and \code{plot}, equivalent to the output of \code{\link{.plot_wrapper}}.
#' A \code{cached} field is also returned containing the result of evaluating the user-specified function.
#' 
#' @details
#' This function will try to use the input \code{cached} coordinates if possible.
#' In particular, it will only regenerate the coordinates if a user transmits a restricting selection to the custom column plot,
#' and the selection differs from what is in the input \code{cached} (based on differences in the names of the selected samples).
#'
#' For the output \code{cached}, the idea is that it will be stored in the main \code{\link{iSEE}} function.
#' It can then be passed to this function upon future re-plotting of the custom column plot.
#'
#' If the selected custom column function is \code{"---"}, this function will return \code{NULL} in \code{cmd_list};
#' a data.frame with no rows in \code{xy}; an empty ggplot in \code{plot}; and \code{NULL} in \code{cached}.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_make_customDataPlot
#' @seealso
#' \code{\link{.process_custom_selections}},
#' \code{\link{.text2args}}
#' 
.make_customDataPlot <- function(id, all_memory, all_coordinates, se, select_all=FALSE) {
    param_choices <- all_memory$customDataPlot[id,]
    eval_env <- new.env()
    eval_env$se <- se
    eval_env$all_coordinates <- all_coordinates
    eval_env$custom_data_fun <- .get_internal_info(se, "custom_data_fun")

    # Getting the subselections. 
    select_out <- .process_custom_selections(param_choices, all_memory, select_all=select_all)

    brushes <- lassos <- histories <- list()
    temp_env <- new.env()
    for (i in seq_along(select_out$transmitter)) {
        current <- select_out$transmitter[[i]]
        transmit_param <- all_memory[[current$Type]][current$ID,]
        .populate_selection_environment(transmit_param, temp_env)
        brushes <- c(brushes, temp_env$all_brushes) # Bit of work here to merge the row/col selection objects.
        lassos <- c(lassos, temp_env$all_lassos)
        histories <- c(histories, temp_env$all_select_histories)
    }

    eval_env$all_brushes <- brushes
    eval_env$all_lassos <- lassos
    eval_env$all_select_histories <- histories
    .text_eval(select_out$cmds, eval_env)
 
    # Constructing the evaluation command to get the plot. 
    fun_args <- .text2args(param_choices[[.customArgs]])
    if (length(fun_args)) {
        as_cmd_args <- paste(sprintf("%s=%s", names(fun_args), vapply(fun_args, deparse, FUN.VALUE=character(1))), collapse=", ")
        as_cmd_args <- paste0(", ", as_cmd_args)
    } else {
        as_cmd_args <- ""
    }

    fun_name <- param_choices[[.customFun]]
    if (fun_name!=.noSelection) {
        custom_cmd <- sprintf("custom_data_fun[[%s]](se, row.names, col.names%s);", deparse(fun_name), as_cmd_args)
    } else {
        custom_cmd <- "ggplot()"
    }
    plot_out <- .text_eval(custom_cmd, eval_env)
    list(cmd_list = list(select=select_out$cmds, plot=custom_cmd), plot=plot_out)
}

#' Process selections for a custom data plot
#'
#' Process the row and column-level selections for a custom data plot.
#'
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param all_memory list of DataFrames, where each DataFrame corresponds to a panel type and contains the settings for each individual panel of that type.
#' @param select_all Logical scalar specifying whether both active and saved selections should be reported.
#'
#' @return A list that includes the following elements:
#' \describe{
#' \item{cmds}{A character vector of commands that generates \code{row.names} and \code{col.names} variables.
#' Either can be \code{NULL} if \code{select_all=FALSE} and no selection should be applied in that dimension.
#' }
#' \item{transmitters}{A list containing lists with encoded panel types and IDs for the row and column transmitting panels.
#' If the current plot does not specify a transmitter for rows/columns, this is left as \code{NULL}.
#' }
#' }
#'
#' @details
#' This function is very similar to \code{\link{.process_selectby_choice}}, but contains several modifications specific for custom data plots.
#' Specifically, we need to perform selections for both column and rows based on separate fields in the \code{param_choices} (i.e., not just \code{"SelectBy"}).
#' There is also no need to consider the selection effect, as this is not relevant for custom data plots.
#'
#' If \code{select_all=FALSE}, the output commands should generate either \code{NULL} or a character vector of names.
#' If \code{select_all=TRUE}, the output commands should generate a list with the \code{active} vector of names and a \code{saved} list of name vectors.
#' In short, this function should mimic the output of \code{\link{.get_selected_points}}.
#' 
#' @author Aaron Lun
#' @rdname INTERNAL_process_custom_selections
#' @seealso
#' \code{\link{.process_selectby_choice}}
#'
#' @importFrom shiny brushedPoints
.process_custom_selections <- function(param_choices, all_memory, select_all=FALSE) {
    commands <- list()
    transmitters <- list()

	for (dim in c("row", "col")){ 
        if (dim=="col") {
            select_in <- param_choices[[.customColSource]]
        } else {
            select_in <- param_choices[[.customRowSource]]
        }
        if (identical(select_in, .noSelection)) {
            commands[[dim]] <- if (select_all) "list(active=NULL, saved=list())" else "NULL"
            next
        }

        cmds <- NULL
        select_by <- .encode_panel_name(select_in)
        transmitter <- paste0(select_by$Type, select_by$ID)
        source_data <- sprintf("all_coordinates[['%s']]", transmitter)
        transmitters[[dim]] <- select_by

        # Getting the active selections.
        brush_val <- all_memory[[select_by$Type]][,.brushData][[select_by$ID]]
        if (!is.null(brush_val)) {
            cmds <- sprintf("shiny::brushedPoints(%s, all_brushes[['%s']])", source_data, transmitter)
            
        } else {
            lasso_val <- all_memory[[select_by$Type]][,.lassoData][[select_by$ID]]
            if (!is.null(lasso_val) && lasso_val$closed) { 
                cmds <- sprintf("lassoPoints(%s, all_lassos[['%s']])", source_data, transmitter)
            }
        }

        if (!is.null(cmds)) {
            cmds <- sprintf("rownames(%s)", cmds)
        } else {
            cmds <- "NULL"
        }
            
        if (!select_all) {
            commands[[dim]] <- cmds
            next
        }

        allsaved <- all_memory[[select_by$Type]][,.multiSelectHistory][[select_by$ID]]
        saved_cmds <- character(length(allsaved))
        for (i in seq_along(allsaved)) {
            cursaved <- allsaved[[i]]
            if (.is_lasso(cursaved)) {
                fun <- "lassoPoints"
            } else {
                fun <- "shiny::brushedPoints"
            }
            transmit_src <- sprintf("all_select_histories[['%s']][[%i]]", transmitter, i)
            saved_cmds[i] <- sprintf("rownames(%s(%s, %s))", fun, source_data, transmit_src)
        }

        commands[[dim]] <- sprintf("list(active=%s,
    saved=list(%s))", cmds, paste(saved_cmds, collapse=",\n        "))
    }

    list(cmds=paste0(names(commands), ".names <- ", unlist(commands), ";"), transmitters=transmitters) 
}

#' Convert multi-line text to arguments
#'
#' Convert multi-line text input from the app into a series of named arguments for a custom function.
#'
#' @param fun_args A multi-line string containing multiple named arguments and their values.
#'
#' @details
#' Each line corresponds to one argument:value pair, stripped of any leading spaces.
#' The word before the first remaining space defines the argument name, while all characters after the first space define the value.
#' Note that all values are strings for purposes of safety, so any type coercion is the responsibility of the function.
#'
#' @return
#' A named character vector of values, where the names are the arguments.
#' 
#' @rdname INTERNAL_text2args
#' @author Aaron Lun
#' @seealso
#' \code{\link{.make_customDataPlot}}
.text2args <- function(fun_args) {
    fun_args <- strsplit(fun_args, "\n")[[1]]
    fun_args <- sub("^ +", "", fun_args)
    arg_names <- sub(" .*", "", fun_args)
    arg_values <- sub("^ +", "", sub("^[^ ]+", "", fun_args)) # double trim to avoid failure when no spaces are supplied.

    keep <- arg_names!=""
    arg_names <- arg_names[keep]
    arg_values <- arg_values[keep]
    names(arg_values) <- arg_names
    return(arg_values)
}
