#' Create Table output
#'
#' Create a reactive expression to render the Table output,
#' in a manner that satisfies all the requirements of the \code{\link{.renderOutput}} generic.
#' This function will call \code{\link{.generateOutput}} to get the heavy lifting done.
#'
#' @param panel_name String containing the name of the panel.
#' @param se A \linkS4class{SummarizedExperiment} object for the current dataset.
#' @param output The Shiny output object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' A reactive element to render the table is added to \code{output}.
#' A \code{NULL} is invisibly returned.
#' 
#' @author Aaron Lun
#'
#' @rdname INTERNAL_create_table_output
#' @importFrom DT datatable renderDataTable selectRows dataTableProxy
.create_table_output <- function(panel_name, se, output, pObjects, rObjects) {
    force(se)

    output[[panel_name]] <- renderDataTable({
        force(rObjects[[panel_name]])
        param_choices <- pObjects$memory[[panel_name]]
       
        t.out <- .generateOutput(param_choices, se=se, all_memory=pObjects$memory, all_contents=pObjects$contents)
        full_tab <- t.out$contents
        pObjects$contents[[panel_name]] <- full_tab
        pObjects$commands[[panel_name]] <- t.out$commands
        pObjects$varname[[panel_name]] <- "tab"

        chosen <- param_choices[[.TableSelected]]
        search <- param_choices[[.TableSearch]]
        search_col <- param_choices[[.TableColSearch]]
        search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

        # If the existing row in memory doesn't exist in the current table, we
        # don't initialize it with any selection.
        idx <- which(rownames(full_tab)==chosen)[1]
        if (!is.na(idx)) {
            selection <- list(mode="single", selected=idx)
        } else {
            selection <- "single"
        }

        # Clearing the current row selection in 'input', otherwise some madness
        # happens with the observer seeming to respond to the datatable()
        # re-rendering but applying the old value of 'input[[*_rows_selected]]'
        # to the new 'full_tab' - not good.
        selectRows(dataTableProxy(panel_name, deferUntilFlush=FALSE), NULL)

        datatable(
            full_tab, filter="top", rownames=TRUE,
            options=list(
                search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), search_col), # row names are the first column!
                scrollX=TRUE),
            selection=selection
        )
    })
}

#' Commands to define the Table
#'
#' Define commands to generate the contents of the \linkS4class{Table}.
#' This uses \code{\link{.getTableCommands}} and is itself called inside \code{\link{.generateOutput}}.
#' 
#' @param x An instance of a \linkS4class{Table} class.
#' @param se A \linkS4class{SummarizedExperiment} object for the current dataset.
#' @param all_memory A list of \linkS4class{Panel} instances representing the current state of the application.
#' @param all_contents A list of displayed contents for each panel in the app, see \code{\link{.renderOutput}} for details.
#'
#' @return
#' A list containing \code{\link{commands}}, the commands required to produce the data.frame;
#' and \code{contents}, a data.frame of the current contents of the Table.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_table_commands
.define_table_commands <- function(x, se, all_memory, all_contents) {
    tab_cmds <- .initialize_cmd_store()
    eval_env <- new.env()
    eval_env$se <- se

    # Doing this first so that .getTableCommands can respond to the selection.
    select_cmds <- .processMultiSelections(x, all_memory, all_contents, eval_env)

    # Creating the table and storing it.
    tab_cmds <- .add_command(tab_cmds, .getTableCommands(x, eval_env))
    tab_cmds <- .evaluate_commands(tab_cmds, eval_env)

    list(commands=tab_cmds, contents=eval_env$tab)
}
