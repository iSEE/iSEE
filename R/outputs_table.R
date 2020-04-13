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
#' @importFrom utils head
.create_table_output <- function(panel_name, se, output, pObjects, rObjects) {
    force(se)

    # nocov start
    output[[panel_name]] <- renderDataTable({
        .trackUpdate(panel_name, rObjects)
        param_choices <- pObjects$memory[[panel_name]]

        # This is a rather odd one. It is necessary because the DT doesn't
        # conventionally re-render when the API regenerates; rather, it seems
        # somehow to load the cached version of the initialized table, thus
        # wiping out any changes that have happened in the meantime. So,
        # we force the DT to rerender so that it loads with memorized values.
        force(rObjects$rerendered)

        t.out <- .retrieveOutput(panel_name, se, pObjects, rObjects)
        full_tab <- t.out$contents
        pObjects$varname[[panel_name]] <- "tab"

        chosen <- param_choices[[.TableSelected]]
        search <- param_choices[[.TableSearch]]
        search_col <- param_choices[[.TableColSearch]]

        # Protection against a change in the number of columns from .generateOutput.
        # .generate_table_filter protects against a mismatch in use by children,
        #  so there's no need to edit the memory here (and in fact that won't work
        # anyway because the children are evaluating way before we get here).
        delta <- ncol(full_tab) - length(search_col)
        if (delta!=0L) {
            if (delta < 0L) {
                search_col <- head(search_col, ncol(full_tab))
            } else {
                search_col <- c(search_col, character(delta))
            }
        }
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

        # Dummying up a variable and hiding it to force datatable to show something.
        # when there are no columns.
        if (ncol(full_tab)==0L) {
            full_tab$DUMMY <- integer(nrow(full_tab))
            columnDefs <- list(list(targets=1L, visible=FALSE))
        } else {
            columnDefs <- NULL
        }

        datatable(
            full_tab, filter="top", rownames=TRUE,
            options=list(
                search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), search_col), # row names are the first column!
                scrollX=TRUE,
                columnDefs=columnDefs),
            selection=selection
        )
    })
    # nocov end

    invisible(NULL)
}

#' Generate the Table
#'
#' Define commands to generate the contents of the \linkS4class{Table}.
#' This uses \code{\link{.generateTable}} and is itself called inside \code{\link{.generateOutput}}.
#'
#' @param x An instance of a \linkS4class{Table} class.
#' @param se A \linkS4class{SummarizedExperiment} object for the current dataset.
#' @param all_memory A list of \linkS4class{Panel} instances representing the current state of the application.
#' @param all_contents A list of displayed contents for each panel in the app, see \code{\link{.renderOutput}} for details.
#'
#' @return
#' A list containing \code{commands}, a list of the commands required to produce the data.frame;
#' and \code{contents}, a data.frame of the current contents of the Table.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_table_commands
.define_table_commands <- function(x, se, all_memory, all_contents) {
    eval_env <- new.env()
    eval_env$se <- se

    # Doing this first so that .generateTable can respond to the selection.
    select_cmds <- .processMultiSelections(x, all_memory, all_contents, eval_env)

    # Creating the table and storing it.
    tab_cmds <- .generateTable(x, eval_env)

    list(commands=list(select_cmds, tab_cmds), contents=eval_env$tab)
}
