#' @importFrom DT datatable renderDataTable selectRows dataTableProxy
.define_table_output <- function(panel_name, FUN, se, output, pObjects, rObjects) {
    force(FUN)
    force(se)

    output[[panel_name]] <- renderDataTable({
        force(rObjects[[panel_name]])
        param_choices <- pObjects$memory[[panel_name]]
        tab_cmds <- .initialize_cmd_store()
        eval_env <- new.env()

        # Defining the row and column selections, and hoping that the 
        # table-generating function in FUN knows what to do with this.
        row_select_cmds <- .process_selectby_choice(param_choices, 
            by_field=.selectRowSource, type_field=.selectRowType, saved_field=.selectRowSaved,
            all_memory=pObjects$memory, var_name="row_selected")

        if (!is.null(row_select_cmds)) {
            transmitter <- param_choices[[.selectRowSource]]
            .populate_selection_environment(pObjects$memory[[transmitter]], eval_env)
            eval_env$all_contents <- pObjects$contents
            tab_cmds <- .add_command(tab_cmds, row_select_cmds)
            tab_cmds <- .evaluate_commands(tab_cmds, eval_env)
        }

        col_select_cmds <- .process_selectby_choice(param_choices, 
            by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
            all_memory=pObjects$memory, var_name="col_selected")

        if (!is.null(col_select_cmds)) {
            transmitter <- param_choices[[.selectColSource]]
            .populate_selection_environment(pObjects$memory[[transmitter]], eval_env)
            eval_env$all_contents <- pObjects$contents
            tab_cmds <- .add_command(tab_cmds, col_select_cmds)
            tab_cmds <- .evaluate_commands(tab_cmds, eval_env)
        }

        # Creating the table and storing it.
        tab_cmds <- .add_command(tab_cmds, FUN(pObjects$memory[[panel_name]], se, eval_env))
        tab_cmds <- .evaluate_commands(tab_cmds, eval_env)

        full_tab <- eval_env$tab
        pObjects$contents[[panel_name]] <- full_tab
        pObjects$commands[[panel_name]] <- tab_cmds$processed

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
        selectRows(dataTableProxy(panel_name), NULL)

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

