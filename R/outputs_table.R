#' @importFrom DT datatable renderDataTable
.define_table_output <- function(mode, id, FUN, se, output, pObjects, rObjects) {
    panel_name <- paste0(mode, id)

    output[[panel_name]] <- renderDataTable({
        force(rObjects$rerendered) # to trigger recreation when the number of plots is changed.
        force(rObjects[[panel_name]])

        param_choices <- pObjects$memory[[panel_name]]
        chosen <- param_choices[[.statTableSelected]]
        search <- param_choices[[.statTableSearch]]
        search_col <- param_choices[[.statTableColSearch]]
        search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

        # Constructing commands to generate the final table.
        # TODO: flip this around so table creation can be aware of the subsetting.
        tab_cmds <- .initialize_cmd_store()
        tab_cmds <- .add_command(tab_cmds, FUN(pObjects$memory[[panel_name]], se))

        # We record the filtered table but we implicitly show the full table,
        # using some DataTable trickery. This is necessary to avoid
        # invalidating table links (e.g., to feature assay plots) when the
        # current selection is not in the subset.
        eval_env <- new.env()
        tab_cmds <- .evaluate_commands(tab_cmds, eval_env)
        full_tab <- eval_env$tab

        select_cmds <- .process_selectby_choice(param_choices, pObjects$memory)
        if (!is.null(select_cmds)) {
            eval_env <- new.env()
            tab_cmds <- .add_command(tab_cmds, "plot.data <- data.frame(row.names=rownames(tab));")
            tab_cmds <- .evaluate_commands(tab_cmds, eval_env)

            tab_cmds <- .add_command(tab_cmds, select_cmds)
            transmitter <- param_choices[[.selectByPlot]]
            .populate_selection_environment(pObjects$memory[[transmitter]], eval_env)
            tab_cmds <- .evaluate_commands(tab_cmds, eval_env)

            tab_cmds <- .add_command(tab_cmd, "tab <- tab[dummy$SelectBy,];") 
            tab_cmds <- .evaluate_commands(tab_cmds, eval_env)
        }

        pObjects$coordinates[[panel_name]] <- eval_env$tab
        pObjects$commands[[panel_name]] <- tab_cmds$processed

        # We need to account for the fact that we are silently adding an extra column to this table.
        columnDefs <- list()
        if (!is.null(select_cmds)) {
            full_tab[[.tableSecretColumnTitle]] <- eval_env$dummy$SelectBy

            # brackets appears to fix row indexing in RStudio browser
            search_col <- c(search_col, list(list(search="[\"true\"]")))
            columnDefs <- list(list(visible=FALSE, targets=length(search_col)))
        }

        datatable(
            full_tab, filter="top", rownames=TRUE,
            options=list(
                search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), search_col), # row names are the first column!
                columnDefs=columnDefs,
                scrollX=TRUE),
            selection=list(mode="single", selected=chosen)
        )
    })
}
