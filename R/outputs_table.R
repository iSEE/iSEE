#' @importFrom DT datatable renderDataTable
.define_table_output <- function(mode, id, tab, select_col, output, pObjects, rObjects) {
    panel_name <- paste0(mode, id)
    output[[panel_name]] <- renderDataTable({
        force(rObjects$active_panels) # to trigger recreation when the number of plots is changed.
        force(rObjects[[panel_name]])

        param_choices <- pObjects$memory[[mode]][id,]
        chosen <- param_choices[[.statTableSelected]]
        search <- param_choices[[.statTableSearch]]
        search_col <- param_choices[[.statTableColSearch]][[1]]
        search_col <- lapply(search_col, FUN=function(x) { list(search=x) })

        # After the first initialization there may not be any need to add columns
        # Fact is, the extra "Selected" column may make the table it has more columns than filters
        missing_columns <- max(0, ncol(tab) - length(search_col))
        search_col <- c(search_col, rep(list(list(search="")), missing_columns)) # TODO: fix for internal fields.

        # Adding a "Selected" field to the plotting data, which responds to point selection input.
        # Note that this AUTOMATICALLY updates search_col upon re-rendering via the observer below.
        # The code below keeps search_col valid for the number of columns (i.e., with or without selection).
        selected <- .get_selected_points(rownames(tab),
            param_choices[[.selectByPlot]], pObjects$memory, pObjects$coordinates,
            select_type=param_choices[[.selectMultiType]], select_saved=param_choices[[.selectMultiSaved]])

        tmp_df <- tab
        columnDefs <- list()
        if (!is.null(selected)) {
            tmp_df[[select_col0]] <- selected
            if (length(search_col)!=ncol(tmp_df)) {
                # brackets appears to fix row indexing in RStudio browser (1/2)
                search_col <- c(search_col, list(list(search="[\"true\"]")))
            } else {
                # brackets appears to fix row indexing in RStudio browser (2/2)
                search_col[[ncol(tmp_df)]]$search <- "[\"true\"]"
            }
            # this line must stay below the if block
            columnDefs <- append(columnDefs, list(list(visible=FALSE, targets=length(search_col))))
        } else {
            search_col <- search_col[seq_len(ncol(tmp_df))]
        }

        datatable(
            tmp_df, filter="top", rownames=TRUE,
            options=list(
                search=list(search=search, smart=FALSE, regex=TRUE, caseInsensitive=FALSE),
                searchCols=c(list(NULL), search_col), # row names are the first column!
                columnDefs=columnDefs,
                scrollX=TRUE),
            selection=list(mode="single", selected=chosen)
        )
    })
}
