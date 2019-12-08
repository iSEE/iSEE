#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".refineParameters", "ColumnTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replace_na_with_first(x, .TableSelected, colnames(se))

    x
})

#' @export
setMethod(".defineParamInterface", "ColumnTable", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    link_sources <- .define_link_sources(active_panels)
    col_selectable <- c(.noSelection, link_sources$col_plot, link_sources$col_tab)

    .define_selection_param_box(mode, id, x,
       .define_selection_choices(mode, id, x, .selectByPlot, col_selectable, "column")
    )
})

#' @export
setMethod(".createParamObservers", "ColumnTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- paste0(.getEncodedName(x), x[[.organizationId]])
    .define_table_selection_observer(panel_name, choices=colnames(se), input=input,
        session=session, pObjects=pObjects, rObjects=rObjects)
})
