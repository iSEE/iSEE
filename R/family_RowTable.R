#' @export
#' @importFrom SummarizedExperiment rowData
setMethod(".refineParameters", "RowTable", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replace_na_with_first(x, .TableSelected, rownames(se))

    x
})

#' @export
setMethod(".defineParamInterface", "RowTable", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    link_sources <- .define_link_sources(active_panels)
    row_selectable <- c(.noSelection, link_sources$row_plot, link_sources$row_tab)

    .define_selection_param_box(mode, id, x,
       .define_selection_choices(mode, id, x, .selectByPlot, row_selectable, "row")
    )
})

#' @export
setMethod(".createParamObservers", "RowTable", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    panel_name <- paste0(.getEncodedName(x), x[[.organizationId]])
    .define_table_selection_observer(panel_name, choices=rownames(se), input=input, 
        session=session, pObjects=pObjects, rObjects=rObjects)
})
