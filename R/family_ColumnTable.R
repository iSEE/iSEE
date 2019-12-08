#' @export
setMethod(".defineParamInterface", "ColumnTable", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    link_sources <- .define_link_sources(active_panels)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    .define_selection_param_box(mode, id, x,
       .define_selection_choices(mode, id, x, .selectByPlot, col_selectable, "column")
    )
})
