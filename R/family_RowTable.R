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
