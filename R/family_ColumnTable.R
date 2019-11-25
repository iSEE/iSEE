#' @export
setMethod(".defineParamInterface", "ColumnTable", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    link_sources <- .define_link_sources(active_panels)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    .define_selection_param_box(mode, id, param_choices,
       .define_selection_choices(mode, id, param_choices, .selectByPlot, col_selectable, "column")
    )
})

#' @export
setMethod(".createParamObservers", "ColumnTable", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)

    sample_choices <- seq_len(nrow(se))
    names(sample_choices) <- rownames(se)

    .define_table_selection_observer(mode, id,
        x_field=.sampAssayXAxisSampName,
        y_field=.sampAssayYAxisSampName,
        col_field=.colorBySampName,
        choices=sample_choices,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

