#' @export
setMethod(".defineParamInterface", "RowTable", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    link_sources <- .define_link_sources(active_panels)
    row_selectable <- c(.noSelection, link_sources$row_plot)

    .define_selection_param_box(mode, id, param_choices,
       .define_selection_choices(mode, id, param_choices, .selectByPlot, row_selectable, "row")
    )
})

#' @export
setMethod(".createParamObservers", "RowTable", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    .define_table_selection_observer(mode, id, 
        x_field=.featAssayXAxisFeatName, 
        y_field=.featAssayYAxisFeatName,
        col_field=.colorByFeatName,
        choices=feature_choices,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})
