#' @export
setMethod("initialize", "DotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .facetByRow, FALSE)
    .Object <- .empty_default(.Object, .facetByColumn, FALSE)
    .Object
})

#' @export
#' @importFrom S4Vectors isSingleString setValidity2
setValidity2("DotPlot", function(object) {
    msg <- character(0)

    for (field in c(.facetByRow, .facetByColumn)) {
        if (length(val <- object[[field]]) || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA logical scalar for '%s'", field, class(object)[1]))
        }
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

setMethod(".cacheCommonInfo", "DotPlot", function(x, se) {
    if (is.null(.get_common_info(se, "DotPlot"))) {
        named_assays <- assayNames(se)
        named_assays <- named_assays[named_assays!=""]
        se <- .set_common_info(se, "DotPlot",
            valid.assay.names=named_assays)
    }

    callNextMethod()
})

#' @export
setMethod(".createParamObservers", "DotPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    .define_box_observers(mode, id, c(.visualParamBoxOpen, .selectParamBoxOpen), input, pObjects)

    .define_visual_parameter_choice_observer(mode, id, input, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=c(.facetByRow, .facetByColumn, .facetRowsByRowData, .facetColumnsByRowData),
        nonfundamental=c(
            .colorByDefaultColor, .selectColor, .selectTransAlpha,
            .shapeByField, .sizeByField,
            .plotPointSize, .plotPointAlpha, .plotFontSize, .plotLegendPosition,
            .plotPointDownsample, .plotPointSampleRes, .contourAddTitle,
            .contourColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects) 

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)
    sample_choices <- seq_len(ncol(se))
    names(sample_choices) <- colnames(se)

    .define_dim_name_observer(mode, id,
        name_field=.colorByFeatName,
        choices=feature_choices,
        in_use_field=.colorByField,
        in_use_value=.colorByFeatNameTitle,
        table_field=.colorByRowTable,
        is_protected=FALSE,
        link_type="color",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(mode, id,
        name_field=.colorBySampName,
        choices=sample_choices,
        in_use_field=.colorByField,
        in_use_value=.colorBySampNameTitle,
        table_field=.colorByColTable,
        is_protected=FALSE,
        link_type="color",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})


#' @export
setMethod(".defineOutputElement", "DotPlot", function(x, id, height) {
    mode <- .getEncodedName(x)
    .create_plot_ui(mode, id, brush_direction="xy", 
        height=height,
        brush_fill=brush_fill_color[mode],
        brush_stroke=brush_stroke_color[mode]
    )
})

#' @export
setMethod(".createRenderedOutput", "DotPlot", function(x, id, se, colormap, output, pObjects, rObjects) {
# TODO: move colormap INSIDE se's metadata.
    .define_plot_output(.getEncodedName(x), id, FUN=.getPlottingFunction(x), selectable=TRUE, 
        se=se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)
})
