#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "DotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    .Object <- .empty_default(.Object, .facetByRow, FALSE)
    .Object <- .empty_default(.Object, .facetByColumn, FALSE)

    .Object <- .empty_default(.Object, .colorByField, .colorByNothingTitle)
    .Object <- .empty_default(.Object, .colorByDefaultColor, "black")
    .Object <- .empty_default(.Object, .colorByFeatName, NA)
    .Object <- .empty_default(.Object, .colorByRowTable, .noSelection)
    .Object <- .empty_default(.Object, .colorBySampName, NA)
    .Object <- .empty_default(.Object, .colorByColTable, .noSelection)

    .Object <- .empty_default(.Object, .shapeByField, .shapeByNothingTitle)

    .Object <- .empty_default(.Object, .sizeByField, .sizeByNothingTitle)

    .Object <- .empty_default(.Object, .selectEffect, .selectTransTitle)
    .Object <- .empty_default(.Object, .selectColor, "red")
    .Object <- .empty_default(.Object, .selectTransAlpha, 0.1)

    .Object <- .empty_default(.Object, .selectMultiType, .selectMultiActiveTitle)
    .Object <- .empty_default(.Object, .selectMultiSaved, 0L)

    .Object
})

#' @importFrom S4Vectors setValidity2
setValidity2("DotPlot", function(object) {
    msg <- character(0)

    msg <- .valid_logical_error(msg, object, c(.facetByRow, .facetByColumn))

    msg <- .single_string_error(msg, object,
        c(.colorByField, .colorByFeatName, .colorByRowTable, .colorBySampName, .colorByColTable,
            .shapeByField,
            .sizeByField,
            .selectEffect))

    msg <- .valid_string_error(msg, object, c(.colorByDefaultColor, .selectColor))

    msg <- .allowable_choice_error(msg, object, .selectEffect,
        c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle))

    msg <- .transparency_error(msg, object, .selectByTransAlpha)

    msg <- .allowable_choice_error(msg, object, .selectMultiType,
        c(.selectMultiActiveTitle, .selectMultiUnionTitle, .selectMultiSavedTitle))

    if (length(saved <- object[[.selectMultiSaved]]) > 1L || saved < 0L) {
        msg <- c(msg, sprintf("'%s' must be a non-negative integer in '%s'", .selectMultiSaved, class(object)[1]))
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom methods callNextMethod
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
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "DotPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replace_na_with_first(x, .colorByFeatName, rownames(se))
    x <- .replace_na_with_first(x, .colorBySampName, colnames(se))

    x
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
