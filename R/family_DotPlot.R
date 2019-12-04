#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "DotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    .Object <- .empty_default(.Object, .facetByRow, FALSE)
    .Object <- .empty_default(.Object, .facetByColumn, FALSE)

    .Object <- .empty_default(.Object, .colorByField, .colorByNothingTitle)
    .Object <- .empty_default(.Object, .colorByDefaultColor, "black")
    .Object <- .empty_default(.Object, .colorByFeatName)
    .Object <- .empty_default(.Object, .colorByRowTable, .noSelection)
    .Object <- .empty_default(.Object, .colorBySampName)
    .Object <- .empty_default(.Object, .colorByColTable, .noSelection)

    .Object <- .empty_default(.Object, .shapeByField, .shapeByNothingTitle)

    .Object <- .empty_default(.Object, .sizeByField, .sizeByNothingTitle)

    .Object <- .empty_default(.Object, .selectEffect, .selectTransTitle)
    .Object <- .empty_default(.Object, .selectColor, "red")
    .Object <- .empty_default(.Object, .selectTransAlpha, 0.1)

    .Object <- .empty_default(.Object, .dataParamBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .visualParamBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .visualParamChoice, .visualParamChoiceColorTitle)

    .Object <- .empty_default(.Object, .contourAddTitle, FALSE)
    .Object <- .empty_default(.Object, .contourColor, "blue")

    .Object <- .empty_default(.Object, .plotPointSize, 1)
    .Object <- .empty_default(.Object, .plotPointAlpha, 1)
    .Object <- .empty_default(.Object, .plotPointDownsample, FALSE)
    .Object <- .empty_default(.Object, .plotPointSampleRes, 200)

    .Object <- .empty_default(.Object, .plotFontSize, 1)
    .Object <- .empty_default(.Object, .plotLegendPosition, .plotLegendBottomTitle)

    .Object
})

#' @importFrom S4Vectors setValidity2
setValidity2("DotPlot", function(object) {
    msg <- character(0)

    msg <- .valid_logical_error(msg, object, 
        c(.facetByRow, .facetByColumn,
            .dataParamBoxOpen, .visualParamBoxOpen,
            .contourAddTitle))

    msg <- .single_string_error(msg, object,
        c(.colorByField, .colorByFeatName, .colorByRowTable, .colorBySampName, .colorByColTable,
            .shapeByField,
            .sizeByField,
            .selectEffect))

    msg <- .valid_string_error(msg, object, 
        c(.colorByDefaultColor, 
            .selectColor,
            .contourColor))

    msg <- .allowable_choice_error(msg, object, .selectEffect,
        c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle))

    msg <- .valid_number_error(msg, object, .selectTransAlpha, lower=0, upper=1)

    msg <- .multiple_choice_error(msg, object, .visualParamChoice,
        c(.visualParamChoiceColorTitle, .visualParamChoiceShapeTitle, .visualParamChoicePointTitle,
            .visualParamChoiceFacetTitle, .visualParamChoiceOtherTitle))

    msg <- .valid_number_error(msg, object, .plotPointSize, lower=0, upper=Inf)

    msg <- .valid_number_error(msg, object, .plotPointAlpha, lower=0, upper=1)

    msg <- .valid_number_error(msg, object, .plotPointSampleRes, lower=0, upper=Inf)

    msg <- .valid_number_error(msg, object, .plotFontSize, lower=0, upper=Inf)

    msg <- .allowable_choice_error(msg, object, .plotLegendPosition,
        c(.plotLegendRightTitle, .plotLegendBottomTitle))

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
setMethod(".createParamObservers", "DotPlot", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_box_observers(plot_name, c(.visualParamBoxOpen, .selectParamBoxOpen), input, pObjects)

    .define_visual_parameter_choice_observer(plot_name, input, pObjects)

    .define_protected_parameter_observers(plot_name,
        fields=c(.facetByRow, .facetByColumn, .facetRowsByRowData, .facetColumnsByRowData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_nonfundamental_parameter_observers(plot_name,
        fields=c(
            .colorByDefaultColor, .selectColor, .selectTransAlpha,
            .shapeByField, .sizeByField,
            .plotPointSize, .plotPointAlpha, .plotFontSize, .plotLegendPosition,
            .plotPointDownsample, .plotPointSampleRes, .contourAddTitle,
            .contourColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(plot_name,
        name_field=.colorByFeatName,
        choices=rownames(se),
        in_use_field=.colorByField,
        in_use_value=.colorByFeatNameTitle,
        table_field=.colorByRowTable,
        is_protected=FALSE,
        link_type="color",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(plot_name,
        name_field=.colorBySampName,
        choices=colnames(se),
        in_use_field=.colorByField,
        in_use_value=.colorBySampNameTitle,
        table_field=.colorByColTable,
        is_protected=FALSE,
        link_type="color",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_selectize_update_observer(plot_name, .colorByFeatName, 
        choices=rownames(se), selected=x[[.colorByFeatName]],
        session=session, rObjects=rObjects) 

    .define_selectize_update_observer(plot_name, .colorBySampName,
        choices=colnames(se), selected=x[[.colorBySampName]],
        session=session, rObjects=rObjects)

    .define_brush_observer(plot_name, input=input, session=session, 
        pObjects=pObjects, rObjects=rObjects)

    .define_lasso_observer(plot_name, input=input, session=session, 
        pObjects=pObjects, rObjects=rObjects)

    .define_multiselect_observers(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".defineOutputElement", "DotPlot", function(x, id) {
    mode <- .getEncodedName(x)
    .create_plot_ui(mode, x[[.organizationId]], brush_direction="xy",
        height=x[[.organizationHeight]],
        brush_fill=brush_fill_color[mode],
        brush_stroke=brush_stroke_color[mode]
    )
})

#' @export
setMethod(".createRenderedOutput", "DotPlot", function(x, se, colormap, output, pObjects, rObjects) {
# TODO: move colormap INSIDE se's metadata.
    .define_plot_output(.getEncodedName(x), x[[.organizationId]],
        FUN=.getPlottingFunction(x), selectable=TRUE,
        se=se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)
})
