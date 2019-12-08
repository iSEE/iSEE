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
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .safe_reactive_init(rObjects, paste0(plot_name, "_", .panelGeneralInfo))

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

    .define_selection_effect_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .define_saved_selection_observers(plot_name, input=input, session=session,
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
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]

    out <- .getCodeChunk(x, pObjects$memory, pObjects$coordinates, se, colormap)
    lapply(out, function(x){invisible(cat(paste0(x, collapse = "\n"), sep = "\n"))})

    .define_plot_output(mode, id,
        FUN=.getPlottingFunction(x), selectable=TRUE,
        se=se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)

    .define_selection_info_output(mode, id,
        output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".getCodeChunk", "DotPlot", function(x, all_memory, all_coordinates, se, colormap) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)
    param_choices <- all_memory[[plot_name]]
    is_row_plot <- is(x, "RowDotPlot")
    # Apply the function provided to generate XY commands and axis labels
    out_xy <- .getCommandsDataXY(x, param_choices)
    # Initialize an environment storing information for generating ggplot commands
    plot_env <- new.env()
    plot_env$se <- se

    # Process the XY commands
    data_cmds_store <- .initialize_cmd_store()
    data_cmds_store <- .add_command(data_cmds_store, out_xy$data_cmds)
    data_cmds_store <- .evaluate_commands(data_cmds_store, plot_env)
    plot_env$labs <- c(x=out_xy$x_lab, y=out_xy$y_lab, title=out_xy$plot_title)

    # Add commands coercing X and Y to appropriate type
    data_cmds_store <- .add_commands_coerce(plot_env, data_cmds_store, c("X", "Y"))

    # Add commands adding optional columns to plot.data
    out <- .getCommandsDataColor(x, param_choices, se)
    data_cmds_store <- .add_command(data_cmds_store, out$cmds, name='color')
    plot_env$labs <- c(plot_env$labs, color = out$label)

    out <- .getCommandsDataShape(x, param_choices, se)
    data_cmds_store <- .add_command(data_cmds_store, out$cmds, name='shape')
    plot_env$labs <- c(plot_env$labs, shape = out$label)

    out <- .getCommandsDataSize(x, param_choices, se)
    data_cmds_store <- .add_command(data_cmds_store, out$cmds, name='size')
    plot_env$labs <- c(plot_env$labs, size = out$label)

    out_facets <- .getCommandsDataFacets(x, param_choices, se)
    data_cmds_store <- .add_command(data_cmds_store, out_facets)

    # Add commands coercing ColorBy to appropriate type, if present
    data_cmds_store <- .evaluate_commands(data_cmds_store, plot_env)
    color_data <- plot_env$plot.data$ColorBy
    if (!is.null(color_data)) {
        data_cmds_store <- .add_commands_coerce(plot_env, data_cmds_store, c("ColorBy"))
    }

    # Removing NAs in axes aesthetics as they mess up .process_selectby_choice.
    clean_select_fields <- c("X", "Y", names(out_facets))
    clean_expression <- paste(sprintf("!is.na(%s)", clean_select_fields), collapse=" & ")
    data_cmds_store <- .add_command(data_cmds_store, sprintf("plot.data <- subset(plot.data, %s);", clean_expression), name='na.rm')

    # Add commands adding the optional SelectBy column to plot.data
    data_cmds_store <- .evaluate_commands(data_cmds_store, plot_env)
    select_out <- .process_selectby_choice(param_choices, all_memory)
    select_cmds <- select_out$cmds
    if (!is.null(select_cmds)) {
        .populate_selection_environment(all_memory[[select_out$transmitter$Type]][select_out$transmitter$ID,], plot_env)
        data_cmds_store <- .add_command(data_cmds_store, select_cmds, clean_expression)
        data_cmds_store <- .evaluate_commands(data_cmds_store, plot_env)
    }

    # Define the type of plot to create, and add geometry-specific commands, if needed
    specific <- .choose_plot_type(plot_env)
    data_cmds_store <- .add_command(data_cmds_store, specific, clean_expression)
    data_cmds_store <- .evaluate_commands(data_cmds_store, plot_env)

    # TODO: don't forget to define the plot type based on XY and add extra commands

    # TODO: streamline the workflow below (previously .plot_wrapper)
    setup_out <- .extract_plotting_data(out_xy$data_cmds, param_choices, all_memory, all_coordinates, se, by_row=is_row_plot)
    downsample_cmds <- .downsample_points(param_choices, setup_out$envir)
    plot_out <- .create_plot(setup_out$envir, param_choices, colormap=colormap,
        x_lab=out_xy$x_lab, y_lab=out_xy$y_lab, title=out_xy$plot_title, color_lab=setup_out$color_lab, shape_lab=setup_out$shape_lab, size_lab=setup_out$size_lab,
        by_row=is_row_plot)
    cmd_list <- c(setup_out$cmd_list, list(plot=c(downsample_cmds, plot_out$cmds)))
    return(cmd_list)
})

#' @export
setMethod(".restrictsSelection", "DotPlot", function(x) {
    x[[.selectEffect]]==.selectRestrictTitle
})
