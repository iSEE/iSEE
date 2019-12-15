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

    # Setting up the linked colors:
    .define_dimname_observers(plot_name,
        name_field=.colorByFeatName,
        choices=rownames(se),
        in_use_field=.colorByField,
        in_use_value=.colorByFeatNameTitle,
        table_field=.colorByRowTable,
        is_protected=FALSE,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dimname_observers(plot_name,
        name_field=.colorBySampName,
        choices=colnames(se),
        in_use_field=.colorByField,
        in_use_value=.colorBySampNameTitle,
        table_field=.colorByColTable,
        is_protected=FALSE,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    for (field in c(.colorByColTable, .colorByRowTable)) {
        pObjects$aesthetics_links <- .add_interpanel_link(pObjects$aesthetics_links,
            panel_name=plot_name, parent_name=x[[field]], field=field)
    }

    # Filling the plot interaction observers:
    .define_brush_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .define_lasso_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .define_zoom_observer(plot_name, input=input, session=session,
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

    .define_plot_output(mode, id,
        FUN=.getPanelPlottingFunction(x), selectable=TRUE,
        se=se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)

    .define_selection_info_output(mode, id,
        output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".restrictsSelection", "DotPlot", function(x) {
    x[[.selectEffect]]==.selectRestrictTitle
})

#' @export
setMethod(".processTransmission", "DotPlot", function(x, index) {
    transmitter <- paste0(.getEncodedName(x), x[[.organizationId]])

    if (is.na(index)) {
        brush_val <- x[[.brushData]]
        brush_src <- sprintf("all_brushes[['%s']]", transmitter)
    } else {
        brush_val <- x[[.multiSelectHistory]][[i]]
        brush_src <- sprintf("all_select_histories[['%s']][[%i]]", transmitter, index)
    }

    if (.is_brush(brush_val)) {
        cur_cmds <- sprintf("selected0 <- shiny::brushedPoints(transmitter, %s)", brush_src)
    } else if (isTRUE(brush_val$closed)) {
        cur_cmds <- sprintf("selected0 <- iSEE::lassoPoints(transmitter, %s)", brush_src)
    } else { # i.e., an unclosed lasso.
        return(NULL)
    }

    c(cur_cmds, "selected <- rownames(selected0);")
})

#' @export
setMethod(".getPanelPlottingFunction", "DotPlot", function(x) {
    function(param_choices, all_memory, all_coordinates, se, colormap) {
        # Initialize an environment storing information for generating ggplot commands
        plot_env <- new.env()
        plot_env$se <- se
        plot_env$colormap <- colormap

        # Defining the row and column selections, and hoping that the 
        # plot-generating functions know what to do with them.
        row_select_cmds <- .process_selectby_choice(param_choices, 
            by_field=.selectRowSource, type_field=.selectRowType, saved_field=.selectRowSaved,
            all_memory=all_memory, var_name="row_selected")

        if (!is.null(row_select_cmds)) {
            transmitter <- param_choices[[.selectRowSource]]
            .populate_selection_environment(all_memory[[transmitter]], plot_env)
            plot_env$all_coordinates <- all_coordinates
            .text_eval(row_select_cmds, plot_env)
        }

        col_select_cmds <- .process_selectby_choice(param_choices, 
            by_field=.selectColSource, type_field=.selectColType, saved_field=.selectColSaved,
            all_memory=all_memory, var_name="col_selected")

        if (!is.null(col_select_cmds)) {
            transmitter <- param_choices[[.selectColSource]]
            .populate_selection_environment(all_memory[[transmitter]], plot_env)
            plot_env$all_coordinates <- all_coordinates
            .text_eval(col_select_cmds, plot_env)
        }

        # Apply the function provided to generate XY commands and axis labels
        out_xy <- .getCommandsDataXY(x, param_choices)
        data_cmds <- .initialize_cmd_store()
        data_cmds <- .add_command(data_cmds, out_xy$data_cmds)
        data_cmds <- .evaluate_commands(data_cmds, plot_env)
        ggplot_labs <- c(x=out_xy$x_lab, y=out_xy$y_lab, title=out_xy$plot_title)

        # Add commands coercing X and Y to appropriate type
        data_cmds <- .add_commands_coerce(plot_env, data_cmds, c("X", "Y"))

        # Add commands adding optional columns to plot.data
        out_color <- .getCommandsDataColor(x, param_choices, se)
        data_cmds <- .add_command(data_cmds, out_color$cmds, name='color')
        ggplot_labs <- c(ggplot_labs, color = out_color$label)

        out_shape <- .getCommandsDataShape(x, param_choices, se)
        data_cmds <- .add_command(data_cmds, out_shape$cmds, name='shape')
        ggplot_labs <- c(ggplot_labs, shape = out_shape$label)

        out_size <- .getCommandsDataSize(x, param_choices, se)
        data_cmds <- .add_command(data_cmds, out_size$cmds, name='size')
        ggplot_labs <- c(ggplot_labs, size = out_size$label)

        facets_cmds <- .getCommandsDataFacets(x, param_choices, se)
        data_cmds <- .add_command(data_cmds, facets_cmds)

        # Add commands coercing ColorBy to appropriate type, if present
        data_cmds <- .evaluate_commands(data_cmds, plot_env)
        color_data <- plot_env$plot.data$ColorBy
        if (!is.null(color_data)) {
            data_cmds <- .add_commands_coerce(plot_env, data_cmds, c("ColorBy"))
        }

        # Removing NAs in axes aesthetics as they mess up .process_selectby_choice.
        clean_select_fields <- c("X", "Y", names(facets_cmds))
        clean_expression <- paste(sprintf("!is.na(%s)", clean_select_fields), collapse=" & ")
        data_cmds <- .add_command(data_cmds, sprintf("plot.data <- subset(plot.data, %s);", clean_expression), name='na.rm')

        # TODO: fix this to use generics that know to whether to use
        # col_selected or row_selected to create SelectBy.
        if (param_choices[[.selectEffect]]==.selectRestrictTitle) {
            data_cmds <- .add_command(data_cmds, "plot.data <- subset(plot.data, SelectBy);")
        }

        # Define the type of plot to create, and add geometry-specific commands, if needed
        out_specific <- .choose_plot_type(plot_env)
        data_cmds <- .add_command(data_cmds, out_specific)
        data_cmds <- .evaluate_commands(data_cmds, plot_env)

        # Collect the plot coordinates BEFORE downsampling (which alters the environment value)
        panel_data <- plot_env$plot.data

        # Add downsampling commands, if applicable
        downsample_cmds <- .downsample_points(param_choices, plot_env)
        data_cmds <- .add_command(data_cmds, downsample_cmds)

        # Prepare information about subsetting and downsampling, to generate the plotting commands
        is_subsetted <- exists("plot.data.all", envir=plot_env)
        is_downsampled <- exists("plot.data.pre", envir=plot_env)
        plot_type <- plot_env$plot.type

        # Get the ggplot call
        plot_cmds <- .getCommandsPlot(x, param_choices, plot_env$plot.data, plot_type, as.list(ggplot_labs), is_subsetted, is_downsampled)

        # Adding a faceting command, if applicable
        facet_cmd <- .add_facets(param_choices)
        if (length(facet_cmd)) {
            N <- length(plot_cmds)
            plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")
            plot_cmds <- c(plot_cmds, facet_cmd)
        }

        # Adding self-brushing boxes, if they exist.
        to_flip <- plot_type == "violin_horizontal"
        select_cmds <- .self_select_boxes(param_choices, flip=to_flip)

        if (length(select_cmds)) {
            N <- length(plot_cmds)
            plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")

            intermediate <- seq_len(length(select_cmds)-1L)
            select_cmds[intermediate] <- paste(select_cmds[intermediate], "+")
            plot_cmds <- c(plot_cmds, select_cmds)

            # We overwrite any existing 'all_brushes' or 'all_lassos',
            # as they have already served their purpose in defining plot_data above
            .populate_selection_environment(param_choices, plot_env)
        }

        # TODO: make sure "plot_env" contains the bare essentials
        # Evaluating the plotting commands.
        plot_out <- .text_eval(plot_cmds, plot_env)

        return(list(cmd_list=c(data_cmds, plot_cmds), xy=panel_data, plot=plot_out))
    }

})

#' @export
setMethod(".getCommandsPlot", "DotPlot", function(x, param_choices, plot_data, plot_type, labs, is_subsetted, is_downsampled) {

    is_row_plot <- is(x, "RowDotPlot")

    # TODO: update the functions below to work with a single list "labs"
    plot_cmds <- switch(plot_type,
        square=.square_plot(plot_data, param_choices, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted),
        violin=.violin_plot(plot_data = plot_data, param_choices, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted),
        violin_horizontal=.violin_plot(plot_data = plot_data, param_choices, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted, horizontal=TRUE),
        scatter=.scatter_plot(plot_data = plot_data, param_choices, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted)
    )

    return(plot_cmds)
})
