#' The DotPlot virtual class
#'
#' The DotPlot is a virtual class for all panels where each row or column in the \linkS4class{SummarizedExperiment} is represented by a point (\dQuote{dot}) in a brushable plot.
#' It provides slots and methods to control various aesthetics of the dots and to store the brush or lasso selection.
#' 
#' @section Slot overview:
#' The following slots are relevant to coloring of the points:
#' \itemize{
#' \item \code{ColorBy}, a string specifying how points should be colored.
#' This should be one of \code{"None"}, \code{"Feature name"}, \code{"Sample name"} and either \code{"Column data"} (for \linkS4class{ColumnDotPlot}s) or \code{"Row data"} (for \linkS4class{RowDotPlot}s).
#' Defaults to \code{"None"}.
#' \item \code{ColorByDefaultColor}, a string specifying the default color to use for all points if \code{ColorBy="None"}.
#' Defaults to \code{"black"}.
#' \item \code{ColorByFeatName}, a string specifying the feature to be used for coloring points when \code{ColorBy="Feature name"}.
#' For \linkS4class{RowDotPlot}s, this is used to highlight the point corresponding to the selected feature;
#' for \linkS4class{ColumnDotPlot}s, this is used to color each point according to the expression of that feature.
#' If \code{NA}, this defaults to the name of the first row.
#' \item \code{ColorByRowTable}, a string specifying the name of the panel to use for transmitting the feature selection to \code{ColorByFeatName}.
#' Defaults to \code{"---"}.
#' \item \code{ColorBySampName}, a string specifying the sample to be used for coloring points when \code{ColorBy="Sample name"}.
#' For \linkS4class{RowDotPlot}s, this is used to color each point according to the expression of that sample;
#' for \linkS4class{ColumnDotPlot}s, this is used to highlight the point corresponding to the selected sample.
#' If \code{NA}, this defaults to the name of the first column.
#' \item \code{ColorByColTable}, a string specifying the name of the panel to use for transmitting the sample selection to \code{ColorBySampName}.
#' Defaults to \code{"---"}.
#' }
#'
#' The following slots control other metadata-related aesthetic aspects of the points:
#' \itemize{
#' \item \code{ShapeBy}, a string specifying how the point shape should be determined.
#' This should be one of \code{"None"} and either \code{"Column data"} (for \linkS4class{ColumnDotPlot}s) or \code{"Row data"} (for \linkS4class{RowDotPlot}s).
#' Defaults to \code{"None"}.
#' \item \code{SizeBy}, a string specifying the metadata field for controlling point size. 
#' This should be one of \code{"None"} and either \code{"Column data"} (for \linkS4class{ColumnDotPlot}s) or \code{"Row data"} (for \linkS4class{RowDotPlot}s).
#' Defaults to \code{"None"}.
#' }
#'
#' The following slots control the faceting:
#' \itemize{
#' \item \code{FacetByRow}, a string specifying the metadata field to use for creating row facets.
#' For \linkS4class{RowDotPlot}s, this should be a field in the \code{\link{rowData}},
#' while for \linkS4class{ColumnDotPlot}s, this should be a field in the \code{\link{colData}}.
#' Defaults to \code{"---"}, i.e., no row faceting.
#' \item \code{FacetByColumn}, a string specifying the metadata field to use for creating column facets.
#' For \linkS4class{RowDotPlot}s, this should be a field in the \code{\link{rowData}},
#' while for \linkS4class{ColumnDotPlot}s, this should be a field in the \code{\link{colData}}.
#' Defaults to \code{"---"}, i.e., no column faceting.
#' }
#'
#' The following slots control the effect of the transmitted selection from another panel:
#' \itemize{
#' \item \code{SelectEffect}, a string specifying the selection effect.
#' This should be one of \code{"Transparent"} (the default), where all non-selected points become transparent;
#' \code{"Color"}, where all selected points change to the specified color;
#' \code{"Restrict"}, where all non-selected points are not plotted.
#' \item \code{SelectAlpha}, a numeric scalar in [0, 1] specifying the transparency to use for non-selected points when \code{SelectEffect="Transparent"}.
#' Defaults to 0.1.
#' \item \code{SelectColor}, a string specifying the color to use for selected points when \code{SelectEffect="Color"}.
#' Defaults to \code{"red"}.
#' }
#'
#' The following slots control the behavior of brushes:
#' \itemize{
#' \item \code{ZoomData}, a named numeric vector of plot coordinates with \code{"xmin"}, \code{"xmax"}, \code{"ymin"} and \code{"ymax"} elements parametrizing the zoom boundaries.
#' Defaults to an empty vector, i.e., no zoom.
#' \item \code{BrushData}, a list containing either a Shiny brush (see \code{?\link{brushedPoints}}) or an \pkg{iSEE} lasso (see \code{?\link{lassoPoints}}).
#' Defaults to an empty list, i.e., no brush or lasso.
#' }
#'
#' The following slots control some aspects of the user interface:
#' \itemize{
#' \item \code{DataBoxOpen}, a logical scalar indicating whether the data parameter box should be open.
#' Defaults to \code{FALSE}.
#' \item \code{VisualBoxOpen}, a logical scalar indicating whether the visual parameter box should be open.
#' Defaults to \code{FALSE}.
#' \item \code{VisualChoices}, a character vector specifying the visible interface elements upon initialization.
#' This can contain zero or more of \code{"Color"}, \code{"Shape"}, \code{"Facets"}, \code{"Points"} and \code{"Other"}.
#' Defaults to \code{"Color"}.
#' }
#'
#' The following slots control the addition of a contour:
#' \itemize{
#' \item \code{ContourAdd}, logical scalar indicating whether a contour should be added to a (scatter) plot.
#' Defaults to \code{FALSE}.
#' \item \code{ContourColor}, string specifying the color to use for the contour lines.
#' Defaults to \code{"blue"}.
#' }
#'
#' The following slots control the general appearance of the points.
#' \itemize{
#' \item \code{PointSize}, positive numeric scalar specifying the relative size of the points.
#' Defaults to 1.
#' \item \code{PointAlpha}, non-negative numeric scalar specifying the transparency of the points.
#' Defaults to 1, i.e., not transparent.
#' \item \code{Downsample}, logical scalar indicating whether to downsample points for faster plotting.
#' Defaults to \code{FALSE}.
#' \item \code{SampleRes}, numeric scalar specifying the resolution of the downsampling grid (see \code{?\link{subsetPointsByGrid}}) if \code{Downsample=TRUE}.
#' Larger values correspond to reduced downsampling at the cost of plotting speed.
#' Defaults to 200.
#' }
#' 
#' The following slots refer to general plotting parameters:
#' \itemize{
#' \item \code{FontSize}, positive numeric scalar specifying the relative font size.
#' Defaults to 1.
#' \item \code{LegendPosition}, string specifying the position of the legend on the plot.
#' Defaults to \code{"Right"} but can also be \code{"Bottom"}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
#'
#' @section Contract description:
#' This is a rather vaguely defined class for which the only purpose is to avoid duplicating code for \linkS4class{ColumnDotPlot}s and \linkS4class{RowDotPlot}s.
#' Observers are only provided for some slots - the remainders are supported by the aforementioned subclasses - and no interface elements are provided at all.
#' It is likely that developers will prefer to extend these subclasses instead of the \linkS4class{DotPlot} directly, as the former have more well-defined contracts.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{DotPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up the objects:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"DotPlot"} entry containing \code{valid.assay.names}, a character vector of valid assay names (i.e., non-empty and non-duplicated).
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{ColorByFeatName} and \code{ColorBySampName} with the first row and column name, respectively, of \code{se}.
#' This will also call the equivalent \linkS4class{Panel} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineOutputElement}(x, id)} returns a UI element for a brushable plot.
#' }
#'
#' For defining reactive expressions:
#' \itemize{
#' \item \code{\link{.createParamObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for some (but not all!) of the slots. 
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.createRenderedOutput}(x, se, colormap, output, pObjects, rObjects)} will add a rendered plot element to \code{output}.
#' It will also create a rendered UI element for selection information.
#' }
#'
#' For controlling selections: 
#' \itemize{
#' \item \code{\link{.restrictsSelection}(x)} returns a logical scalar indicating whether \code{x} is restricting the plotted points to those that were selected in a transmitting panel, i.e., is \code{SelectEffect="Restrict"}.
#' \item \code{\link{.hasActiveSelection}(x)} returns a logical scalar indicating whether \code{x} has an active brush or lasso.
#' \item \code{\link{.processSelection}(x, index)} returns a character vector of R expressions that - when evaluated - return a character vector of the names of selected points in the active and/or saved selections of \code{x}.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent class \linkS4class{Panel} are also available.
#'
#' @seealso
#' \linkS4class{RowDotPlot} and \linkS4class{ColumnDotPlot}, which are more amenable to extension.
#' 
#' @author Aaron Lun
#'
#' @name DotPlot-class
#' @aliases
#' initialize,DotPlot-method
#' [[,DotPlot-method
#' [[<-,DotPlot-method
#' .refineParameters,DotPlot-method
#' .cacheCommonInfo,DotPlot-method
#' .createParamObservers,DotPlot-method
#' .hideInterfaceElement,DotPlot-method
#' .restrictsSelection,DotPlot-method
#' .transmittedDimension,DotPlot-method
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "DotPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)

    .Object <- .empty_default(.Object, .facetByRow, .noSelection)
    .Object <- .empty_default(.Object, .facetByColumn, .noSelection)

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

    .Object <- .empty_default(.Object, .contourAdd, FALSE)
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
        c(.dataParamBoxOpen, .visualParamBoxOpen,
            .contourAdd,
            .plotPointDownsample))

    msg <- .single_string_error(msg, object,
        c(.colorByField, .colorByFeatName, .colorByRowTable, .colorBySampName, .colorByColTable,
            .shapeByField,
            .sizeByField,
            .selectEffect))

    msg <- .valid_string_error(msg, object,
        c(.colorByDefaultColor,
            .selectColor,
            .facetByRow, .facetByColumn,
            .contourColor))

    msg <- .allowable_choice_error(msg, object, .selectEffect,
        c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle))

    msg <- .valid_number_error(msg, object, .selectTransAlpha, lower=0, upper=1)

    msg <- .multiple_choice_error(msg, object, .visualParamChoice,
        c(.visualParamChoiceColorTitle, .visualParamChoiceShapeTitle, .visualParamChoicePointTitle,
            .visualParamChoiceFacetTitle, .visualParamChoiceOtherTitle))

    msg <- .valid_number_error(msg, object, .plotPointSize, lower=0, upper=Inf)

    msg <- .valid_number_error(msg, object, .plotPointAlpha, lower=0, upper=1)

    msg <- .valid_number_error(msg, object, .plotPointSampleRes, lower=1, upper=Inf)

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
        fields=c(.facetByRow, .facetByColumn),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_nonfundamental_parameter_observers(plot_name,
        fields=c(
            .colorByDefaultColor, .selectColor, .selectTransAlpha,
            .shapeByField, .sizeByField,
            .plotPointSize, .plotPointAlpha, .plotFontSize, .plotLegendPosition,
            .plotPointDownsample, .plotPointSampleRes, .contourAdd,
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
setMethod(".hasActiveSelection", "DotPlot", function(x) {
    length(x[[.brushData]]) > 0L
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
        cur_cmds <- sprintf(".selected <- shiny::brushedPoints(transmitter, %s)", brush_src)
    } else if (isTRUE(brush_val$closed)) {
        cur_cmds <- sprintf(".selected <- iSEE::lassoPoints(transmitter, %s)", brush_src)
    } else { # i.e., an unclosed lasso.
        return(NULL)
    }

    c(cur_cmds, "selected <- rownames(.selected);")
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
