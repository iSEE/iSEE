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
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"DotPlot"} entry containing \code{valid.assay.names}, a character vector of valid assay names.
#' Valid names are defined as those that are non-empty, i.e., not \code{""}.
#' This method will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{ColorByFeatName} and \code{ColorBySampName} with the first row and column name, respectively, of \code{se}.
#' This will also call the equivalent \linkS4class{Panel} method.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineOutput}(x, id)} returns a UI element for a brushable plot.
#' }
#' 
#' For generating the output:
#' \itemize{
#' \item \code{\link{.generateOutput}(x, se, colormap, all_memory, all_contents)} returns a list containing \code{contents}, a data.frame with one row per point currently present in the plot;
#' \code{plot}, a \link{ggplot} object;
#' and \code{commands}, a list of character vector containing the R commands required to generate \code{contents} and \code{plot}.
#' }
#'
#' For defining reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for some (but not all!) of the slots.
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.renderOutput}(x, se, colormap, output, pObjects, rObjects)} will add a rendered plot element to \code{output}.
#' The reactive expression will add the contents of the plot to \code{pObjects$contents} and the relevant commands to \code{pObjects$commands}.
#' This will also call the equivalent \linkS4class{Panel} method to render the panel information testboxes.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionRestricted}(x)} returns a logical scalar indicating whether \code{x} is restricting the plotted points to those that were selected in a transmitting panel, i.e., is \code{SelectEffect="Restrict"}.
#' \item \code{\link{.multiSelectionCommands}(x, index)} returns a character vector of R expressions that - when evaluated - return a character vector of the names of selected points in the active and/or saved selections of \code{x}.
#' The active selection is returned if \code{index=NA}, otherwise one of the saved selection is returned.
#' \item \code{\link{.multiSelectionActive}(x)} returns \code{x[["BrushData"]]} or \code{NULL} if there is no brush or closed lasso.
#' \item \code{\link{.singleSelectionValue}(x)} returns the name of the first selected element in the active brush. 
#' If no brush is active, \code{NULL} is returned instead.
#' \item \code{\link{.singleSelectionSlots}(x)} will return a list specifying the slots that can be updated by single selections in transmitter panels, mostly related to the choice of coloring parameters.
#' This includes the output of \code{callNextMethod}.
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
#' .createObservers,DotPlot-method
#' .hideInterface,DotPlot-method
#' .multiSelectionRestricted,DotPlot-method
#' .multiSelectionActive,DotPlot-method
#' .multiSelectionCommands,DotPlot-method
#' .multiSelectionDimension,DotPlot-method
#' .singleSelectionValue,DotPlot-method
#' .singleSelectionSlots,DotPlot-method
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "DotPlot", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .facetByRow, .noSelection)
    args <- .empty_default(args, .facetByColumn, .noSelection)

    args <- .empty_default(args, .colorByField, .colorByNothingTitle)
    args <- .empty_default(args, .colorByDefaultColor, "black")
    args <- .empty_default(args, .colorByFeatName, NA_character_)
    args <- .empty_default(args, .colorByRowTable, .noSelection)
    args <- .empty_default(args, .colorBySampName, NA_character_)
    args <- .empty_default(args, .colorByColTable, .noSelection)

    args <- .empty_default(args, .shapeByField, .shapeByNothingTitle)

    args <- .empty_default(args, .sizeByField, .sizeByNothingTitle)

    args <- .empty_default(args, .selectEffect, .selectTransTitle)
    args <- .empty_default(args, .selectColor, "red")
    args <- .empty_default(args, .selectTransAlpha, 0.1)

    args <- .empty_default(args, .dataParamBoxOpen, FALSE)
    args <- .empty_default(args, .visualParamBoxOpen, FALSE)
    args <- .empty_default(args, .visualParamChoice, .visualParamChoiceColorTitle)

    args <- .empty_default(args, .contourAdd, FALSE)
    args <- .empty_default(args, .contourColor, "blue")

    args <- .empty_default(args, .plotPointSize, 1)
    args <- .empty_default(args, .plotPointAlpha, 1)
    args <- .empty_default(args, .plotPointDownsample, FALSE)
    args <- .empty_default(args, .plotPointSampleRes, 200)

    args <- .empty_default(args, .plotFontSize, 1)
    args <- .empty_default(args, .plotLegendPosition, .plotLegendBottomTitle)

    do.call(callNextMethod, c(list(.Object), args))
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
    if (!is.null(.get_common_info(se, "DotPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]
    .set_common_info(se, "DotPlot",
        valid.assay.names=named_assays)
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
setMethod(".createObservers", "DotPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .safe_reactive_init(rObjects, paste0(plot_name, "_", .panelGeneralInfo))

    .create_box_observers(plot_name, c(.visualParamBoxOpen, .selectParamBoxOpen), input, pObjects)

    .create_visual_parameter_choice_observer(plot_name, input, pObjects)

    .create_protected_parameter_observers(plot_name,
        fields=c(.facetByRow, .facetByColumn),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_nonfundamental_parameter_observers(plot_name,
        fields=c(
            .colorByDefaultColor, .selectColor, .selectTransAlpha,
            .shapeByField, .sizeByField,
            .plotPointSize, .plotPointAlpha, .plotFontSize, .plotLegendPosition,
            .plotPointDownsample, .plotPointSampleRes, .contourAdd,
            .contourColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    # Filling the plot interaction observers:
    .create_brush_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_lasso_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_zoom_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".defineOutput", "DotPlot", function(x, id) {
    plot_name <- .getEncodedName(x)
    col <- .getPanelColor(x)

    .define_plot_ui(plot_name, brush_direction="xy",
        height=x[[.organizationHeight]],
        brush_fill=.lighten_color_for_fill(col),
        brush_stroke=col
    )
})

#' @export
setMethod(".renderOutput", "DotPlot", function(x, se, colormap, output, pObjects, rObjects) {
# TODO: move colormap INSIDE se's metadata.
    plot_name <- .getEncodedName(x)

    .create_plot_output(plot_name, se=se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".multiSelectionRestricted", "DotPlot", function(x) {
    x[[.selectEffect]]==.selectRestrictTitle
})

#' @export
setMethod(".multiSelectionActive", "DotPlot", function(x) {
    to_store <- x[[.brushData]]
    if (!length(to_store) || (!.is_brush(to_store) && !to_store$closed)) {
        NULL
    } else {
        to_store
    }
})

#' @export
setMethod(".multiSelectionCommands", "DotPlot", function(x, index) {
    transmitter <- .getEncodedName(x)

    if (is.na(index)) {
        brush_val <- x[[.brushData]]
    } else {
        brush_val <- x[[.multiSelectHistory]][[index]]
    }

    if (.is_brush(brush_val)) {
        "selected <- rownames(shiny::brushedPoints(contents, select));"
    } else if (isTRUE(brush_val$closed)) {
        "selected <- rownames(iSEE::lassoPoints(contents, select));"
    } else { # i.e., an unclosed lasso.
        return(NULL)
    }
})

#' @export
setMethod(".singleSelectionValue", "DotPlot", function(x, pObjects) {
    plot_name <- .getEncodedName(x)
    chosen <- .get_brushed_points(pObjects$contents[[plot_name]], x[[.brushData]])
    if (!length(chosen)) NULL else chosen[1]
})

#' @export
setMethod(".singleSelectionSlots", "DotPlot", function(x) {
    c(callNextMethod(), 
        list(
            list(parameter=.colorByFeatName, source=.colorByRowTable, dimension="row", 
                use_mode=.colorByField, use_value=.colorByFeatNameTitle, protected=FALSE),
            list(parameter=.colorBySampName, source=.colorByColTable, dimension="column",
                use_mode=.colorByField, use_value=.colorBySampNameTitle, protected=FALSE)
        )
    ) 
})

#' @export
setMethod(".generateOutput", "DotPlot", function(x, se, colormap, all_memory, all_contents) {
    # Initialize an environment storing information for generating ggplot commands
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- colormap

    # Doing this first so that .getCommandsDataXY can respond to the selection.
    select_cmds <- .processMultiSelections(x, all_memory, all_contents, plot_env)

    # Apply the function provided to generate XY commands and axis labels
    out_xy <- .getCommandsDataXY(x)
    data_cmds <- .initialize_cmd_store()
    data_cmds <- .add_command(data_cmds, out_xy$data_cmds)
    data_cmds <- .evaluate_commands(data_cmds, plot_env)
    ggplot_labs <- c(x=out_xy$x_lab, y=out_xy$y_lab, title=out_xy$plot_title)

    # Add commands coercing X and Y to appropriate type
    data_cmds <- .add_commands_coerce(plot_env, data_cmds, c("X", "Y"))

    # Add commands adding optional columns to plot.data
    out_color <- .getCommandsDataColor(x, se)
    data_cmds <- .add_command(data_cmds, out_color$cmds, name='color')
    ggplot_labs <- c(ggplot_labs, color = out_color$label)

    out_shape <- .getCommandsDataShape(x, se)
    data_cmds <- .add_command(data_cmds, out_shape$cmds, name='shape')
    ggplot_labs <- c(ggplot_labs, shape = out_shape$label)

    out_size <- .getCommandsDataSize(x, se)
    data_cmds <- .add_command(data_cmds, out_size$cmds, name='size')
    ggplot_labs <- c(ggplot_labs, size = out_size$label)

    facets_cmds <- .getCommandsDataFacets(x, se)
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

    data_cmds <- .add_command(data_cmds, .getCommandsDataSelect(x, plot_env))
    data_cmds <- .evaluate_commands(data_cmds, plot_env)

    # Define the type of plot to create, and add geometry-specific commands, if needed
    out_specific <- .choose_plot_type(plot_env)
    data_cmds <- .add_command(data_cmds, out_specific)
    data_cmds <- .evaluate_commands(data_cmds, plot_env)

    # Collect the plot coordinates BEFORE downsampling (which alters the environment value)
    panel_data <- plot_env$plot.data

    # Add downsampling commands, if applicable
    downsample_cmds <- .downsample_points(x, plot_env)
    data_cmds <- .add_command(data_cmds, downsample_cmds)
    data_cmds <- .evaluate_commands(data_cmds, plot_env)

    # Prepare information about subsetting and downsampling, to generate the plotting commands
    is_subsetted <- exists("plot.data.all", envir=plot_env, inherits=FALSE)
    is_downsampled <- exists("plot.data.pre", envir=plot_env, inherits=FALSE)
    plot_type <- plot_env$plot.type

    # Get the ggplot call
    plot_cmds <- .getCommandsPlot(x, plot_env$plot.data, plot_type, as.list(ggplot_labs), is_subsetted, is_downsampled)

    # Adding a faceting command, if applicable
    facet_cmd <- .add_facets(x)
    if (length(facet_cmd)) {
        N <- length(plot_cmds)
        plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")
        plot_cmds <- c(plot_cmds, facet_cmd)
    }

    # Adding self-brushing boxes, if they exist.
    to_flip <- plot_type == "violin_horizontal"
    self_select_cmds <- .self_select_boxes(x, flip=to_flip)

    if (length(self_select_cmds)) {
        N <- length(plot_cmds)
        plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")

        intermediate <- seq_len(length(self_select_cmds)-1L)
        self_select_cmds[intermediate] <- paste(self_select_cmds[intermediate], "+")
        plot_cmds <- c(plot_cmds, self_select_cmds)

        # We overwrite any existing 'all_brushes' or 'all_lassos',
        # as they have already served their purpose in defining plot_data above
        .populate_selection_environment(x, plot_env)
    }

    # TODO: make sure "plot_env" contains the bare essentials
    # Evaluating the plotting commands.
    plot_out <- .text_eval(plot_cmds, plot_env)

    list(commands=list(select_cmds, data_cmds$processed, plot_cmds), contents=panel_data, plot=plot_out)
})

#' @export
setMethod(".getCommandsPlot", "DotPlot", function(x, plot_data, plot_type, labs, is_subsetted, is_downsampled) {
    is_row_plot <- is(x, "RowDotPlot")

    # TODO: update the functions below to work with a single list "labs"
    switch(plot_type,
        square=.square_plot(plot_data, x, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted),
        violin=.violin_plot(plot_data = plot_data, x, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted),
        violin_horizontal=.violin_plot(plot_data = plot_data, x, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted, horizontal=TRUE),
        scatter=.scatter_plot(plot_data = plot_data, x, labs$x, labs$y, labs$color, labs$shape, labs$size, labs$title,
            is_row_plot, is_subsetted)
    )
})
