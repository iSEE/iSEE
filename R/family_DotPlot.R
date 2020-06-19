#' The DotPlot virtual class
#'
#' The DotPlot is a virtual class for all panels where each row or column in the \linkS4class{SummarizedExperiment} is represented by no more than one point (i.e., a \dQuote{dot}) in a brushable \link{ggplot} plot.
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
#' \item \code{ColorByFeatureName}, a string specifying the feature to be used for coloring points when \code{ColorBy="Feature name"}.
#' For \linkS4class{RowDotPlot}s, this is used to highlight the point corresponding to the selected feature;
#' for \linkS4class{ColumnDotPlot}s, this is used to color each point according to the expression of that feature.
#' If \code{NA}, this defaults to the name of the first row.
#' \item \code{ColorByFeatureSource}, a string specifying the name of the panel to use for transmitting the feature selection to \code{ColorByFeatureName}.
#' Defaults to \code{"---"}.
#' \item \code{ColorBySampleName}, a string specifying the sample to be used for coloring points when \code{ColorBy="Sample name"}.
#' For \linkS4class{RowDotPlot}s, this is used to color each point according to the expression of that sample;
#' for \linkS4class{ColumnDotPlot}s, this is used to highlight the point corresponding to the selected sample.
#' If \code{NA}, this defaults to the name of the first column.
#' \item \code{ColorBySampleSource}, a string specifying the name of the panel to use for transmitting the sample selection to \code{ColorBySampleNameColor}.
#' Defaults to \code{"---"}.
#' \item \code{ColorByFeatureDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source when coloring by feature.
#' Defaults to \code{FALSE}.
#' \item \code{ColorBySampleDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source when coloring by feature.
#' Defaults to \code{FALSE}.
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
#' \item \code{SelectionEffect}, a string specifying the selection effect.
#' This should be one of \code{"Transparent"} (the default), where all non-selected points become transparent;
#' \code{"Color"}, where all selected points change to the specified color;
#' \code{"Restrict"}, where all non-selected points are not plotted.
#' \item \code{SelectionAlpha}, a numeric scalar in [0, 1] specifying the transparency to use for non-selected points when \code{SelectionEffect="Transparent"}.
#' Defaults to 0.1.
#' \item \code{SelectionColor}, a string specifying the color to use for selected points when \code{SelectionEffect="Color"}.
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
#' This can contain zero or more of \code{"Color"}, \code{"Shape"}, \code{"Size"}, \code{"Point"} , \code{"Facet"}, \code{"Text"}, and \code{"Other"}.
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
#' \item \code{DownsampleResolution}, numeric scalar specifying the resolution of the downsampling grid (see \code{?\link{subsetPointsByGrid}}) if \code{Downsample=TRUE}.
#' Larger values correspond to reduced downsampling at the cost of plotting speed.
#' Defaults to 200.
#' }
#'
#' The following slots refer to general plotting parameters:
#' \itemize{
#' \item \code{FontSize}, positive numeric scalar specifying the relative font size.
#' Defaults to 1.
#' \item \code{PointSize}, positive numeric scalar specifying the relative point size.
#' Defaults to 1.
#' \item \code{LegendPosition}, string specifying the position of the legend on the plot.
#' Defaults to \code{"Right"} but can also be \code{"Bottom"}.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{Panel} class.
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
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{ColorByFeatureName} and \code{ColorBySampleNameColor} with the first row and column name, respectively, of \code{se}.
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
#' \item \code{\link{.generateOutput}(x, se, all_memory, all_contents)} returns a list containing \code{contents}, a data.frame with one row per point currently present in the plot;
#' \code{plot}, a \link{ggplot} object;
#' and \code{commands}, a list of character vector containing the R commands required to generate \code{contents} and \code{plot}.
#' \item \code{\link{.generateDotPlot}(x, labels, envir)} returns a list containing \code{plot} and \code{commands}, as described above.
#' This is called within \code{\link{.generateOutput}} for all \linkS4class{DotPlot} instances by default.
#' Methods are also guaranteed to generate a \code{dot.plot} variable in \code{envir} containing the \link{ggplot} object corresponding to \code{plot}.
#' \item \code{\link{.prioritizeDotPlotData}(x, envir)} returns \code{NULL}.
#' \item \code{\link{.colorByNoneDotPlotField}(x)} returns \code{NULL}.
#' \item \code{\link{.colorByNoneDotPlotScale}(x)} returns \code{NULL}.
#' \item \code{\link{.exportOutput}(x, se, all_memory, all_contents)} will create a PDF file containing the current plot, and return a string containing the path to that PDF.
#' This assumes that the \code{plot} field returned by \code{\link{.generateOutput}} is a \link{ggplot} object.
#' }
#'
#' For defining reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for some (but not all!) of the slots.
#' This will also call the equivalent \linkS4class{Panel} method.
#' \item \code{\link{.renderOutput}(x, se, output, pObjects, rObjects)} will add a rendered plot element to \code{output}.
#' The reactive expression will add the contents of the plot to \code{pObjects$contents} and the relevant commands to \code{pObjects$commands}.
#' This will also call the equivalent \linkS4class{Panel} method to render the panel information testboxes.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionRestricted}(x)} returns a logical scalar indicating whether \code{x} is restricting the plotted points to those that were selected in a transmitting panel, i.e., is \code{SelectionEffect="Restrict"}?
#' \item \code{\link{.multiSelectionCommands}(x, index)} returns a character vector of R expressions that - when evaluated - returns a character vector of the names of selected points in the active and/or saved selections of \code{x}.
#' The active selection is returned if \code{index=NA}, otherwise one of the saved selection is returned.
#' \item \code{\link{.multiSelectionActive}(x)} returns \code{x[["BrushData"]]} or \code{NULL} if there is no brush or closed lasso.
#' \item \code{\link{.multiSelectionClear}(x)} returns \code{x} after setting the \code{BrushData} slot to an empty list.
#' \item \code{\link{.singleSelectionValue}(x)} returns the name of the first selected element in the active brush.
#' If no brush is active, \code{NULL} is returned instead.
#' \item \code{\link{.singleSelectionSlots}(x)} will return a list specifying the slots that can be updated by single selections in transmitter panels, mostly related to the choice of coloring parameters.
#' This includes the output of \code{callNextMethod}.
#' }
#'
#' Unless explicitly specialized above, all methods from the parent class \linkS4class{Panel} are also available.
#'
#' @section Subclass expectations:
#' The DotPlot is a rather vaguely defined class for which the only purpose is to avoid duplicating code for \linkS4class{ColumnDotPlot}s and \linkS4class{RowDotPlot}s.
#' We recommend extending those subclasses instead.
#'
#' @seealso
#' \linkS4class{RowDotPlot} and \linkS4class{ColumnDotPlot}, which are more amenable to extension.
#'
#' @author Aaron Lun
#'
#' @name DotPlot-class
#' @aliases
#' initialize,DotPlot-method
#' .defineOutput,DotPlot-method
#' .generateOutput,DotPlot-method
#' .generateDotPlot,DotPlot-method
#' .renderOutput,DotPlot-method
#' .exportOutput,DotPlot-method
#' .refineParameters,DotPlot-method
#' .cacheCommonInfo,DotPlot-method
#' .createObservers,DotPlot-method
#' .hideInterface,DotPlot-method
#' .multiSelectionRestricted,DotPlot-method
#' .multiSelectionActive,DotPlot-method
#' .multiSelectionCommands,DotPlot-method
#' .multiSelectionClear,DotPlot-method
#' .multiSelectionDimension,DotPlot-method
#' .singleSelectionValue,DotPlot-method
#' .singleSelectionSlots,DotPlot-method
#' .prioritizeDotPlotData,DotPlot-method
#' .colorByNoneDotPlotField,DotPlot-method
#' .colorByNoneDotPlotScale,DotPlot-method
#' .defineVisualTextInterface,DotPlot-method
#' .defineVisualOtherInterface,DotPlot-method
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "DotPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .facetByRow, .noSelection)
    args <- .emptyDefault(args, .facetByColumn, .noSelection)

    args <- .emptyDefault(args, .colorByField, .colorByNothingTitle)
    args <- .emptyDefault(args, .colorByDefaultColor, iSEEOptions$get("point.color"))

    args <- .emptyDefault(args, .colorByFeatName, NA_character_)
    args <- .emptyDefault(args, .colorByFeatDynamic, iSEEOptions$get("selection.dynamic.single"))
    args <- .emptyDefault(args, .colorByRowTable, .noSelection)

    args <- .emptyDefault(args, .colorBySampName, NA_character_)
    args <- .emptyDefault(args, .colorBySampDynamic, iSEEOptions$get("selection.dynamic.single"))
    args <- .emptyDefault(args, .colorByColTable, .noSelection)

    args <- .emptyDefault(args, .shapeByField, .shapeByNothingTitle)

    args <- .emptyDefault(args, .sizeByField, .sizeByNothingTitle)

    args <- .emptyDefault(args, .selectEffect, .selectTransTitle)
    args <- .emptyDefault(args, .selectColor, iSEEOptions$get("selected.color"))
    args <- .emptyDefault(args, .selectTransAlpha, iSEEOptions$get("selected.alpha"))

    args <- .emptyDefault(args, .visualParamBoxOpen, FALSE)
    args <- .emptyDefault(args, .visualParamChoice, .visualParamChoiceColorTitle)

    args <- .emptyDefault(args, .contourAdd, FALSE)
    args <- .emptyDefault(args, .contourColor, iSEEOptions$get("contour.color"))

    args <- .emptyDefault(args, .plotPointSize, iSEEOptions$get("point.size"))
    args <- .emptyDefault(args, .plotPointAlpha, iSEEOptions$get("point.alpha"))
    args <- .emptyDefault(args, .plotPointDownsample, iSEEOptions$get("downsample"))
    args <- .emptyDefault(args, .plotPointSampleRes, iSEEOptions$get("downsample.resolution"))

    args <- .emptyDefault(args, .plotFontSize, iSEEOptions$get("font.size"))
    args <- .emptyDefault(args, .legendPointSize, iSEEOptions$get("legend.point.size"))
    args <- .emptyDefault(args, .plotLegendPosition, iSEEOptions$get("legend.position"))

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("DotPlot", function(object) {
    msg <- character(0)

    msg <- .valid_logical_error(msg, object,
        c(.visualParamBoxOpen, .contourAdd, .plotPointDownsample))

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
        c(.visualParamChoiceColorTitle, .visualParamChoiceShapeTitle, .visualParamChoiceSizeTitle, .visualParamChoicePointTitle,
            .visualParamChoiceFacetTitle, .visualParamChoiceTextTitle, .visualParamChoiceOtherTitle))

    msg <- .valid_number_error(msg, object, .plotPointSize, lower=0, upper=Inf)

    msg <- .valid_number_error(msg, object, .plotPointAlpha, lower=0, upper=1)

    msg <- .valid_number_error(msg, object, .plotPointSampleRes, lower=1, upper=Inf)

    msg <- .valid_number_error(msg, object, .plotFontSize, lower=0, upper=Inf)
    
    msg <- .valid_number_error(msg, object, .legendPointSize, lower=0, upper=Inf)

    msg <- .allowable_choice_error(msg, object, .plotLegendPosition,
        c(.plotLegendRightTitle, .plotLegendBottomTitle))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment assayNames
setMethod(".cacheCommonInfo", "DotPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "DotPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]
    .setCachedCommonInfo(se, "DotPlot",
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

    .create_box_observers(plot_name, .visualParamBoxOpen, input, pObjects)

    .create_visual_parameter_choice_observer(plot_name, input, pObjects)

    .createProtectedParameterObservers(plot_name,
        fields=c(.facetByRow, .facetByColumn),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(
            .colorByDefaultColor, .selectColor, .selectTransAlpha,
            .shapeByField, .sizeByField,
            .plotPointSize, .plotPointAlpha, .plotFontSize, .legendPointSize, .plotLegendPosition,
            .plotPointDownsample, .plotPointSampleRes, .contourAdd,
            .contourColor),
        input=input, pObjects=pObjects, rObjects=rObjects)

    # Filling the plot interaction observers:
    .create_brush_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_lasso_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_zoom_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)
})

# Interface ----

#' @export
setMethod(".defineVisualTextInterface", "DotPlot", function(x) {
    plot_name <- .getEncodedName(x)

    tagList(
        numericInput(
            paste0(plot_name, "_", .plotFontSize), label="Font size:",
            min=0, value=x[[.plotFontSize]]),
        numericInput(
            paste0(plot_name, "_", .legendPointSize), label="Legend point size:",
            min=0, value=x[[.legendPointSize]]),
        radioButtons(
            paste0(plot_name, "_", .plotLegendPosition), label="Legend position:", inline=TRUE,
            choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
            selected=x[[.plotLegendPosition]])
    )

})

#' @export
#' @export
setMethod(".defineVisualOtherInterface", "DotPlot", function(x) {
    NULL
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
#' @importFrom shiny renderPlot tagList
setMethod(".renderOutput", "DotPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.

    # nocov start
    output[[plot_name]] <- renderPlot({
        p.out <- .retrieveOutput(plot_name, se, pObjects, rObjects)
        pObjects$varname[[plot_name]] <- "plot.data"
        p.out$plot
    })
    # nocov end

    callNextMethod()
})

#' @export
#' @importFrom grDevices pdf dev.off
setMethod(".exportOutput", "DotPlot", function(x, se, all_memory, all_contents) {
    contents <- .generateOutput(x, se, all_memory=all_memory, all_contents=all_contents)
    newpath <- paste0(.getEncodedName(x), ".pdf")

    # These are reasonably satisfactory heuristics:
    # Width = Pixels -> Inches, Height = Bootstrap -> Inches.
    pdf(newpath, width=x[[.organizationHeight]]/75, height=x[[.organizationWidth]]*2)
    print(contents$plot)
    dev.off()

    newpath
})

#' @export
setMethod(".multiSelectionRestricted", "DotPlot", function(x) {
    x[[.selectEffect]] == .selectRestrictTitle
})

#' @export
setMethod(".multiSelectionClear", "DotPlot", function(x) {
    x[[.brushData]] <- list()
    x
})

#' @export
setMethod(".multiSelectionActive", "DotPlot", function(x) {
    to_store <- x[[.brushData]]
    if (.is_brush(to_store) || .is_closed_lasso(to_store)) {
        to_store
    } else {
        NULL
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
    } else {
        "selected <- rownames(iSEE::lassoPoints(contents, select));"
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
            list(parameter=.colorByFeatName,
                source=.colorByRowTable,
                dimension="feature",
                use_mode=.colorByField,
                use_value=.colorByFeatNameTitle,
                dynamic=.colorByFeatDynamic,
                protected=FALSE
            ),
            list(parameter=.colorBySampName,
                source=.colorByColTable,
                dimension="sample",
                use_mode=.colorByField,
                use_value=.colorBySampNameTitle,
                dynamic=.colorBySampDynamic,
                protected=FALSE
            )
        )
    )
})

#' @export
#' @importFrom S4Vectors metadata
setMethod(".generateOutput", "DotPlot", function(x, se, all_memory, all_contents) {
    # Initialize an environment storing information for generating ggplot commands
    plot_env <- new.env()
    plot_env$se <- se
    plot_env$colormap <- metadata(se)$colormap

    all_cmds <- list()
    all_labels <- list()

    # Doing this first so that .generateDotPlotData can respond to the selection.
    all_cmds$select <- .processMultiSelections(x, all_memory, all_contents, plot_env)

    xy_out <- .generateDotPlotData(x, plot_env)
    all_cmds$xy <- xy_out$commands
    all_labels <- c(all_labels, xy_out$labels)

    extra_out <- .add_extra_aesthetic_columns(x, plot_env)
    all_cmds <- c(all_cmds, extra_out$commands)
    all_labels <- c(all_labels, extra_out$labels)

    select_out2 <- .add_selectby_column(x, plot_env)
    all_cmds <- c(all_cmds, select_out2)

    # We need to set up the plot type before downsampling,
    # to ensure the X/Y jitter is correctly computed.
    all_cmds$setup <- .choose_plot_type(plot_env)

    # Also collect the plot coordinates before downsampling.
    panel_data <- plot_env$plot.data

    # Non-data-related fiddling to affect the visual display.
    # First, scrambling the plot.data to avoid biases.
    scramble_cmds <- c(
        "# Avoid visual biases from default ordering by shuffling the points",
        sprintf("set.seed(%i);", nrow(panel_data)), # Using a deterministically different seed to keep things exciting.
        "plot.data <- plot.data[sample(nrow(plot.data)),,drop=FALSE];"
    )
    .textEval(scramble_cmds, plot_env)
    all_cmds$shuffle <- scramble_cmds

    # Next, reordering by priority (this is stable so any ordering due to the
    # shuffling above is still preserved within each priority level).
    priority_out <- .prioritizeDotPlotData(x, plot_env)
    rescaled_res <- FALSE
    if (has_priority <- !is.null(priority_out)) {
        order_cmds <- "plot.data <- plot.data[order(.priority),,drop=FALSE];"
        .textEval(order_cmds, plot_env)
        all_cmds$priority <- c(priority_out$commands, order_cmds)
        rescaled_res <- priority_out$rescaled
    }

    # Finally, the big kahuna of downsampling.
    all_cmds$downsample <- .downsample_points(x, plot_env, priority=has_priority, rescaled=rescaled_res)

    plot_out <- .generateDotPlot(x, all_labels, plot_env)
    all_cmds$plot <- plot_out$commands

    list(commands=all_cmds, contents=panel_data, plot=plot_out$plot)
})

#' @export
setMethod(".generateDotPlot", "DotPlot", function(x, labels, envir) {
    plot_data <- envir$plot.data
    is_subsetted <- exists("plot.data.all", envir=envir, inherits=FALSE)
    is_downsampled <- exists("plot.data.pre", envir=envir, inherits=FALSE)
    plot_type <- envir$plot.type

    args <- list(plot_data,
        param_choices=x,
        x_lab=labels$X,
        y_lab=labels$Y,
        color_lab=labels$ColorBy,
        shape_lab=labels$ShapeBy,
        size_lab=labels$SizeBy,
        title=labels$title,
        is_subsetted=is_subsetted,
        is_downsampled=is_downsampled)

    plot_cmds <- switch(plot_type,
        square=do.call(.square_plot, args),
        violin=do.call(.violin_plot, args),
        violin_horizontal=do.call(.violin_plot, c(args, list(horizontal=TRUE))),
        scatter=do.call(.scatter_plot, args)
    )

    # Adding a faceting command, if applicable.
    facet_cmd <- .addFacets(x)
    if (length(facet_cmd)) {
        N <- length(plot_cmds)
        plot_cmds[[N]] <- paste(plot_cmds[[N]], "+")
        plot_cmds <- c(plot_cmds, facet_cmd)
    }

    # Adding self-brushing boxes, if they exist.
    plot_cmds <- .addMultiSelectionPlotCommands(x,
        flip=(plot_type == "violin_horizontal"),
        envir=envir, commands=plot_cmds)

    list(plot=.textEval(plot_cmds, envir), commands=plot_cmds)
})

#' @export
setMethod(".prioritizeDotPlotData", "DotPlot", function(x, envir) NULL)

#' @export
setMethod(".colorByNoneDotPlotField", "DotPlot", function(x) NULL)

#' @export
setMethod(".colorByNoneDotPlotScale", "DotPlot", function(x) NULL)
