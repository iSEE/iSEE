#' The DotPlot virtual class
#'
#' The DotPlot is a virtual class for all panels where each row or column in the \linkS4class{SummarizedExperiment} is represented by no more than one point (i.e., a \dQuote{dot}) in a brushable \link{ggplot} plot.
#' It provides slots and methods to create the plot, to control various aesthetics of the dots, and to store the brush or lasso selection.
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
#' The following slots control any text to be shown on the plot:
#' \itemize{
#' \item \code{LabelCenters}, a logical scalar indicating whether the label the centers (technically medoids) of all cells in each group, where groups are defined by a discrete covariate in the relevant metadata field.
#' Defaults to \code{FALSE}.
#' \item \code{LabelCentersBy}, a string specifying the metadata field to define the groups when \code{LabelCenters} is \code{TRUE}.
#' This should be a discrete variable in \code{\link{rowData}} or \code{\link{colData}} for \linkS4class{RowDotPlot}s and \linkS4class{ColumnDotPlot}s, respectively.
#' Defaults to the name of the first column.
#' \item \code{LabelCentersColor}, a string specifying the color used for the labels at the center of each group.
#' Only used when \code{LabelCenters} is \code{TRUE}.
#' Defaults to \code{"black"}.
#' \item \code{CustomLabels}, a logical scalar indicating whether custom labels should be inserted on specific points.
#' Defaults to \code{FALSE}.
#' \item \code{CustomLabelsText}, a (possibly multi-line) string with the names of the points to label when \code{CustomLabels} is set to \code{TRUE}.
#' Each line should contain the name of a row or column for \linkS4class{RowDotPlot}s and \linkS4class{ColumnDotPlot}s, respectively.
#' Leading and trailing whitespace are stripped, and all text on a line after \code{#} is ignored.
#' Defaults to the name of the first row/column.
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
#' The following slots control interactions with the plot image:
#' \itemize{
#' \item \code{ZoomData}, a named numeric vector of plot coordinates with \code{"xmin"}, \code{"xmax"}, \code{"ymin"} and \code{"ymax"} elements parametrizing the zoom boundaries.
#' Defaults to an empty vector, i.e., no zoom.
#' \item \code{BrushData}, a list containing either a Shiny brush (see \code{?\link{brushedPoints}}) or an \pkg{iSEE} lasso (see \code{?\link{lassoPoints}}).
#' Defaults to an empty list, i.e., no brush or lasso.
#' \item \code{HoverInfo}, a logical scalar indicating whether the feature/sample name should be shown upon mouse-over of the point.
#' Defaults to \code{TRUE}.
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
#' \item \code{\link{.defineInterface}(x, se, select_info)} defines the user interface for manipulating all slots described above and in the parent classes.
#' It will also create a data parameter box that can respond to specialized \code{\link{.defineDataInterface}}.
#' This will \emph{override} the \linkS4class{Panel} method.
#' \item \code{\link{.defineVisualColorInterface}(x, se, select_info)} defines the user interface subpanel for manipulating the color of the points.
#' \item \code{\link{.defineVisualShapeInterface}(x, se)} defines the user interface subpanel for manipulating the shape of the points.
#' \item \code{\link{.defineVisualSizeInterface}(x, se)} defines the user interface subpanel for manipulating the size of the points.
#' \item \code{\link{.defineVisualPointInterface}(x, se)} defines the user interface subpanel for manipulating other point-related parameters.
#' \item \code{\link{.defineVisualFacetInterface}(x, se)} defines the user interface subpanel for manipulating facet-related parameters.
#' \item \code{\link{.defineVisualTextInterface}(x, se)} defines the user interface subpanel for manipulating text-related parameters.
#' \item \code{\link{.defineVisualOtherInterface}(x, se)} defines the user interface subpanel for manipulating other parameters.
#' Currently this returns \code{NULL}.
#' \item \code{\link{.defineOutput}(x)} returns a UI element for a brushable plot.
#' }
#'
#' For generating the output:
#' \itemize{
#' \item \code{\link{.generateOutput}(x, se, all_memory, all_contents)} returns a list containing \code{contents}, a data.frame with one row per point currently present in the plot;
#' \code{plot}, a \link{ggplot} object;
#' \code{commands}, a list of character vector containing the R commands required to generate \code{contents} and \code{plot};
#' and \code{varname}, a string containing the name of the variable in \code{commands} that was used to obtain \code{contents}.
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
#' This will also call the equivalent \linkS4class{Panel} method to render the panel information text boxes.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionRestricted}(x)} returns a logical scalar indicating whether \code{x} is restricting the plotted points to those that were selected in a transmitting panel, i.e., is \code{SelectionEffect="Restrict"}?
#' \item \code{\link{.multiSelectionCommands}(x, index)} returns a character vector of R expressions that - when evaluated - returns a character vector of the names of selected points in the active and/or saved selections of \code{x}.
#' The active selection is returned if \code{index=NA}, otherwise one of the saved selection is returned.
#' \item \code{\link{.multiSelectionActive}(x)} returns \code{x[["BrushData"]]} or \code{NULL} if there is no brush or closed lasso.
#' \item \code{\link{.multiSelectionClear}(x)} returns \code{x} after setting the \code{BrushData} slot to an empty list.
#' \item \code{\link{.singleSelectionValue}(x, contents)} returns the name of the first selected element in the active brush.
#' If no brush is active, \code{NULL} is returned instead.
#' \item \code{\link{.singleSelectionSlots}(x)} will return a list specifying the slots that can be updated by single selections in transmitter panels, mostly related to the choice of coloring parameters.
#' This includes the output of \code{callNextMethod}.
#' }
#'
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns an data.frame containing the steps of a tour relevant to subclasses,
#' mostly describing the specification of visual effects and the creation of a brush or lasso.
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
#' .defineInterface,DotPlot-method
#' .defineVisualColorInterface,DotPlot-method
#' .defineVisualSizeInterface,DotPlot-method
#' .defineVisualShapeInterface,DotPlot-method
#' .defineVisualTextInterface,DotPlot-method
#' .defineVisualPointInterface,DotPlot-method
#' .defineVisualOtherInterface,DotPlot-method
#' .defineVisualFacetInterface,DotPlot-method
#' .definePanelTour,DotPlot-method
#' .updateObject,DotPlot-method
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

    args <- .emptyDefault(args, .plotCustomLabels, FALSE)
    args <- .emptyDefault(args, .plotCustomLabelsText, NA_character_)
    args <- .emptyDefault(args, .plotFontSize, iSEEOptions$get("font.size"))
    args <- .emptyDefault(args, .legendPointSize, iSEEOptions$get("legend.point.size"))
    args <- .emptyDefault(args, .plotLegendPosition, iSEEOptions$get("legend.position"))

    args <- .emptyDefault(args, .plotHoverInfo, TRUE)

    args <- .emptyDefault(args, .plotLabelCenters, FALSE)
    args <- .emptyDefault(args, .plotLabelCentersBy, NA_character_)
    args <- .emptyDefault(args, .plotLabelCentersColor, "black") 

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("DotPlot", function(object) {
    msg <- character(0)

    msg <- .valid_logical_error(msg, object,
        c(.plotCustomLabels, .visualParamBoxOpen, .contourAdd, .plotPointDownsample, 
            .plotHoverInfo,
            .plotLabelCenters            
        ))

    msg <- .single_string_error(msg, object,
        c(.plotCustomLabelsText, .colorByField, .colorByFeatName, .colorByRowTable, .colorBySampName, .colorByColTable,
            .shapeByField,
            .sizeByField,
            .selectEffect,
            .plotLabelCentersBy
        ))

    msg <- .valid_string_error(msg, object,
        c(.colorByDefaultColor,
            .selectColor,
            .facetByRow, .facetByColumn,
            .contourColor,
            .plotLabelCentersColor
        ))

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
    x <- .replace_na_with_first(x, .plotLabelCentersBy, .getDiscreteMetadataChoices(x, se))

    x
})

#' @export
setMethod(".createObservers", "DotPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)
    plot_dimension <- .multiSelectionDimension(x)

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
            .contourColor, .plotCustomLabels, .plotHoverInfo, 
            .plotLabelCenters, .plotLabelCentersBy, .plotLabelCentersColor),
        input=input, pObjects=pObjects, rObjects=rObjects)

    # Filling the plot interaction observers:
    .create_brush_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_lasso_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_zoom_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_hover_observer(plot_name, input=input, session=session, pObjects=pObjects)

    .create_modal_observers_for_dimnames(plot_name, .plotCustomLabelsText, .dimnamesModalOpen,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects, plot_dimension)
})

# Interface ----

#' @export
setMethod(".defineInterface", "DotPlot", function(x, se, select_info) {
    list(
        .create_data_param_box(x, se, select_info),
        .create_visual_box(x, se, select_info$single),
        .create_dotplot_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

#' @export
setMethod(".defineVisualColorInterface", "DotPlot", function(x, se, select_info) {
    covariates <- .getMetadataChoices(x, se)
    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(x)
    colorby_field <- paste0(plot_name, "_", .colorByField)

    colorby <- .getDotPlotColorConstants(x)
    mydim_single <- .singleSelectionDimension(x)
    otherdim_single <- setdiff(c("feature", "sample"), mydim_single)
    mydim_choices <- select_info[[mydim_single]]
    otherdim_choices <- select_info[[otherdim_single]]

    tagList(
        hr(),
        radioButtons(
            colorby_field, label="Color by:", inline=TRUE,
            choices=.defineDotPlotColorChoices(x, se),
            selected=x[[.colorByField]]
        ),
        .conditional_on_radio(
            colorby_field, .colorByNothingTitle,
            colourInput(
                paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                value=x[[.colorByDefaultColor]])
        ),
        .conditional_on_radio(
            colorby_field, colorby$metadata$title,
            selectInput(
                paste0(plot_name, "_", colorby$metadata$field), label=NULL,
                choices=covariates, selected=x[[colorby$metadata$field]])
        ),
        .conditional_on_radio(colorby_field, colorby$name$title,
            selectizeInput(paste0(plot_name, "_", colorby$name$field),
                label=NULL, selected=NULL, choices=NULL, multiple=FALSE),
            selectInput(
                paste0(plot_name, "_", colorby$name$table), label=NULL, choices=mydim_choices,
                selected=.choose_link(x[[colorby$name$table]], mydim_choices)),
            colourInput(paste0(plot_name, "_", colorby$name$color), label=NULL,
                value=x[[colorby$name$color]]),
            checkboxInput(
                paste0(plot_name, "_", colorby$name$dynamic), 
                label=sprintf("Use dynamic %s selection", mydim_single), 
                value=x[[colorby$name$dynamic]])
        ),
        .conditional_on_radio(colorby_field, colorby$assay$title,
            selectizeInput(paste0(plot_name, "_", colorby$assay$field),
                label=NULL, choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(
                paste0(plot_name, "_", colorby$assay$assay), label=NULL,
                choices=all_assays, selected=x[[colorby$assay$assay]]),
            selectInput(
                paste0(plot_name, "_", colorby$assay$table), label=NULL, choices=otherdim_choices, 
                selected=.choose_link(x[[colorby$assay$table]], otherdim_choices)),
            checkboxInput(
                paste0(plot_name, "_", colorby$assay$dynamic), 
                label=sprintf("Use dynamic %s selection", otherdim_single),
                value=x[[colorby$assay$dynamic]])
        )
    )
})

#' @export
setMethod(".defineVisualShapeInterface", "DotPlot", function(x, se) {
    discrete_covariates <- .getDiscreteMetadataChoices(x, se)

    if (length(discrete_covariates)) {
        plot_name <- .getEncodedName(x)
        shapeby_field <- paste0(plot_name, "_", .shapeByField)
        shapeby <- .getDotPlotShapeConstants(x)
        
        tagList(
            hr(),
            radioButtons(
                shapeby_field, label="Shape by:", inline=TRUE,
                choices=c(.shapeByNothingTitle, if (length(discrete_covariates)) shapeby$metadata$title),
                selected=x[[.shapeByField]]
            ),
            .conditional_on_radio(
                shapeby_field, shapeby$metadata$title,
                selectInput(
                    paste0(plot_name, "_", shapeby$metadata$field), label=NULL,
                    choices=discrete_covariates, selected=x[[shapeby$metadata$field]])
            )
        )
    } else {
        NULL
    }
})

#' @export
setMethod(".defineVisualSizeInterface", "DotPlot", function(x, se) {
    numeric_covariates <- .getContinuousMetadataChoices(x, se)
    plot_name <- .getEncodedName(x)
    sizeby_field <- paste0(plot_name, "_", .sizeByField)
    sizeby <- .getDotPlotSizeConstants(x)

    tagList(
        hr(),
        radioButtons(
            sizeby_field, label="Size by:", inline=TRUE,
            choices=c(.sizeByNothingTitle, if (length(numeric_covariates)) sizeby$metadata$title),
            selected=x[[.sizeByField]]
        ),
        .conditional_on_radio(
            sizeby_field, .sizeByNothingTitle,
            numericInput(
                paste0(plot_name, "_", .plotPointSize), label="Point size:",
                min=0, value=x[[.plotPointSize]])
        ),
        .conditional_on_radio(
            sizeby_field, sizeby$metadata$title,
            selectInput(paste0(plot_name, "_", sizeby$metadata$field), label=NULL,
                choices=numeric_covariates, selected=x[[sizeby$metadata$field]])
        )
    )
})

#' @export
setMethod(".defineVisualPointInterface", "DotPlot", function(x, se) {
    plot_name <- .getEncodedName(x)
    tagList(
        hr(),
        .add_point_UI_elements(x),
        checkboxInput(
            inputId=paste0(plot_name, "_", .contourAdd),
            label="Add contour (scatter only)",
            value=FALSE),
        .conditional_on_check_solo(
            paste0(plot_name, "_", .contourAdd),
            on_select=TRUE,
            colourInput(
                paste0(plot_name, "_", .contourColor), label=NULL,
                value=x[[.contourColor]]))
    )
})

#' @export
setMethod(".defineVisualFacetInterface", "DotPlot", function(x, se) {
    discrete_covariates <- .getDiscreteMetadataChoices(x, se)

    if (length(discrete_covariates)) {
        tagList(
            hr(),
            .add_facet_UI_elements(x, discrete_covariates)
        )
    } else {
        NULL
    }

})

#' @export
setMethod(".defineVisualTextInterface", "DotPlot", function(x, se) {
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    tagList(
        hr(),
        checkboxInput(.input_FUN(.plotHoverInfo),
            label=sprintf("Show %s details on hover", .singleSelectionDimension(x)),
            value=x[[.plotHoverInfo]]),
        hr(),
        checkboxInput(.input_FUN(.plotCustomLabels),
            label=sprintf("Label custom %ss", .singleSelectionDimension(x)),
            value=x[[.plotCustomLabels]]),
        .conditional_on_check_solo(
            .input_FUN(.plotCustomLabels),
            on_select=TRUE,
            actionButton(.input_FUN(.dimnamesModalOpen), 
                label=sprintf("Edit %s names", .singleSelectionDimension(x)))
        ), 
        hr(),
        checkboxInput(.input_FUN(.plotLabelCenters),
            label="Label centers",
            value=x[[.plotLabelCenters]]),
        .conditional_on_check_solo(
            .input_FUN(.plotLabelCenters),
            on_select=TRUE,
            selectInput(.input_FUN(.plotLabelCentersBy),
                label="Label centers:",
                choices=.getDiscreteMetadataChoices(x, se),
                selected=x[[.plotLabelCentersBy]]),
            colourInput(.input_FUN(.plotLabelCentersColor),
                label=NULL,
                value=x[[.plotLabelCentersColor]])
        ),
        hr(),
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
setMethod(".defineOutput", "DotPlot", function(x) {
    plot_name <- .getEncodedName(x)
    col <- .getPanelColor(x)

    .define_plot_ui(plot_name, brush_direction="xy",
        height=x[[.organizationHeight]],
        brush_fill=.lighten_color_for_fill(col),
        brush_stroke=col
    )
})

#' @export
#' @importFrom shiny renderPlot tagList wellPanel nearPoints renderUI
setMethod(".renderOutput", "DotPlot", function(x, se, output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)
    force(se) # defensive programming to avoid difficult bugs due to delayed evaluation.

    # nocov start
    output[[plot_name]] <- renderPlot({
        .retrieveOutput(plot_name, se, pObjects, rObjects)$plot
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
setMethod(".singleSelectionValue", "DotPlot", function(x, contents) {
    plot_name <- .getEncodedName(x)
    chosen <- .get_brushed_points(contents, x[[.brushData]])
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
#' @importFrom grid unit
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

    list(commands=all_cmds, contents=panel_data, plot=plot_out$plot, varname="plot.data")
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

    plot_cmds <- .addCustomLabelsCommands(x, commands=plot_cmds, plot_type=plot_type)

    if (plot_type=="scatter") {
        plot_cmds <- .addLabelCentersCommands(x, commands=plot_cmds)
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

#' @export
setMethod(".definePanelTour", "DotPlot", function(x) {
    mdim <- .multiSelectionDimension(x)

    collated <- rbind(
        .add_tour_step(x, .visualParamBoxOpen,  "The <i>Visual parameters</i> box contains parameters related to visual aspects like the color, shape, size and so on.<br/><br/><strong>Action:</strong> click on the header of this box to see the available options."),
        .add_tour_step(x, .colorByField, "PLACEHOLDER_COLOR"), # To be filled in by subclasses.
        .add_tour_step(x, .visualParamChoice, "There are a lot of options so not all of them are shown by default. More settings are available by checking some of the boxes here; conversely, options can be hidden by unchecking some of these boxes.<br/><br/>Most of these parameters here are fairly self-explanatory and can be explored at leisure. However, we will highlight one particularly useful piece of functionality.<br/><br/><strong>Action:</strong> tick the checkbox labelled \"Text\"."),
        .add_tour_step(x, .plotCustomLabels, sprintf("Users can show the names of certain %ss alongside the locations of their data points on the plot.<br/><br/><strong>Action:</strong> tick the checkbox to enable custom labels.", mdim)),
        .add_tour_step(x, .dimnamesModalOpen, sprintf("When custom labels are enabled, this button can launch a modal containing a text editor where users can specify the data points to label - in this case, using their %s names.", mdim)),
        callNextMethod(),
        .add_tour_step(x, .selectEffect, sprintf("Here, we can choose the effect of the multiple %s selection that was transmitted from the chosen source panel - should the unselected %ss be made transparent? Should the selected %ss be colored? Or should the plot be explicitly restricted to only the selected %s?", mdim, mdim, mdim, mdim)),
        c(paste0("#", .getEncodedName(x)), sprintf("At the other end of the spectrum, brushing or creating a lasso on this plot will create a selection of multiple %ss, to be transmitted to other panels that choose this one as their selection source.<br/><br/>Drag-and-dropping will create a rectangular brush while a single click will lay down a lasso waypoint for non-rectangular selections.<br/><br/>Brushes and lassos can also be used to transmit single %s selections in which case one %s is arbitrarily chosen from the selection.", mdim, mdim, mdim)),
        .add_tour_step(x, .multiSelectSave, "Advanced users can also save their selections for later use. Brushes and lassos are saved using a first-in-last-out scheme where you can only delete the last saved selection.")
    )

    for (mdim in c("row", "column")) {
        edit <- paste0("PLACEHOLDER_", toupper(mdim), "_SELECT")
        i <- which(collated$intro==edit)
        collated[i,"intro"] <- sprintf("Here we can choose the \"source\" panel from which to receive a multiple %s selection; that is to say, if we selected some %ss of the <code>SummarizedExperiment</code> object in the chosen source panel, the corresponding points in the plot above would be highlighted in some manner.", mdim, mdim)
    }

    collated
})

#' @export
#' @importFrom BiocGenerics updateObject
setMethod("updateObject", "DotPlot", function(object) {
    # Backwards compatibility for new slots (added 3.12).
    # nocov start
    if (is(try(object[[.plotHoverInfo]], silent=TRUE), "try-error")) {
        .Deprecated(msg=sprintf("'%s' is out of date, run 'updateObject(<%s>)'", class(object)[1], class(object)[1]))
        object[[.plotHoverInfo]] <- TRUE
        object[[.legendPointSize]] <- 1
        object[[.plotLabelCenters]] <- FALSE
        object[[.plotLabelCentersBy]] <- NA_character_
        object[[.plotLabelCentersColor]] <- "black"
        object[[.plotCustomLabels]] <- FALSE
        object[[.plotCustomLabelsText]] <- NA_character_
    }
    # nocov end

    object
})
