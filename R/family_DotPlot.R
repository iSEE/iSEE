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
#' Defaults to \code{"black"} in \code{\link{getPanelDefault}}.
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
#' Defaults to \code{FALSE} in \code{\link{getPanelDefault}}.
#' \item \code{ColorBySampleDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source when coloring by feature.
#' Defaults to \code{FALSE} in \code{\link{getPanelDefault}}.
#' \item \code{SelectionAlpha}, a numeric scalar in [0, 1] specifying the transparency to use for non-selected points.
#' Defaults to 0.1 in \code{\link{getPanelDefault}}.
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
#' \item \code{FacetRowBy}, a string indicating what to use for creating row facets.
#' For \linkS4class{RowDotPlot}s, this should be one of \code{"None"}, \code{"Row data"} or \code{"Row selection"}. 
#' For \linkS4class{ColumnDotPlot}s, this should be one of \code{"None"}, \code{"Column data"} or \code{"Column selection"}. 
#' Defaults to \code{"None"}, i.e., no row faceting.
#' \item \code{FacetByColumn}, a string indicating what to use for creating column facets.
#' For \linkS4class{RowDotPlot}s, this should be one of \code{"None"}, \code{"Row data"} or \code{"Row selection"}. 
#' For \linkS4class{ColumnDotPlot}s, this should be one of \code{"None"}, \code{"Column data"} or \code{"Column selection"}. 
#' Defaults to \code{"None"}, i.e., no column faceting.
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
#' The following slots control interactions with the plot image:
#' \itemize{
#' \item \code{ZoomData}, a named numeric vector of plot coordinates with \code{"xmin"}, \code{"xmax"}, \code{"ymin"} and \code{"ymax"} elements parameterizing the zoom boundaries.
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
#' The following slot controls whether the aspect ratio is fixed to 1 or not:
#' \itemize{
#' \item \code{FixAspectRatio}, logical scalar indicating whether the aspect ratio of a scatter plot should be fixed to 1. 
#' Defaults to \code{FALSE}.
#' }
#'
#' The following slot controls whether the violin boundaries are plotted or not:
#' \itemize{
#' \item \code{ViolinAdd}, logical scalar indicating whether the violin boundaries should be plotted. 
#' Defaults to \code{TRUE}.
#' }
#'
#' The following slots control the general appearance of the points.
#' \itemize{
#' \item \code{PointSize}, positive numeric scalar specifying the relative size of the points.
#' Defaults to 1.
#' \item \code{PointAlpha}, non-negative numeric scalar specifying the transparency of the points.
#' Defaults to 1, i.e., not transparent.
#' \item \code{Downsample}, logical scalar indicating whether to downsample points for faster plotting.
#' Defaults to \code{FALSE} in \code{\link{getPanelDefault}}.
#' \item \code{DownsampleResolution}, numeric scalar specifying the resolution of the downsampling grid (see \code{?\link{subsetPointsByGrid}}) if \code{Downsample=TRUE}.
#' Larger values correspond to reduced downsampling at the cost of plotting speed.
#' Defaults to 200 in \code{\link{getPanelDefault}}.
#' }
#'
#' The following slots refer to general plotting parameters:
#' \itemize{
#' \item \code{FontSize}, positive numeric scalar specifying the relative font size.
#' Defaults to 1 in \code{\link{getPanelDefault}}.
#' \item \code{PointSize}, positive numeric scalar specifying the relative point size.
#' Defaults to 1 in \code{\link{getPanelDefault}}.
#' \item \code{LegendPosition}, string specifying the position of the legend on the plot.
#' Defaults to \code{"Bottom"} in \code{\link{getPanelDefault}}.
#' The other valid choice is \code{"Right"}.
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
#' \item \code{\link{.allowableColorByDataChoices}(x, se)} returns a character vector containing all atomic variables in the relevant \code{*Data} dimension.
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
#' updateObject,DotPlot-method
#' .defineOutput,DotPlot-method
#' .generateOutput,DotPlot-method
#' .generateDotPlot,DotPlot-method
#' .renderOutput,DotPlot-method
#' .exportOutput,DotPlot-method
#' .refineParameters,DotPlot-method
#' .cacheCommonInfo,DotPlot-method
#' .createObservers,DotPlot-method
#' .hideInterface,DotPlot-method
#' .multiSelectionActive,DotPlot-method
#' .multiSelectionCommands,DotPlot-method
#' .multiSelectionClear,DotPlot-method
#' .multiSelectionDimension,DotPlot-method
#' .isBrushable,DotPlot-method
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
#' .allowableColorByDataChoices,DotPlot-method
#' .definePanelTour,DotPlot-method
#' updateObject,DotPlot-method
#' [[,DotPlot-method
#' [[,DotPlot,ANY,ANY-method
#' [[<-,DotPlot-method
#' [[<-,DotPlot,ANY,ANY-method
NULL

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "DotPlot", function(.Object, ...) {
    args <- list(...)
    args <- .emptyDefault(args, .facetRow, .facetByNothingTitle)
    args <- .emptyDefault(args, .facetColumn, .facetByNothingTitle)

    args <- .emptyDefault(args, .colorByField, .colorByNothingTitle)
    args <- .emptyDefault(args, .colorByDefaultColor, getPanelDefault(.colorByDefaultColor))

    args <- .emptyDefault(args, .colorByFeatName, NA_character_)
    args <- .emptyDefault(args, .colorByFeatDynamic, getPanelDefault("SingleSelectionDynamicSource"))
    args <- .emptyDefault(args, .colorByRowTable, .noSelection)

    args <- .emptyDefault(args, .colorBySampName, NA_character_)
    args <- .emptyDefault(args, .colorBySampDynamic, getPanelDefault("SingleSelectionDynamicSource"))
    args <- .emptyDefault(args, .colorByColTable, .noSelection)

    args <- .emptyDefault(args, .shapeByField, .shapeByNothingTitle)

    args <- .emptyDefault(args, .sizeByField, .sizeByNothingTitle)

    args <- .emptyDefault(args, .selectTransAlpha, getPanelDefault(.selectTransAlpha))

    args <- .emptyDefault(args, .visualParamBoxOpen, FALSE)
    args <- .emptyDefault(args, .visualParamChoice, .visualParamChoiceColorTitle)

    args <- .emptyDefault(args, .contourAdd, FALSE)
    args <- .emptyDefault(args, .contourColor, getPanelDefault(.contourColor))
    
    args <- .emptyDefault(args, .fixAspectRatio, FALSE)

    args <- .emptyDefault(args, .violinAdd, TRUE)
    
    args <- .emptyDefault(args, .plotPointSize, getPanelDefault(.plotPointSize))
    args <- .emptyDefault(args, .plotPointAlpha, getPanelDefault(.plotPointAlpha))
    args <- .emptyDefault(args, .plotPointDownsample, getPanelDefault(.plotPointDownsample))
    args <- .emptyDefault(args, .plotPointSampleRes, getPanelDefault(.plotPointSampleRes))

    args <- .emptyDefault(args, .plotCustomLabels, FALSE)
    args <- .emptyDefault(args, .plotCustomLabelsText, NA_character_)
    args <- .emptyDefault(args, .plotFontSize, getPanelDefault(.plotFontSize))
    args <- .emptyDefault(args, .legendPointSize, getPanelDefault(.legendPointSize))
    args <- .emptyDefault(args, .plotLegendPosition, getPanelDefault(.plotLegendPosition))

    args <- .emptyDefault(args, .plotHoverInfo, TRUE)

    args <- .emptyDefault(args, .plotLabelCenters, FALSE)
    args <- .emptyDefault(args, .plotLabelCentersBy, NA_character_)
    args <- .emptyDefault(args, .plotLabelCentersColor, "black")

    do.call(callNextMethod, c(list(.Object), args))
})

#' @importFrom S4Vectors setValidity2
setValidity2("DotPlot", function(object) {
    msg <- character(0)

    msg <- .validLogicalError(msg, object,
        c(.plotCustomLabels, .visualParamBoxOpen, .contourAdd, .plotPointDownsample,
            .plotHoverInfo,
            .plotLabelCenters, .fixAspectRatio, .violinAdd
        ))

    msg <- .singleStringError(msg, object,
        c(.plotCustomLabelsText, .colorByField, .colorByFeatName, .colorByRowTable, .colorBySampName, .colorByColTable,
            .shapeByField,
            .sizeByField,
            .plotLabelCentersBy
        ))

    msg <- .validStringError(msg, object,
        c(.colorByDefaultColor,
            .contourColor,
            .plotLabelCentersColor
        ))

    facet_info <- .getDotPlotFacetConstants(object) 
    for (field in c(.facetRow, .facetColumn)) {
        msg <- .allowableChoiceError(msg, object, field,
            c(.facetByNothingTitle, facet_info$metadata$title, facet_info$selections$title))
    }

    msg <- .validNumberError(msg, object, .selectTransAlpha, lower=0, upper=1)

    msg <- .multipleChoiceError(msg, object, .visualParamChoice,
        c(.visualParamChoiceColorTitle, .visualParamChoiceShapeTitle, .visualParamChoiceSizeTitle, .visualParamChoicePointTitle,
            .visualParamChoiceFacetTitle, .visualParamChoiceTextTitle, .visualParamChoiceOtherTitle))

    msg <- .validNumberError(msg, object, .plotPointSize, lower=0, upper=Inf)

    msg <- .validNumberError(msg, object, .plotPointAlpha, lower=0, upper=1)

    msg <- .validNumberError(msg, object, .plotPointSampleRes, lower=1, upper=Inf)

    msg <- .validNumberError(msg, object, .plotFontSize, lower=0, upper=Inf)

    msg <- .validNumberError(msg, object, .legendPointSize, lower=0, upper=Inf)

    msg <- .allowableChoiceError(msg, object, .plotLegendPosition,
        c(.plotLegendRightTitle, .plotLegendBottomTitle))

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("[[", "DotPlot", function(x, i, j, ...) {
    if (i %in% c("FacetByRow", "FacetByColumn")) {
        x <- updateObject(x, check=FALSE)

        facet_info <- .getDotPlotFacetConstants(x)
        row_field <- facet_info$metadata$row_field
        col_field <- facet_info$metadata$column_field

        if (i=="FacetByRow") {
            dim <- .facetRow
            dim_field <- row_field
        } else {
            dim <- .facetColumn
            dim_field <- col_field
        }

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.\nUse <%s>[['%s']] and/or <%s>[['%s']] instead.", 
            cname, i, cname, dim, cname, dim_field))

        title <- facet_info$metadata$title
        if (slot(x, dim)!=title) {
            .noSelection
        } else {
            slot(x, dim_field)
        }
    } else {
        callNextMethod()
    }
})

#' @export
setReplaceMethod("[[", "DotPlot", function(x, i, j, ..., value) {
    if (i %in% c("FacetByRow", "FacetByColumn")) {
        x <- updateObject(x, check=FALSE)

        facet_info <- .getDotPlotFacetConstants(x)
        row_field <- facet_info$metadata$row_field
        col_field <- facet_info$metadata$column_field

        if (i=="FacetByRow") {
            dim <- .facetRow
            dim_field <- row_field
        } else {
            dim <- .facetColumn
            dim_field <- col_field
        }

        cname <- class(x)[1]
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.\nSet <%s>[['%s']] and/or <%s>[['%s']] instead.", 
            cname, i, cname, dim, cname, dim_field))

        title <- facet_info$metadata$title
        if (value==.noSelection) {
            slot(x, dim) <- .facetByNothingTitle
        } else {
            slot(x, dim) <- title
            slot(x, dim_field) <- value
        }
        x
    } else {
        callNextMethod()
    }
})

#' @export
#' @importFrom methods callNextMethod
#' @importFrom SummarizedExperiment assayNames
setMethod(".cacheCommonInfo", "DotPlot", function(x, se) {
    if (!is.null(.getCachedCommonInfo(se, "DotPlot"))) {
        return(se)
    }

    se <- callNextMethod()

    .setCachedCommonInfo(se, "DotPlot", valid.assay.names=assayNames(se))
})

#' @export
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "DotPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    x <- .replaceMissingWithFirst(x, .colorByFeatName, rownames(se))
    x <- .replaceMissingWithFirst(x, .colorBySampName, colnames(se))
    x <- .replaceMissingWithFirst(x, .plotLabelCentersBy, .getDiscreteMetadataChoices(x, se))

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
        fields=c(.facetRow, .facetColumn),
        input=input, pObjects=pObjects, rObjects=rObjects)

    .createUnprotectedParameterObservers(plot_name,
        fields=c(
            .colorByDefaultColor, .selectTransAlpha,
            .shapeByField, .sizeByField,
            .plotPointSize, .plotPointAlpha, .plotFontSize, .legendPointSize, .plotLegendPosition,
            .plotPointDownsample, .plotPointSampleRes, .contourAdd,
            .contourColor, .fixAspectRatio, .violinAdd, .plotCustomLabels, .plotHoverInfo,
            .plotLabelCenters, .plotLabelCentersBy, .plotLabelCentersColor),
        input=input, pObjects=pObjects, rObjects=rObjects)

    # Filling the plot interaction observers:
    .create_brush_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_lasso_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_zoom_observer(plot_name, input=input, session=session,
        pObjects=pObjects, rObjects=rObjects)

    .create_hover_observer(plot_name, se, input=input, session=session, pObjects=pObjects)

    .createCustomDimnamesModalObservers(plot_name, .plotCustomLabelsText, .dimnamesModalOpen,
        se, input=input, session=session, pObjects=pObjects, rObjects=rObjects, 
        source_type=plot_dimension)
})

# Interface ----

#' @export
setMethod(".defineInterface", "DotPlot", function(x, se, select_info) {
    out <- callNextMethod()
    c(
        out[1], # data parameters box
        list(.create_visual_box(x, se, select_info$single)),
        out[-1] # selection parameters box
    )
})

#' @export
setMethod(".allowableColorByDataChoices", "DotPlot", function(x, se) {
    .getMetadataChoices(x, se)
})

#' @export
setMethod(".defineVisualColorInterface", "DotPlot", function(x, se, select_info) {
    all_assays <- .getCachedCommonInfo(se, "DotPlot")$valid.assay.names

    plot_name <- .getEncodedName(x)
    colorby_field <- paste0(plot_name, "_", .colorByField)

    colorby <- .getDotPlotColorConstants(x)
    mydim_single <- .singleSelectionDimension(x)
    otherdim_single <- setdiff(c("feature", "sample"), mydim_single)
    mydim_choices <- select_info[[mydim_single]]
    otherdim_choices <- select_info[[otherdim_single]]

    color_choices <- .defineDotPlotColorChoices(x, se)

    .addSpecificTour(class(x), .colorByField, .getDotPlotColorHelp(x, color_choices))

    .addSpecificTour(class(x), .selectTransAlpha, {
        mdim <- .multiSelectionDimension(x)
        function(plot_name) {
            data.frame(
                rbind(
                    c(
                        element=paste0("#", plot_name, "_", .selectTransAlpha, .slider_extra),
                        intro=sprintf("When we make a multiple %s selection on another panel, 
                                       we can transmit that selection to the current panel. 
                                       When we do so, we can choose to highlight the selected points on this panel 
                                       by making all the <em>unselected</em> points a little transparent. 
                                       This slider controls the transparency level for those unselected points.", mdim)
                    )
                )
            )
        }
    })

    # Actually creating the UI.
    tagList(
        hr(),
        .radioButtons.iSEE(x, .colorByField, 
            label="Color by:",
            inline=TRUE,
            choices=.defineDotPlotColorChoices(x, se),
            selected=slot(x, .colorByField)
        ),
        .conditionalOnRadio(
            colorby_field, .colorByNothingTitle,
                colourInput(
                    paste0(plot_name, "_", .colorByDefaultColor), label=NULL,
                    value=slot(x, .colorByDefaultColor))
        ),
        .conditionalOnRadio(
            colorby_field, colorby$metadata$title,
            selectInput(
                paste0(plot_name, "_", colorby$metadata$field), label=NULL,
                choices=.allowableColorByDataChoices(x, se), 
                selected=x[[colorby$metadata$field]])
        ),
        .conditionalOnRadio(colorby_field, colorby$name$title,
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
                value=x[[colorby$name$dynamic]]),
        ),
        .conditionalOnRadio(colorby_field, colorby$assay$title,
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
        ),
        .sliderInput.iSEE(x, .selectTransAlpha,
            label="Unselected point opacity:", 
            min=0, max=1, value=slot(x, .selectTransAlpha)
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

        .addSpecificTour(class(x)[1], .shapeByField, {
            mdim <- .multiSelectionDimension(x)
            shape_meta_field <- shapeby$metadata$field
            function(plot_name) {
                data.frame(
                    rbind(
                        c(
                            element=paste0("#", plot_name, "_", .shapeByField),
                            intro=sprintf("We can make the shape of each point depend on the value of a categorical %s data field. 
                                           For example, if you were to <strong>select <em>%s data</em></strong>...", mdim, mdim)
                        ),
                        c(
                            element=paste0("#", plot_name, "_", shape_meta_field, " + .selectize-control"),
                            intro="... we can then choose a variable for shaping each point in the plot. 
                                   Note that there are only a limited number of unique shapes, 
                                   so past a certain number of levels, the plot will just give up."
                        )
                    )
                )
            }
        })

        tagList(
            hr(),
            .radioButtons.iSEE(x, .shapeByField,
                label="Shape by:",
                inline=TRUE,
                choices=c(.shapeByNothingTitle, shapeby$metadata$title),
                selected=slot(x, .shapeByField)
            ),
            .conditionalOnRadio(
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

    pointsize_field <- paste0(plot_name, "_", .plotPointSize)
    common_ui <- .numericInput.iSEE(x, .plotPointSize,
        label="Point size:", min=0, value=slot(x, .plotPointSize))

    .addSpecificTour(class(x)[1], .plotPointSize, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", pointsize_field),
                    intro="This controls the size of all points... nothing much more to be said."
                )
            )
        )
    })

    if (length(numeric_covariates)) {
        .addSpecificTour(class(x)[1], .sizeByField, {
            mdim <- .multiSelectionDimension(x)
            size_meta_field <- sizeby$metadata$field
            function(plot_name) {
                data.frame(
                    rbind(
                        c(
                            element=paste0("#", sizeby_field),
                            intro=sprintf("We can make the size of each point depend on the value of a numeric %s data field. 
                                           For example, if you were to <strong>select <em>%s data</em></strong>...", mdim, mdim)
                        ),
                        c(
                            element=paste0("#", plot_name, "_", size_meta_field, " + .selectize-control"),
                            intro="... we can then choose a variable for determining the size of each point in the plot."
                        )
                    )
                )
            }
        })

        tagList(
            hr(),
            .radioButtons.iSEE(x, .sizeByField,
                label="Size by:",
                inline=TRUE,
                choices=c(.sizeByNothingTitle, sizeby$metadata$title),
                selected=slot(x, .sizeByField)
            ),
            .conditionalOnRadio(
                sizeby_field, .sizeByNothingTitle,
                common_ui
            ),
            .conditionalOnRadio(
                sizeby_field, sizeby$metadata$title,
                selectInput(paste0(plot_name, "_", sizeby$metadata$field), label=NULL,
                    choices=numeric_covariates, selected=x[[sizeby$metadata$field]])
            )
        )
    } else {
        common_ui
    }
})

#' @export
setMethod(".defineVisualPointInterface", "DotPlot", function(x, se) {
    plot_name <- .getEncodedName(x)
    ds_id <- paste0(plot_name, "_", .plotPointDownsample)

    .addSpecificTour(class(x)[1], .plotPointAlpha, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .plotPointAlpha, .slider_extra),
                    intro="This controls the opacity of all points, with 0 being fully transparent and 1 being fully opaque.
                    
                    Note that, unlike the <em>Unselected point opacity</em> option,
                    this option applies regardless of whether a point is selected or not."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .plotPointDownsample, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .plotPointDownsample),
                    intro="For larger datasets, we downsample points in a density-dependent manner.
                    This basically involves removing points that are covered by other points,
                    thus reducing the number of points and speeding up the plot rendering.
                    To demonstrate, <strong>check this box</strong>."
                ),
                c(
                    element=paste0("#", plot_name, "_", .plotPointSampleRes),
                    intro="The sampling resolution determines how many points we remove.
                    For example, if we have a sampling resolution of 100, this means that we
                    cut up the plot into a 100-by-100 grid and keep only one point per grid cell.
                    Higher resolutions retain more points at the cost of rendering time."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .contourAdd, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .contourAdd),
                    intro="For scatter plots, we can add a contour representing the density of points.
                    For all other plots, this has no effect.
                    If we're on a scatter plot, you can <strong>check this box</strong>."
                ),
                c(
                    element=paste0("#", plot_name, "_", .contourColor),
                    intro="And you can change the color of the contour lines."
                )
            )
        )
    })
    
    .addSpecificTour(class(x)[1], .fixAspectRatio, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .fixAspectRatio),
                    intro="For scatter plots, we can fix the aspect ratio to 1 by checking this box.
                    For all other plots, this has no effect."
                )
            )
        )
    })
    
    .addSpecificTour(class(x)[1], .violinAdd, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .violinAdd),
                    intro="For violin plots, we can decide whether the violin boundaries should be plotted or not.
                    For all other plots, this has no effect."
                )
            )
        )
    })

    tagList(
        hr(),
        .sliderInput.iSEE(x, .plotPointAlpha, label="Point opacity:", 
            min=0.1, max=1, value=slot(x, .plotPointAlpha)),
        hr(),
        .checkboxInput.iSEE(x, .plotPointDownsample, 
            label="Downsample points for speed",
            value=slot(x, .plotPointDownsample)),
        .conditionalOnCheckSolo(
            ds_id, on_select=TRUE,
            numericInput(
                paste0(plot_name, "_", .plotPointSampleRes), label="Sampling resolution:",
                min=1, value=slot(x, .plotPointSampleRes))
        ),
        .checkboxInput.iSEE(x, .contourAdd,
            label="Add contour (scatter only)",
            value=slot(x, .contourAdd)),
        .conditionalOnCheckSolo(
            paste0(plot_name, "_", .contourAdd),
            on_select=TRUE,
            colourInput(
                paste0(plot_name, "_", .contourColor), label=NULL,
                value=slot(x, .contourColor))),
        .checkboxInput.iSEE(x, .fixAspectRatio,
                            label="Fix aspect ratio to 1 (scatter only)",
                            value=slot(x, .fixAspectRatio)),
        .checkboxInput.iSEE(x, .violinAdd,
                            label="Display violin boundaries (violin only)",
                            value=slot(x, .violinAdd))
    )
})

#' @export
#' @importFrom shiny tagList selectInput radioButtons
setMethod(".defineVisualFacetInterface", "DotPlot", function(x, se) {
    covariates <- .getDiscreteMetadataChoices(x, se)
    plot_name <- .getEncodedName(x)
    rowId <- paste0(plot_name, "_", .facetRow)
    columnId <- paste0(plot_name, "_", .facetColumn)

    facet_info <- .getDotPlotFacetConstants(x)
    title_choices <- .facetByNothingTitle
    if (length(covariates)) {
        title_choices <- c(title_choices, facet_info$metadata$title)
    }
    title_choices <- c(title_choices, facet_info$selections$title)

    things <- list(
        row=c(.facetRow, facet_info$metadata$row_field),
        column=c(.facetColumn, facet_info$metadata$column_field)
    )
    ui <- list()

    for (dim in names(things)) {
        fields <- things[[dim]]
        use_field <- fields[1]
        choice_field <- fields[2]

        local({
            dim0 <- dim
            use_field0 <- use_field
            choice_field0 <- choice_field
            .addSpecificTour(class(x)[1], use_field0, {
                mdim <- .multiSelectionDimension(x)
                function(plot_name) {
                    if (length(covariates)) {

                    }
                    data.frame(
                        rbind(
                            c(
                                element=paste0("#", plot_name, "_", use_field0),
                                intro=sprintf("We can choose split the points into multiple %s facets.
                                Points are allocated to the subplots according to the value of the factor used for faceting.", dim0)
                            ),

                            if (length(covariates)) {
                                rbind(
                                    c(
                                        element=paste0("#", plot_name, "_", use_field0),
                                        intro=sprintf("If we were to <strong>select <em>%s</em></strong>...", facet_info$metadata$title)
                                    ),
                                    c(
                                        element=paste0("#", plot_name, "_", choice_field0, " + .selectize-control"),
                                        intro=sprintf("... we can choose the <code>%sData</code> variable to use for faceting.", substr(mdim, 1, 3))
                                    )
                                )
                            },

                            c(
                                element=paste0("#", plot_name, "_", use_field0),
                                intro=sprintf("If we were to <strong>select <em>%s</em></strong>, 
                                the factor is defined based on the multiple %s selections transmitted from another panel.
                                All points corresponding to %ss in the active selection of another panel are assigned to one facet;
                                all points in each saved selection of another panel are assigned to another facet;
                                and all points not in any selection are assigned to yet another facet.
                                Points that are present in multiple selections also get assigned to a separate facet.",
                                    facet_info$selections$title, mdim, mdim)
                            )
                        )
                    )
                }
            })
        })

        ui <- c(ui, list(
            .radioButtons.iSEE(x, use_field, 
                label=sprintf("Facet as %s:", dim),
                choices=title_choices, 
                selected=slot(x, use_field), 
                inline=TRUE),

            if (length(covariates)) {
                .conditionalOnRadio(paste0(plot_name, "_", use_field), 
                    facet_info$metadata$title, 
                    selectInput(paste0(plot_name, "_", choice_field), 
                        label=NULL, 
                        choices=covariates, 
                        selected=slot(x, choice_field))
                )
            }
        ))
    }

    do.call(tagList, c(list(hr()), ui))
})

#' @export
#' @importFrom shiny tagList
setMethod(".defineVisualTextInterface", "DotPlot", function(x, se) {
    plot_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    .addSpecificTour(class(x)[1], .plotCustomLabels, {
        mdim <- .multiSelectionDimension(x)
        function(plot_name) {
            data.frame(
                rbind(
                    c(
                        element=paste0("#", plot_name, "_", .plotCustomLabels), 
                        intro=sprintf("Users can show the names of certain %ss alongside their locations on the plot. This is done by <strong>checking the highlighted box</strong>...", mdim)
                    ),
                    c(
                        element=paste0("#", plot_name, "_", .dimnamesModalOpen),
                        intro=sprintf("... and then clicking on this button to open a modal in which users can enter the names of the %ss of interest. All points named in this manner will have their names appear next to their coordinates on the plot.<br/><br/>(By default, we don't name all points as this may result in too many names for large numbers of points.)", mdim)
                    )
                )
            )
        }
    })

    sdim <- .singleSelectionDimension(x)
    .addSpecificTour(class(x)[1], .plotHoverInfo, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .plotHoverInfo),
                    intro=sprintf("If this is checked, we show the name of the %s when we hover over the corresponding point in the plot.", sdim)
                )
            )
        )
    })

    ui <- list(
        hr(),
        .checkboxInput.iSEE(x, .plotHoverInfo,
            label=sprintf("Show %s details on hover", sdim),
            value=slot(x, .plotHoverInfo)),
        hr(),
        .checkboxInput.iSEE(x, .plotCustomLabels,
            label=sprintf("Label custom %ss", sdim),
            value=slot(x, .plotCustomLabels)),
        .conditionalOnCheckSolo(
            .input_FUN(.plotCustomLabels),
            on_select=TRUE,
            actionButton(.input_FUN(.dimnamesModalOpen),
                label=sprintf("Edit %s names", sdim))
        )
    )

    discrete.choices <- .getDiscreteMetadataChoices(x, se)
    if (length(discrete.choices)) {
        .addSpecificTour(class(x)[1], .plotLabelCenters, {
            mdim <- .multiSelectionDimension(x)
            function(plot_name) {
                data.frame(
                    rbind(
                        c(
                            element=paste0("#", plot_name, "_", .plotLabelCenters),
                            intro="In certain applications, we may have a factor that defines groups of points in the plot. 
                            A typical example would be that a factor that holds cluster identity on a Reduced Dimension Plot.
                            We can then use that factor to annotate the plot by putting the group label at the center of the group's points.
                            This can be done by <strong>checking this box</strong>..."
                        ),
                        c(
                            element=paste0("#", plot_name, "_", .plotLabelCentersBy, " + .selectize-control"),
                            intro=sprintf("... and choosing a categorical factor from the <code>%sData</code> to label points with.
                            Of course, this really only makes sense for factors that are somehow associated with the plot coordinates.", substr(mdim, 1, 3))
                        )
                    )
                )
            }
        })

        ui <- c(ui, 
            list(
                hr(),
                .checkboxInput.iSEE(x, .plotLabelCenters,
                    label="Label centers",
                    value=slot(x, .plotLabelCenters)),
                .conditionalOnCheckSolo(
                    .input_FUN(.plotLabelCenters),
                    on_select=TRUE,
                    selectInput(.input_FUN(.plotLabelCentersBy),
                        label="Label centers:",
                        choices=discrete.choices,
                        selected=slot(x, .plotLabelCentersBy)),
                    colourInput(.input_FUN(.plotLabelCentersColor),
                        label=NULL,
                        value=slot(x, .plotLabelCentersColor))
                )
            )
        )
    }

    .addSpecificTour(class(x)[1], .plotFontSize, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .plotFontSize),
                    intro="Changes the font size, nothing much more to say here."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .legendPointSize, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .legendPointSize),
                    intro="Changes the size of the points in the legend.
                    To be honest, I can't remember why we put this in here,
                    but someone must have asked for it... so here it is."
                )
            )
        )
    })

    .addSpecificTour(class(x)[1], .plotLegendPosition, function(plot_name) {
        data.frame(
            rbind(
                c(
                    element=paste0("#", plot_name, "_", .plotLegendPosition),
                    intro="Changes the position of the legend on the plot, if any legend exists.
                    On the bottom, on the right; the choice is yours."
                )
            )
        )
    })

    ui <- c(ui,
        list(
            hr(),
            .numericInput.iSEE(x, .plotFontSize,
                label="Font size:",
                min=0, value=slot(x, .plotFontSize)),
            .numericInput.iSEE(x, .legendPointSize,
                label="Legend point size:",
                min=0, value=slot(x, .legendPointSize)),
            .radioButtons.iSEE(x, .plotLegendPosition,
                label="Legend position:", inline=TRUE,
                choices=c(.plotLegendBottomTitle, .plotLegendRightTitle),
                selected=slot(x, .plotLegendPosition))
        )
    )

    do.call(tagList, ui)
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
        height=slot(x, .organizationHeight),
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
    pdf(newpath, width=slot(x, .organizationHeight)/75, height=slot(x, .organizationWidth)*2)
    print(contents$plot)
    dev.off()

    newpath
})

#' @export
setMethod(".multiSelectionClear", "DotPlot", function(x) {
    slot(x, .brushData) <- list()
    x
})

#' @export
setMethod(".multiSelectionActive", "DotPlot", function(x) {
    to_store <- slot(x, .brushData)
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
        brush_val <- slot(x, .brushData)
    } else {
        brush_val <- slot(x, .multiSelectHistory)[[index]]
    }

    if (.is_brush(brush_val)) {
        "selected <- rownames(shiny::brushedPoints(contents, select));"
    } else {
        "selected <- rownames(iSEE::lassoPoints(contents, select));"
    }
})

#' @export
setMethod(".isBrushable", "DotPlot", function(x) TRUE)

#' @export
setMethod(".singleSelectionValue", "DotPlot", function(x, contents) {
    plot_name <- .getEncodedName(x)
    chosen <- .get_brushed_points(contents, slot(x, .brushData))
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
    plot_env$colormap <- .get_colormap(se)

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

###############################################################################
# Documentation

#' @export
setMethod(".definePanelTour", "DotPlot", function(x) {
    mdim <- .multiSelectionDimension(x)
    rbind(
        .addTourStep(x, .visualParamBoxOpen,  "The <i>Visual parameters</i> box contains parameters related to visual aspects like the color, shape, size and so on.<br/><br/><strong>Action:</strong> click on the header of this box to see the available options."),
        .addTourStep(x, .visualParamChoice, "There are a lot of options so not all of them are shown by default. More settings are available by checking some of the boxes here; conversely, options can be hidden by unchecking some of these boxes.<br/><br/>Most of these parameters here are fairly self-explanatory and can be explored at leisure."),
        callNextMethod(),
        c(paste0("#", .getEncodedName(x)), sprintf("At the other end of the spectrum, brushing or creating a lasso on this plot will create a selection of multiple %ss, to be transmitted to other panels that choose this one as their selection source.<br/><br/>Drag-and-dropping will create a rectangular brush while a single click will lay down a lasso waypoint for non-rectangular selections.<br/><br/>Brushes and lassos can also be used to transmit single %s selections in which case one %s is arbitrarily chosen from the selection.", mdim, mdim, mdim))
    )
})

###############################################################################
# Back compatibility

#' @export
#' @importFrom BiocGenerics updateObject
setMethod("updateObject", "DotPlot", function(object, ..., verbose=FALSE) {
    if (!.is_latest_version(object)) {
        # nocov start

        # Do this before 'callNextMethod()', which fills in the Restrict.
        update.2.1 <- is(try(slot(object, .plotHoverInfo), silent=TRUE), "try-error")
        update.2.3 <- is(try(slot(object, .facetRow), silent=TRUE), "try-error")

        # NOTE: it is crucial that updateObject does not contain '[[' or '[[<-'
        # calls, lest we get sucked into infinite recursion with the calls to
        # 'updateObject' from '[['.
        object <- callNextMethod()

        # Backwards compatibility for new slots (added 3.12, preceding versioning information).
        if (update.2.1) {
            .Deprecated(msg=sprintf("detected outdated '%s' instance, run 'updateObject(<%s>)'", class(object)[1], class(object)[1]))
            slot(object, .plotHoverInfo) <- TRUE
            slot(object, .legendPointSize) <- 1
            slot(object, .plotLabelCenters) <- FALSE
            slot(object, .plotLabelCentersBy) <- NA_character_
            slot(object, .plotLabelCentersColor) <- "black"
            slot(object, .plotCustomLabels) <- FALSE
            slot(object, .plotCustomLabelsText) <- NA_character_
        }

        # Backwards compatibility for new slots (added 3.13, preceding versioning information).
        if (update.2.3) {
            .Deprecated(msg=sprintf("detected outdated '%s' instance, run 'updateObject(<%s>)'", class(object)[1], class(object)[1]))

            facet_info <- .getDotPlotFacetConstants(object)
            row_field <- facet_info$metadata$row_field
            col_field <- facet_info$metadata$column_field
            title <- facet_info$metadata$title

            oldr <- object@FacetByRow
            if (oldr==.noSelection) {
                slot(object, .facetRow) <- .facetByNothingTitle
                slot(object, row_field) <- NA_character_
            } else {
                slot(object, .facetRow) <- title
                slot(object, row_field) <- oldr
            }

            oldc <- object@FacetByColumn
            if (oldc==.noSelection) {
                slot(object, .facetColumn) <- .facetByNothingTitle
                slot(object, col_field) <- NA_character_
            } else {
                slot(object, .facetColumn) <- title
                slot(object, col_field) <- oldc
            }
        }
        # nocov end
    }

    object
})
