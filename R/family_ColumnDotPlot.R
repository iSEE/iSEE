#' Column dot plot panel family 
#' 
#' The column dot plot panel family covers all plot panel types where each column of the \linkS4class{SummarizedExperiment} is represented by a point.
#' This family is represented by the \code{ColumnDotPlot} virtual class, which has a number of concrete subclasses (e.g., \linkS4class{RedDimPlot}) to direct the construction of specific plots.
#' We provide a number of useful methods on this virtual class to make it easier for developers to define their own subclasses.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for visualization and point selection.
#' More details to be added.
#'
#' @section Output plot:
#' \code{\link{.createOutputElement}} will return the output of \code{\link{plotOutput}} with two-dimensional brushing.
#' More details to be added.
#' 
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases .defineParamInterface,ColumnDotPlot-method
#' .createParamObservers,ColumnDotPlot-method
#' .defineOutputElement,ColumnDotPlot-method
#' .createRenderedOutput,ColumnDotPlot-method
#' @name ColumnDotPlot
NULL

#' @export
setMethod(".defineParamInterface", "ColumnDotPlot", function(x, id, param_choices, se, active_panels) {
    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)
    tab_by_col <- c(.noSelection, link_sources$col_tab)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    mode <- .getEncodedName(x)
    list(
        .create_visual_box_for_column_plots(mode, id, param_choices, tab_by_row, tab_by_col, se), 
        .create_selection_param_box(mode, id, param_choices, col_selectable, "column") 
    )
})

# TODO: move to a better place.
.dot_parameter_nonfundamental <- c(
    .colorByDefaultColor, .selectColor, .selectTransAlpha,
    .plotPointSize, .plotPointAlpha, .plotFontSize, .plotLegendPosition,
    .plotPointDownsample, .plotPointSampleRes, .contourAddTitle,
    .contourColor
)

#' @export
setMethod(".createParamObservers", "ColumnDotPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    .define_box_observers(mode, id, c(.visualParamBoxOpen, .selectParamBoxOpen), input, pObjects)

    .define_visual_parameter_choice_observer(mode, id, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=c(.facetByRow, .facetByColumn, .facetRowsByColData, .facetColumnsByColData),
        nonfundamental=c(.general_nonfundamental,
            .colorByColData, .colorByFeatNameAssay, .shapeByField,
            .shapeByColData, .sizeByField, .sizeByColData, .colorBySampNameColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects) 

    .define_dim_name_observer(mode, id,
        name_field=.colorByFeatName,
        in_use_field=.colorByField,
        in_use_value=.colorByFeatNameTitle,
        table_field=.colorByRowTable,
        choices=feature_choices)

    .define_dim_name_observer(mode, id,
        name_field=.colorBySampName,
        in_use_field=.colorByField,
        in_use_value=.colorBySampNameTitle,
        table_field=.colorByColTable,
        choices=sample_choices)
})

#' @export
setMethod(".defineOutputElement", "ColumnDotPlot", function(x, id, height) {
    mode <- .getEncodedName(x)
    .create_plot_ui(mode, id, brush_direction="xy", 
        height=height,
        brush_fill=brush_fill_color[mode],
        brush_stroke=brush_stroke_color[mode]
    )
})

#' @export
setMethod(".createRenderedOutput", "ColumnDotPlot", function(x, id, se, colormap, output, pObjects, rObjects) {
# TODO: move colormap INSIDE se's metadata.
    .define_plot_output(.getEncodedName(x), id, FUN=.getPlottingFunction(x), selectable=TRUE, 
        se=se, colormap=colormap, output=output, pObjects=pObjects, rObjects=rObjects)
})
