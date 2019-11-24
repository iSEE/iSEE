#' Row dot plot panel family 
#' 
#' The row dot plot panel family covers all plot panel types where each row of the \linkS4class{SummarizedExperiment} is represented by a point.
#' This family is represented by the \code{RowDotPlot} virtual class, which has a number of concrete subclasses (e.g., \linkS4class{SampAssayPlot}) to direct the construction of specific plots.
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
#' @aliases .defineParamInterface,RowDotPlot-method
#' .createParamObservers,RowDotPlot-method
#' @name RowDotPlot
NULL

#' @export
setMethod(".defineParamInterface", "RowDotPlot", function(x, id, param_choices, se, active_panels) {
    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)
    tab_by_col <- c(.noSelection, link_sources$col_tab)
    row_selectable <- c(.noSelection, link_sources$row_plot)

    mode <- .getEncodedName(x)
    list(
        .create_visual_box_for_row_plots(mode, id, param_choices, tab_by_row, tab_by_col, se), 
        .create_selection_param_box(mode, id, param_choices, row_selectable, "row") 
    )
})

#' @export
setMethod(".createParamObservers", "RowDotPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    .define_plot_parameter_observers(mode, id,
        protected=character(0),
        nonfundamental=c(.colorByRowData, .colorBySampNameAssay, 
            .shapeByRowData, .sizeByRowData, .colorByFeatNameColor),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
    callNextMethod()
})
