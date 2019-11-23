#' Define column dot parameter UI elements
#' 
#' Define UI elements for parameter selection in any panel type belonging to the column dot family.
#' 
#' @param se A \linkS4class{SingleCellExperiment} object with precomputed UI information from \code{\link{.precompute_UI_info}}.
#' @param active_panels A data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#' @param mode String specifying the encoded panel type (e.g., \code{"redDimPlot"}).
#' @param id Integer specifying the identity of the panel.
#' @param param_choices A \linkS4class{DataFrame} with one row containing the parameter choices for the current plot.
#'
#' @return 
#' A list of standardized UI elements for parameter selection in any panel types of the column dot family.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_create_column_dot_parameter_ui
.create_column_dot_parameter_ui <- function(mode, id, param_choices, se, active_panels) {
    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)
    tab_by_col <- c(.noSelection, link_sources$col_tab)
    col_selectable <- c(.noSelection, link_sources$col_plot)

    list(
        .create_visual_box_for_column_plots(mode, id, param_choices, tab_by_row, tab_by_col, se), 
        .create_selection_param_box(mode, id, param_choices, col_selectable, "column") 
    )
}

# TODO: move to a better place.
.dot_parameter_nonfundamental <- c(
    .colorByDefaultColor, .selectColor, .selectTransAlpha,
    .plotPointSize, .plotPointAlpha, .plotFontSize, .plotLegendPosition,
    .plotPointDownsample, .plotPointSampleRes, .contourAddTitle,
    .contourColor
)

#' Define column dot plot observers
#'
#' Define a series of observers to track changes to the standard parameters for a given panel of the column dot family,
#' in order to trigger replotting as necessary.
#'
#' @param mode String specifying the encoded panel type (e.g., \code{"redDimPlot"}).
#' @param id Integer specifying the index of the current panel.
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' Observers pertaining to standard parameters for a column dot plot are created.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_create_column_dot_parameter_observers
.define_column_dot_parameter_observers <- function(mode, id, 
    input, output, session, pObjects, rObjects) 
{
    .define_box_observers(mode, id, c(.visualParamBoxOpen, .selectParamBoxOpen), input, pObjects)

    .define_visual_parameter_choice_observer(mode, id, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=c(.facetByRow, .facetByColumn, .facetRowsByColData, .facetColumnsByColData),
        nonfundamental=c(.general_nonfundamental,
            .colorByColData, .colorByFeatNameAssay, .shapeByField,
            .shapeByColData, .sizeByField, .sizeByColData, .colorBySampNameColor),
        input=input, output=output, session=session, pObjects=pObjects, rObjects=rObjects) 

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
}

#' Create column dot plot UI
#'
#' Create the \code{\link{plotOutput}} object for a given panel of the column dot family.
#' 
#' @inheritParams .create_plot_ui
#' 
#' @return The output of \code{\link{plotOutput}} with relevant parametrization.
#' Brushing direction is set to \code{"xy"}.
#' 
#' @author Aaron Lun
#' 
#' @rdname INTERNAL_create_column_dot_plot_ui
.create_column_dot_plot_ui <- function(mode, id, height, brush_fill, brush_stroke) {
    .create_plot_ui(mode, id, height, brush_direction="xy", brush_fill=brush_fill, brush_stroke=brush_stroke)
}
