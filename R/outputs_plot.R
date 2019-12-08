#' Define the plot output
#'
#' Define a reactive expression to render the plot output for a given panel type and identifier.
#'
#' @inheritParams .define_plot_parameter_observers
#' @param se The \linkS4class{SingleCellExperiment} object to be visualized.
#' @param colormap The \linkS4class{ExperimentColorMap} object containing coloring information.
#' @param FUN A function that creates a plot of the panel type specified by \code{mode}, see \code{\link{.make_redDimPlot}} for an example.
#' @param selectable Logical scalar indicating whether the plot contains points that can be selected.
#'
#' @return
#' A reactive element to render the plot is added to \code{output}.
#' If \code{selectable=TRUE}, another element is added to fill in selection information in the text field.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_define_plot_output
#' @importFrom shiny renderPlot renderUI tagList br
.define_plot_output <- function(mode, id, panel, selectable, output, pObjects, rObjects) {
    plot_name <- paste0(mode, id)

    gen_field <- paste0(plot_name, "_", .panelGeneralInfo)

    output[[plot_name]] <- renderPlot({
        force(rObjects[[plot_name]])
        .safe_reactive_bump(rObjects, gen_field)

        # p.out <- FUN(pObjects$memory[[plot_name]], pObjects$memory, pObjects$coordinates, se, colormap)
        pObjects$commands[[plot_name]] <- panel$cmd_list

        if (selectable) {
            pObjects$coordinates[[plot_name]] <- panel$xy[, intersect(.allCoordinatesNames, colnames(panel$xy))]
        }
        panel$plot
    })


    invisible(NULL)
}

#' Create plot UI
#'
#' Create the \code{\link{plotOutput}} object for a given panel containing a single plot.
#'
#' @inheritParams .define_plot_parameter_observers
#' @param height Integer scalar specifying the height of the plot in pixels.
#' @param brush_direction String specifying the direction of brushing, i.e., \code{"x"}, \code{"y"} or \code{"xy"}.
#' @param brush_fill String containing a color to use for the fill of the brush.
#' @param brush_stroke String containing a color to use for the stroke of the brush.
#'
#' @return The output of \code{\link{plotOutput}} with relevant parametrization.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_create_plot_ui
.create_plot_ui <- function(mode, id, height, brush_direction, brush_fill, brush_stroke) {
    plot_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    brush.opts <- brushOpts(.input_FUN(.brushField), resetOnNew=TRUE, delay=2000,
        direction=brush_direction, fill=brush_fill, stroke=brush_stroke,
        opacity=.brushFillOpacity)

    dblclick <- .input_FUN(.zoomClick)
    clickopt <- .input_FUN(.lassoClick)
    panel_height <- paste0(height, "px")

    plotOutput(plot_name, brush=brush.opts, dblclick=dblclick, click=clickopt, height=panel_height)
}
