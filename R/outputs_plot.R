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
.define_plot_output <- function(plot_name, FUN, selectable, se, colormap, output, pObjects, rObjects) {
    force(FUN)
    force(se)
    force(colormap)
    gen_field <- paste0(plot_name, "_", .panelGeneralInfo)

    output[[plot_name]] <- renderPlot({
        force(rObjects[[plot_name]])
        .safe_reactive_bump(rObjects, gen_field)

        p.out <- FUN(pObjects$memory[[plot_name]], pObjects$memory, pObjects$coordinates, se, colormap)
        pObjects$commands[[plot_name]] <- p.out$cmd_list

        if (selectable) {
            pObjects$coordinates[[plot_name]] <- p.out$xy[, intersect(.allCoordinatesNames, colnames(p.out$xy))]
        }
        p.out$plot
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
#' @importFrom shinyWidgets addSpinner
.create_plot_ui <- function(plot_name, height, brush_direction, brush_fill, brush_stroke) {
    .input_FUN <- function(field) { paste0(plot_name, "_", field) }

    brush.opts <- brushOpts(.input_FUN(.brushField), resetOnNew=TRUE, delay=2000,
                            direction=brush_direction, fill=brush_fill, stroke=brush_stroke,
                            opacity=.brushFillOpacity)

    dblclick <- .input_FUN(.zoomClick)
    clickopt <- .input_FUN(.lassoClick)
    panel_height <- paste0(height, "px")

    addSpinner(
        plotOutput(plot_name, brush=brush.opts, dblclick=dblclick, click=clickopt, height=panel_height),
        color=brush_fill
    )
}

#' Trigger replotting
#'
#' Trigger regeneration of a particular plot, clearing all selections from Shiny brushes or lasso waypoints.
#'
#' @param mode String specifying the (encoded) panel type of the current panel to be replotted.
#' @param id Integer scalar specifying the ID of the current panel of the specified type.
#' @param pObjects An environment containing \code{memory}, a list of DataFrames containing parameters for each panel of each type.
#' @param rObjects A reactive list containing incrementable counters for all panels,
#'
#' @return \code{NULL}, invisibly.
#'
#' @details
#' This function will trigger replotting of the current panel by updating the appropriate incrementable counter in \code{rObjects}.
#' It will clear Shiny brushes, lasso way points and any saved selections.
#' It will also trigger replotting of all children.
#'
#' Note that this function relies on the fact that \code{pObjects} and \code{rObjects} are passed by reference.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_regenerate_unselected_plot
#' @seealso
#' \code{\link{iSEE}}
.regenerate_unselected_plot <- function(plot_name, pObjects, rObjects) {
    rObjects[[plot_name]] <- .increment_counter(isolate(rObjects[[plot_name]]))

    # Destroying any brushes or lasso waypoints.
    has_active <- .multiSelectionHasActive(pObjects$memory[[plot_name]])
    pObjects$memory[[plot_name]][[.brushData]] <- list()

    # Destroying history.
    has_saved <- .any_saved_selection(pObjects$memory[[plot_name]])
    pObjects$memory[[plot_name]][[.multiSelectHistory]] <- list()

    # Forcibly updating all children.
    # Hypothetically, this could cause union children to trigger twice,
    # as their reactive values will be updated twice. In practice,
    # plot rendering should occur after all reactives are resolved,
    # so this shouldn't occur. Oh well.
    if (has_active) {
        .safe_reactive_bump(rObjects, paste0(plot_name, "_", .panelReactivated))
    }
    if (has_saved) {
        .safe_reactive_bump(rObjects, paste0(plot_name, "_", .panelResaved))
    }

    invisible(NULL)
}
