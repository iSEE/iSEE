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
.define_plot_output <- function(mode, id, FUN, selectable,
    se, colormap, output, pObjects, rObjects) 
{
    plot_name <- paste0(mode, id)
    force(FUN)
    force(se)
    force(colormap)

    output[[plot_name]] <- renderPlot({
        force(rObjects[[plot_name]])
        rObjects[[gen_field]] <- .increment_counter(isolate(rObjects[[gen_field]]))

        p.out <- FUN(pObjects$memory[[plot_name]], pObjects$memory, pObjects$coordinates, se, colormap)
        pObjects$commands[[plot_name]] <- p.out$cmd_list

        if (selectable) {
            pObjects$coordinates[[plot_name]] <- p.out$xy[, intersect(.allCoordinatesNames, colnames(p.out$xy))]
        }
        p.out$plot
    })

    if (selectable) {
        gen_field <- paste0(plot_name, "_", .panelGeneralInfo)
        dec_name <- .decode_panel_name(mode, id)
        output[[gen_field]] <- renderUI({
            force(rObjects[[gen_field]])
            selected <- .get_selected_points(
                rownames(pObjects$coordinates[[plot_name]]), dec_name,
                pObjects$memory, pObjects$coordinates, select_all=TRUE
            )

            all_output <- list()
            if (!is.null(selected$active)) {
                n_selected <- sum(selected$active)
                n_total <- length(selected$active)
                all_output <- append(all_output,
                    list(
                        sprintf(
                            "%i of %i points in active selection (%.1f%%)",
                            n_selected, n_total, 100*n_selected/n_total
                        ),
                        br()
                    )
                )
            }

            for (i in seq_along(selected$saved)) {
                n_selected <- sum(selected$saved[[i]])
                n_total <- length(selected$saved[[i]])
                all_output <- append(all_output,
                    list(
                        sprintf(
                            "%i of %i points in saved selection %i (%.1f%%)",
                            n_selected, n_total, i, 100*n_selected/n_total
                        ),
                        br()
                    )
                )
            }

            if (length(all_output)==0L) {
                NULL
            } else {
                do.call(tagList, all_output)
            }
        })
    }

    invisible(NULL)
}

#' Define the link information
#'
#' Define the text field containing linking information for a given plot.
#'
#' @inheritParams .define_plot_parameter_observers
#'
#' @return
#' A reactive element to render the plot is added to \code{output}.
#' A \code{NULL} is invisibly returned.
#'
#' @importFrom shiny renderUI
#' @rdname INTERNAL_define_link_info_output
.define_link_info_output <- function(mode, id, pObjects, rObjects) {
    plot_name <- paste0(mode, id)
    link_field <- paste0(plot_name, "_", .panelLinkInfo)
    output[[link_field]] <- renderUI({
        force(rObjects[[link_field]])
        .define_plot_links(plot_name, pObjects$memory, pObjects$selection_links)
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
