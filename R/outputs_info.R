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
.define_link_info_output <- function(plot_name, pObjects, rObjects) {
    link_field <- paste0(plot_name, "_", .panelLinkInfo)
    output[[link_field]] <- renderUI({
        force(rObjects[[link_field]])
        .define_plot_links(plot_name, pObjects$memory, pObjects$selection_links)
    })
    invisible(NULL)
}

#' @importFrom shiny tagList
.define_selection_info_output <- function(plot_name, output, pObjects, rObjects) {
    gen_field <- paste0(plot_name, "_", .panelGeneralInfo)

    output[[gen_field]] <- renderUI({
        force(rObjects[[gen_field]])
        instance <- pObjects$memory[[plot_name]]
        cur_coords <- pObjects$contents[[plot_name]]
        n_total <- nrow(cur_coords)

        all_output <- list()
        cur_brush <- instance[[.brushData]]
        brushed <- .get_brushed_points(cur_coords, cur_brush)
        if (!is.null(brushed)) {
            n_brushed <- length(brushed)
            all_output <- append(all_output,
                list(
                    sprintf(
                        "%i of %i points in active selection (%.1f%%)",
                        n_brushed, n_total, 100*n_brushed/n_total
                    ),
                    br()
                )
            )
        }

        saved <- instance[[.multiSelectHistory]]
        for (i in seq_along(saved)) {
            brushed <- .get_brushed_points(cur_coords, saved[[i]])
            if (is.null(brushed)) {
                next
            }

            n_brushed <- length(brushed)
            all_output <- append(all_output,
                list(
                    sprintf(
                        "%i of %i points in saved selection %i (%.1f%%)",
                        n_brushed, n_total, i, 100*n_brushed/n_total
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
