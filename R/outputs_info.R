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
#' @rdname INTERNAL_define_link_info_output
#' @importFrom shiny tagList renderUI
.define_selection_info_output <- function(plot_name, output, pObjects, rObjects) {
    gen_field <- paste0(plot_name, "_", .panelGeneralInfo)

    output[[gen_field]] <- renderUI({
        force(rObjects[[gen_field]])
        instance <- pObjects$memory[[plot_name]]
        cur_coords <- pObjects$contents[[plot_name]]
        n_total <- nrow(cur_coords)

        all_output <- list()
        env <- new.env()
        env$contents <- cur_coords

        if (.multiSelectionHasActive(instance)) {
            env$select <- .multiSelectionActive(instance)
            cmds <- .multiSelectionCommands(instance, NA)
            .text_eval(cmds, env)
            n_brushed <- length(env$selected)

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
            cmds <- .multiSelectionCommands(instance, i)
            if (is.null(cmds)) {
                next
            }

            env$select <- saved[[i]]
            .text_eval(cmds, env)
            n_brushed <- length(env$selected)

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

#' @importFrom shiny tagList renderUI
#' @importFrom igraph adjacent_vertices get.edge.ids E
.define_link_info_output <- function(plot_name, output, pObjects, rObjects) {
    link_field <- paste0(plot_name, "_", .panelLinkInfo)

    output[[link_field]] <- renderUI({
        force(rObjects[[link_field]])
        info <- NULL
        graph <- pObjects$selection_links

        # Defining from 'selection_links' first.
        parents <- names(adjacent_vertices(graph, plot_name, mode="in")[[1]]) 
        for (p in parents) {
            full_name <- .getFullName(pObjects$memory[[p]])
            info <- c(info, list("Receiving selection from", em(strong(full_name)), br()))
        }

        children <- names(adjacent_vertices(graph, plot_name)[[1]])
        for (child in children) {
            full_name <- .getFullName(pObjects$memory[[child]])
            info <- c(info, list("Transmitting selection to", em(strong(full_name)), br()))
        }

        # Defining from aesthetic links.
        graph2 <- pObjects$aesthetics_links

        parents2 <- names(adjacent_vertices(graph2, plot_name, mode="in")[[1]])
        for (p in parents2) {
            full_name <- .getFullName(pObjects$memory[[p]])
            eid <- get.edge.ids(graph2, c(p, plot_name))

            fields <- E(graph2)$fields[[eid]]
            for (f in fields) {
                info <- c(info, list("Receiving", em(strong(f)), "from", em(strong(full_name)), br()))
            }
        }

        children2 <- names(adjacent_vertices(graph2, plot_name)[[1]])
        for (child in children2) {
            full_name <- .getFullName(pObjects$memory[[child]])
            eid <- get.edge.ids(graph2, c(plot_name, child))

            fields <- E(graph2)$fields[[eid]]
            for (f in fields) {
                info <- c(info, list("Transmitting", em(strong(f)), "to", em(strong(full_name)), br()))
            }
        }

        if (length(info)==0L) {
            NULL
        } else {
            do.call(tagList, info)
        }
    })
}
