#' Create information boxes
#'
#' Create reactive elements to render text field containing various information for a given panel.
#'
#' @param plot_name String containing the name of the panel transmitting a multiple selection.
#' @param se The \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param output The Shiny output object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @details
#' The linking information box contains information about single selections that are transmitted to/from the current panel.
#'
#' The selection information box contains information about multiple selections that are transmitted to/from the current panel.
#'
#' @return
#' Reactive elements to render the relevant text fields are added to \code{output}.
#' A \code{NULL} is invisibly returned.
#'
#' @rdname INTERNAL_link_info_output
#' @importFrom shiny tagList renderUI
.create_selection_info_output <- function(plot_name, se, output, pObjects, rObjects) {
    gen_field <- paste0(plot_name, "_", .panelMultiSelectInfo)

    # nocov start
    output[[gen_field]] <- renderUI({
        .trackMultiSelection(plot_name, rObjects)
        instance <- pObjects$memory[[plot_name]]
        cur_coords <- pObjects$contents[[plot_name]]
        n_total <- .multiSelectionAvailable(instance, cur_coords)

        all_output <- list()
        env <- new.env()
        env$contents <- cur_coords
        env$se <- se

        if (.multiSelectionHasActive(instance)) {
            env$select <- .multiSelectionActive(instance)
            cmds <- .multiSelectionCommands(instance, NA)
            .textEval(cmds, env)
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
            .textEval(cmds, env)
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
    # nocov end

    invisible(NULL)
}

#' @rdname INTERNAL_link_info_output
#' @importFrom shiny tagList renderUI em strong br
#' @importFrom igraph adjacent_vertices get.edge.ids E
.create_link_info_output <- function(plot_name, output, pObjects, rObjects) {
    link_field <- paste0(plot_name, "_", .panelSelectLinkInfo)
    # nocov start
    output[[link_field]] <- renderUI({
        .trackRelinkedSelection(plot_name, rObjects)
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
    # nocov end
    invisible(NULL)
}

#' @importFrom shiny tagList HTML a br
iSEE_info <- tagList(
    HTML('<div align="center"><img src="iSEE/iSEE.png" width="150"></div>'),
    br(),
    HTML(sprintf("iSEE is a project developed by
Aaron Lun (%s),
Charlotte Soneson (%s),
Kevin Rue-Albrecht (%s),
and Federico Marini (%s).",
    a(href="http://www.cruk.cam.ac.uk/", "CRUK Cambridge Institute, University of Cambridge"),
    a(href="https://www.fmi.ch/", "Friedrich Miescher Institute for Biomedical Research and SIB Swiss Institute of Bioinformatics"),
    a(href="https://www.kennedy.ox.ac.uk", "Kennedy Institute of Rheumatology, University of Oxford"),
    a(href="http://www.unimedizin-mainz.de/imbei","Institute for Medical Biostatistics, Epidemiology and Informatics"))),
    br(), br(),
    HTML(sprintf("The iSEE package is being developed on %s under the %s license.",
    a(href="https://github.com/iSEE/iSEE", "GitHub"),
    a(href="https://opensource.org/licenses/MIT","MIT")))
)
