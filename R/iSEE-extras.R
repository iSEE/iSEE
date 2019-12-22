#' Report plot links
#'
#' Report the links to/from other panels in the interface for the current plotting panel.
#'
#' @param panel String containing the encoded name for the current plotting panel.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param graph A graph object produced by \code{\link{.spawn_selection_chart}}, specifying the point selection links between panels.
#'
#' @return A HTML object containing a description of the panel from which \code{panel} receives information,
#' and a description of all the other panels to which \code{panel} transmits information.
#'
#' @details
#' Information reception includes the receipt of point selection information from a transmitting plot,
#' or the receipt of a feature/sample selection information from a row/column statistics table (for color or x/y-axis specification).
#' Transmission should involve transferring point selection information from \code{panel} to other receiving panels.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_plot_links
#' @seealso
#' \code{\link{iSEE}}
#'
#' @importFrom shiny em strong br tagList
#' @importFrom igraph adjacent_vertices
.define_plot_links <- function(panel, memory, graph)
{
    enc <- .split_encoded(panel)
    param_choices <- memory[[enc$Type]][enc$ID, ]
    output <- list()

    # Checking select status.
    select_in <- param_choices[[.selectByPlot]]
    if (select_in!=.noSelection) {
        output <- c(output, list("Receiving selection from", em(strong(select_in)), br()))
    }

    if (enc$Type %in% col_point_plot_types) {
        col_tab <- .colorByRowTable
        col_title <- .colorByFeatNameTitle
        y_tab <- .featAssayYAxisRowTable
        x_tab <- .featAssayXAxisRowTable
        x_type <- .featAssayXAxis
        x_title <- .featAssayXAxisFeatNameTitle
    } else {
        col_tab <- .colorByColTable
        col_title <- .colorBySampNameTitle
        y_tab <- .sampAssayYAxisColTable
        x_tab <- .sampAssayXAxisColTable
        x_type <- .sampAssayXAxis
        x_title <- .sampAssayXAxisSampNameTitle
    }

    # Checking colour status.
    if (param_choices[[.colorByField]] == col_title && param_choices[[col_tab]] != .noSelection) {
        output <- c(output, list("Receiving color from", em(strong(param_choices[[col_tab]])), br()))
    }

    # Checking input/output for feature and sample assay plots.
    if (enc$Type %in% c("featAssayPlot", "sampAssayPlot")) {
        if (param_choices[[y_tab]] != .noSelection) {
            output <- c(output, list("Receiving y-axis from", em(strong(param_choices[[y_tab]])), br()))
        }
        if (param_choices[[x_type]] == x_title && param_choices[[x_tab]] != .noSelection) {
            output <- c(output, list("Receiving x-axis from", em(strong(param_choices[[x_tab]])), br()))
        }
    }

    # Defining immediate children.
    children <- names(adjacent_vertices(graph, panel)[[1]])
    child_enc <- .split_encoded(children)
    child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)
    for (child in child_names) {
        output <- c(output, list("Transmitting selection to", em(strong(child)), br()))
    }

    do.call(tagList, output)
}

#' Report table links
#'
#' Report the links to/from other panels in the interface for the current row statistics table.
#'
#' @param panel String containing the encoded name for the current row statistics table.
#' @param memory A list of DataFrames containing parameters for each panel of each type.
#' @param table_links A list of lists produced by \code{\link{.spawn_table_links}}, specifying the links between tables and dependent plots.
#'
#' @return A HTML object containing a description of the panel from which \code{panel} receives information,
#' and a description of all the other panels to which \code{panel} transmits information.
#'
#' @details
#' Information transmission from a row statistics table involves selection of features for use in color or x/y-axis specification in column-based plots.
#' Information transmission from a column statistics table involves selection of samples for use in color or x/y-axis specification in row-based plots.
#' Information reception is less common and involves the receipt of point selections from a transmitting plot.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_define_table_links
#' @seealso
#' \code{\link{iSEE}}
#'
#' @importFrom shiny em strong br tagList
#' @importFrom igraph adjacent_vertices
.define_table_links <- function(panel, memory, table_links) {
    enc <- .split_encoded(panel)
    param_choices <- memory[[enc$Type]][enc$ID,]
    output <- list()

    # Checking select status.
    select_in <- param_choices[[.selectByPlot]]
    if (select_in!=.noSelection) {
        output <- c(output, list("Receiving selection from", em(strong(select_in)), br()))
    }

    transmittees <- list(c("yaxis", "y-axis", NA, NA))
    if (enc$Type == "rowStatTable") {
        transmittees <- c(transmittees,
                list(
                    c("xaxis", "x-axis", .featAssayXAxis, .featAssayXAxisFeatNameTitle),
                    c("color", "color", .colorByField, .colorByFeatNameTitle)))
    } else {
        transmittees <- c(transmittees,
                list(c("xaxis", "x-axis", .sampAssayXAxis, .sampAssayXAxisSampNameTitle),
                c("color", "color", .colorByField, .colorBySampNameTitle)))
    }

    # Checking where it broadcasts to plots.
    current <- table_links[[panel]]
    for (trans in transmittees) {
        children <- current[[trans[1]]]
        child_enc <- .split_encoded(children)
        child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)

        out_str <- paste("Transmitting", trans[2], "to")
        by_field <- trans[3]
        ref_title <- trans[4]

        # Only writing a broadcast label if the plot actually receives the information via the appropriate parameter choices.
        # Y-axis for feature/sample assay plots is NA, as there are no choices there, so it always gets listed.
        for (i in seq_along(child_names)) {
            if (is.na(by_field) || memory[[child_enc$Type[i]]][child_enc$ID[i], by_field] == ref_title) {
                output <- c(output, list(out_str, em(strong(child_names[i])), br()))
            }
        }
    }

    do.call(tagList, output)
}

#' Establish the evaluation order
#'
#' Establish the order in which connected panels are to be evaluated during app initialization.
#'
#' @param graph A graph object containing links between panels, produced by \code{\link{.spawn_selection_chart}}.
#'
#' @details
#' This function identifies any initial connections between panels (e.g., specified in the panel arguments) for point selection.
#' It then orders the connected panels such that any transmitters are placed in front of their receivers.
#'
#' The idea is to \dQuote{evaluate} the plots at the start of the app, to obtain the coordinates for transmitting to other panels.
#' Otherwise, errors will be encountered whereby a panel tries to select from a set of coordinates that do not yet exist.
#'
#' Unlike its relative \code{\link{.get_reporting_order}}, only transmitting panels are ever reported by this function.
#' It is not necessary to evaluate receiving-only panels, and in fact will result in errors for heatmaps and row statistics tables,
#' as these do not even have coordinates to save.
#'
#' @return A character vector containing encoded names for transmitting panels in their evaluation order.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_establish_eval_order
#' @seealso
#' \code{\link{iSEE}},
#' \code{\link{.spawn_selection_chart}}
#' @importFrom igraph delete.vertices V topo_sort degree
.establish_eval_order <- function(graph) {
    iso <- V(graph)[degree(graph, mode="out") == 0]
    graph <- delete.vertices(graph, iso)
    names(topo_sort(graph, mode="out"))
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
    a(href="https://www.sib.swiss/", "University of Zurich and SIB Swiss Institute of Bioinformatics"),
    a(href="https://www.kennedy.ox.ac.uk", "Kennedy Institute of Rheumatology, University of Oxford"),
    a(href="http://www.unimedizin-mainz.de/imbei","Institute for Medical Biostatistics, Epidemiology and Informatics"))),
    br(), br(),
    HTML(sprintf("The iSEE package is being developed on %s under the %s license.",
    a(href="https://github.com/csoneson/iSEE", "GitHub"),
    a(href="https://opensource.org/licenses/MIT","MIT")))
)
