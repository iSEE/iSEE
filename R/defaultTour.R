#' Default tour assembly
#'
#' Assemble an appropriate tour based on the presence of available parameters.
#'
#' @param se A \linkS4class{SummarizedExperiment} object containing the dataset of interest.
#' @param memory A list of \linkS4class{Panel} objects specifying the current memory of the app.
#'
#' @return A named list containing the steps of a tour.
#'
#' @details
#' The default tour is explicitly designed to work with the \code{iSEE(sce)} instance from the Example in \code{?\link{iSEE}}.
#' This requires some heuristics to detect whether \code{se} is, in fact, the desired dataset.
#' In all other applications, the function will return a placeholder tour to indicate that the default tour is disabled.
#'
#' If both \code{se=NULL} and \code{memory=NULL}, no checks are performed and the default tour is always returned.
#' This is used by \code{\link{defaultTour}} to return an example for people to look at.
#'
#' I will note that I tried to create a generic system that would allow each panel to decide what steps it would put in.
#' The problem is that it ends up being too general to be useful as an introductory tour, 
#' as you can't talk about specific covariates or values.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_assemble_tour
#' @importFrom SummarizedExperiment colData
#' @importFrom SingleCellExperiment reducedDimNames
.assemble_tour <- function(se, memory) {
    element <- intro <- character(0)
    all_encs <- vapply(memory, .getEncodedName, "")
    check <- !is.null(se) && !is.null(memory)

    # General purpose.
    element <- c(element, "#Welcome")
    intro <- c(intro, "Welcome to the interactive tour for iSEE - the Interactive SummarizedExperiment Explorer.<br/><br/>You will be shown around the different components of iSEE and learn the basic usage mechanisms by doing. Highlighted elements will respond to the user's actions, while the rest of the UI will be shaded. You will be prompted at particular spots to perform some actions, which will be marked with \"<strong>Action:</strong>\" text. Please take care to follow these instructions, since later parts of the tour may assume that all the actions from previous steps have been performed.<br/><br/><strong>Action:</strong> now, click on the 'Next' button or use the right arrow of your keyboard to proceed into your tour.")

    element <- c(element, "#allpanels")
    intro <- c(intro, "iSEE provides a Shiny interface that allows you to generate a series of panels for exploring <code>SummarizedExperiment</code> objects. Here, we use single-cell RNA sequencing data from the <a href=\"https://doi.org/doi:10.18129/B9.bioc.scRNAseq\"><i>scRNAseq</i> package</a>, specifically a subset of gene expression profiles from cells in the mouse visual cortex <a href=\"https://doi.org/10.1038/nn.4216\">(Tasic <i>et al.</i>, 2016)</a>. Each column of the <code>SummarizedExperiment</code> corresponds to a cell, while each row corresponds to a gene.<br/><br/>Using iSEE, you can generate a variety of different panel types to visualize the data. These are described in more detail in the following steps.")

    # Using the reduced dimension plot to highlight features.
    enc_rdp <- "ReducedDimensionPlot1"
    if (check && !enc_rdp %in% all_encs) {
        return(.truncated_tour)
    }

    element <- c(element, paste0("#", enc_rdp)) 
    intro <- c(intro, "For example, you can construct a <font color=\"#402ee8\">Reduced dimension plot</font> to visualize a low-dimensional representation (e.g., PCA, <i>t</i>-SNE) of our dataset of interest. Here, each point represents a cell.")

    element <- c(element, paste0("#", enc_rdp, "_", .dataParamBoxOpen)) 
    intro <- c(intro, "For each plot panel, a variety of parameters are available to control the appearance and behaviour of the plot. These parameters are located in these collapsible boxes, such as the <font color=\"#402ee8\">Data parameters</font> box that contains parameters related to the type of data being shown.<br /><br /><strong>Action:</strong> click on the header of this collapsible box to see the available options.")

    if (check && (!is(se, "SingleCellExperiment") || !all(c("PCA", "TSNE") %in% reducedDimNames(se)))) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_rdp, "_", .redDimType, " + .selectize-control"))
    intro <- c(intro, "<strong>Action:</strong> change this to <code>TSNE</code> to see the two-dimensional <i>t</i>-SNE representation.")

    element <- c(element, paste0("#", enc_rdp))
    intro <- c(intro, "You can see how the panel immediately switched to the requested dimensionality reduction result.")
 
    element <- c(element, paste0("#", enc_rdp, "_", .visualParamBoxOpen))
    intro <- c(intro, "The <font color=\"#402ee8\">Visual parameters</font> box contains parameters related to coloring, point appearance, and other visual aspects.<br /><br /><strong>Action:</strong> please click on the header of this box to see the available options.")

    element <- c(element, paste0("#", enc_rdp, "_", .colorByField))
    intro <- c(intro, "By default, the points (cells) are not colored.<br/><br/><strong>Action:</strong> select <code>Column data</code> to colour the cells by a column metadata variable.")

    if (check && !"passes_qc_checks_s" %in% colnames(colData(se))) {
        print("passes_qc_checks_s")
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_rdp, "_", .colorByColData, " + .selectize-control"))
    intro <- c(intro, "Currently, the points are colored by the first available column in the <code>colData</code> slot.<br/><br/><strong>Action:</strong> choose <code>passes_qc_checks_s</code> to colour cells based on whether they passed quality control (QC) checks.")

    element <- c(element, paste0("#", enc_rdp))
    intro <- c(intro, "You can now see that all cells that passed QC (<code>Y</code>) are colored differently from those that did not (<code>N</code>).")

    # Demonstrating the column data plot next.
    enc_cdp <- "ColumnDataPlot1"
    if (check && !enc_cdp %in% all_encs) {
        return(.truncated_tour)
    }
    
    element <- c(element, paste0("#", enc_cdp))
    intro <- c(intro, "You can construct a <font color=\"#402ee8\">Column data plot</font> involving column-level metadata, where each point represents a column (in this case, a cell) in the <code>SummarizedExperiment</code> object. Points can be displayed using violin plots stratified by an experimental factor on the x-axis, or as a scatter plot involving continuous variables on both the x- and y-axes.")

    element <- c(element, paste0("#", enc_cdp, "_", .dataParamBoxOpen))
    intro <- c(intro, "The plotting behaviour will also change depending on the type of data being plotted. This is best illustrated with <font color=\"#402ee8\">Column data plot 1</font>.<br/><br/><strong>Action:</strong> click on the header of this collapsible box.")

    element <- c(element, paste0("#", enc_cdp, "_", .colDataXAxis))
    intro <- c(intro, "Currently, there is only one violin plot.<br/><br/><strong>Action:</strong> click on <code>Column data</code>.")

    element <- c(element, paste0("#", enc_cdp))
    intro <- c(intro, "You can see how this now changes to a scatter plot when plotting two continuous variables against each other.")

    if (check && !"driver_1_s" %in% colnames(colData(se))) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_cdp, "_", .colDataXAxisColData, " + .selectize-control"))
    intro <- c(intro, "<strong>Action:</strong> click on (or type) <code>driver_1_s</code>.")

    element <- c(element, paste0("#", enc_cdp))
    intro <- c(intro, "The plot now changes to a grouped violin plot for a categorical variable on the x-axis. Note that a horizontal violin plot can also be generated if the categorical variable is assigned to the y-axis.")

    if (check && !"dissection_s" %in% colnames(colData(se))) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_cdp, "_", .colDataYAxis, " + .selectize-control"))
    intro <- c(intro, "<strong>Action:</strong> click on (or type) <code>dissection_s</code>.")

    element <- c(element, paste0("#", enc_cdp))
    intro <- c(intro, "The plot now changes to a Hinton plot for two categorical variables. The size of each rectangle corresponds to the number of points in each combination of covariate levels.")

    # Doing a roll call of the remaining plots.
    enc_fap <- "FeatureAssayPlot1"
    if (check && !enc_fap %in% all_encs) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_fap))
    intro <- c(intro, "A <font color=\"#402ee8\">Feature assay plot</font> displays the assay values (here, gene expression) across all cells. Like the <font color=\"#402ee8\">Column data plot</font>, points represent cells and can be shown in both violin or scatter plots, depending on the variable selected on the x-axis.")

    enc_cdt <- "ColumnDataTable1"
    if (check && !enc_cdt %in% all_encs) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_cdt))
    intro <- c(intro, "A <font color=\"#402ee8\">Column data table</font> displays the contents of the <code>colData</code> of your input object as a table.")

    enc_rdt <- "RowDataTable1"
    if (check && !enc_rdt %in% all_encs) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_rdt))
    intro <- c(intro, "Similarly, a <font color=\"#402ee8\">Row data table</font> displays the contents of the <code>rowData</code> of your input object as a table.")

    enc_rop <- "RowDataPlot1"
    if (check && !enc_rop %in% all_encs) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_rop))
    intro <- c(intro, "You can also plot the <code>rowData</code> values against each other with a <font color=\"#402ee8\">Row data plot</font>.")

    enc_sap <- "SampleAssayPlot1"
    if (check && !enc_sap %in% all_encs) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_sap))
    intro <- c(intro, "A <font color=\"#402ee8\">Sample assay plot</font> displays the assay values across features for a chosen sample on the y-axis (possibly against another sample on the x-axis). This is best used for bulk datasets where it is more feasible to inspect individual samples.")

    enc_chp <- "ComplexHeatmapPlot1"
    if (check && !enc_chp %in% all_encs) {
        return(.truncated_tour)
    }
    element <- c(element, paste0("#", enc_chp))
    intro <- c(intro, "You can produce a <font color=\"#402ee8\">Complex heat map</font> to visualize a feature-by-sample matrix of assay values, for a selected subset of features.")

    # Highlighting the multiple selection modes.
    element <- c(element, paste0("#", enc_fap, "_", .selectParamBoxOpen))
    intro <- c(intro, 'The <font color="#402ee8">Selection parameters</font> box contains parameters to handle sharing of multiple point selections across panels. The idea is that points selected in a "transmitting" panel can be visualized in a "receiving" panel.<br/><br/><strong>Action:</strong> click on the header of this collapsible box to see the available options.')

    element <- c(element, paste0("#", enc_fap, "_", .selectColSource, " + .selectize-control"))
    intro <- c(intro, "<strong>Action:</strong> select <font color=\"#402ee8\">Reduced dimension plot 1</font>.<br /><br />This means that multiple point selections from <font color=\"#402ee8\">Reduced dimension plot 1</font> will be transmitted to the current <font color=\"#402ee8\">Feature assay plot 1</font>.")

    element <- c(element, paste0("#", enc_rdp))
    intro <- c(intro, "<strong>Action:</strong> make a brush on this plot by click-and-dragging a rectangular region over some points.")

    element <- c(element, paste0("#", enc_fap))
    intro <- c(intro, "The points selected in the <font color=\"#402ee8\">Reduced dimension plot 1</font> are shown as opaque in this plot, while all other points are transparent.")

    element <- c(element, paste0("#", enc_fap, "_", .selectEffect))
    intro <- c(intro, 'The effect of the selection can also be altered. If set to <font color="#402ee8">Restrict</font>, only the selected points will be plotted in the receiving plot. If <font color="#402ee8">Color</font>, all points are plotted and the selected points are marked with a specified color. The default is to use <font color="#402ee8">Transparent</font>, where all non-selected points are transparent.')

    element <- c(element, paste0("#", enc_rdp, "_", .panelMultiSelectInfo))
    intro <- c(intro, "Some information on the number of selected points is displayed in this element.")

    element <- c(element, paste0("#", enc_rdp, "_", .panelSelectLinkInfo))
    intro <- c(intro, "Similarly, a summary of the relationships between panels is displayed here.")

    element <- c(element, paste0("#", enc_fap))
    intro <- c(intro, 'Even though this panel is receiving a selection, it is still able to transmit a selection to other panels. In this manner, we can construct arbitrarily complex (non-circular) networks between panels to display only a subset of points that match a certain criteria. This is equivalent to multi-step gating schemes in flow cytometry, provided the selection effect is set to <font color="#402ee8">Restrict</font>.<br/><br/>Another important thing to keep in mind when selecting points from violin plots is that points will be selected only if the brushed area includes the center of the x-tick, i.e., the center of the violin plot. This simplifies point selection as users do not need to capture the width of the violin plot, the values of which have no real meaning for individual points.')

    # Highlighting the single selection mode.
    element <- c(element, paste0("#", enc_fap, "_", .dataParamBoxOpen))
    intro <- c(intro, 'Certain parameters can also respond to single selections from other panels, which we will demonstrate with the <font color="#402ee8">Feature assay plot</font>.<br/><br/><strong>Action:</strong> open this box.')

    element <- c(element, paste0("#", enc_fap, "_", .featAssayYAxisRowTable, " + .selectize-control"))
    intro <- c(intro, '<strong>Action:</strong> select <font color="#402ee8">Row data table 1</font>.')

    element <- c(element, paste0("#", enc_rdt))
    intro <- c(intro, '<strong>Action:</strong> click on another row.')

    element <- c(element, paste0("#", enc_fap))
    intro <- c(intro, 'You can see that the y-axis has now changed to reflect the new row selection in <font color="#402ee8">Row data table 1</font>. A similar approach can be used to automatically synchronize the coloring of points by assay values.')

    # Highlighting the zoom.
    element <- c(element, paste0("#", enc_rdp))
    intro <- c(intro, 'A zooming functionality is also available by first brushing, then double-clicking on the brushed area.<br /><br /><strong>Action:</strong> click and drag to brush over an area. Now, double click inside the marked area to zoom in. To zoom out to the original plot, double-click on the plot.')

    # Highlighting the heatmap: KRA to add.

    # Wrapping up.
    element <- c(element, ".navbar-static-top")
    intro <- c(intro, "A number of useful features are available from the dashboard header.<br /><br /><strong>Action:</strong> click on the panels icon to open the Panel Organization menu.<br /><br />Clicking on the <i>Organize panels</i> button opens a modal window, which contains a multi-selectize input widget to add, remove, resize, and reorder panels in the main interface. You can see how the boxes below this control mirror the order of the panels in the main body of the app.")

    element <- c(element, ".navbar-static-top")
    intro <- c(intro, "<strong>Action:</strong> click on the download icon to open the Export dropdown.<br /><br />In this dropdown menu, you can access a graph representation of the existing links between panels, the code required to reproduce all of the plots, and the code required to reproduce the current app state.")

    element <- c(element, ".navbar-static-top")
    intro <- c(intro, "<strong>Action:</strong> click on the question mark icon to open the Documentation dropdown.<br/><br/> This contains the button to start the <code>introjs</code>-based tour, which you are currently taking, as well as a link to the vignette for the iSEE package, in case you want to consult it while running the app.")

    element <- c(element, ".navbar-static-top")
    intro <- c(intro, "<strong>Action:</strong> click on the info icon.<br/><br/>This reports the session information from which you are running iSEE (which you should report in case of issues or bugs), the appropriate citation if you use iSEE in your work, as well as information on how to contact the iSEE development team.")

    element <- c(element, "#Thanks")
    intro <- c(intro, "Thank you for taking the tour of iSEE!")

    data.frame(element=element, intro=intro, stringsAsFactors=FALSE)
}

.truncated_tour <- data.frame(
    element="#Disabled",
    intro="The default tour is only enabled for the default Allen example in <code>?iSEE</code>. For all other configurations, maintainers of the iSEE instance should define their own tours.",
    stringsAsFactors=FALSE
)

#' Define the default tour
#'
#' Define the default tour for the subset of the Allen brain dataset.
#' This is only available when run on the \code{iSEE(sce)} example in \code{?"\link{iSEE}"}.
#'
#' @return A data.frame where each row is a tour step.
#' The first column specifies the UI element to be highlighted by the tour,
#' while the second column contains the tour text.
#'
#' @author Aaron Lun
#'
#' @examples
#' defaultTour()
#' @export
defaultTour <- function() .assemble_tour(NULL, NULL) 
