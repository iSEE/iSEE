#' Track code for the plots
#' 
#' Fetches all the code that was used to generate the plots during the live
#' session of \code{iSEE}
#' 
#' @param active_panels A data.frame containing information about the currently active panels, of the same form as that produced by \code{\link{.setup_initial}}.
#' @param pObjects An environment containing \code{memory}, a list of DataFrames containing parameters for each panel of each type;
#' \code{selection_links}, a graph object containing point selection links between panels;
#' \code{commands}, a list of commands required to generated each plot. 
#' @param se_name String containing the name of the \code{SummarizedExperiment}/\code{SingleCellExperiment} object.
#' @param ecm_name String containing the name of the \code{ExperimentColorMap} in use.
#' @param sanitize_cmds Character vector containing the commands used to sanitizing the \code{se} object. 
#' This is obtained by extracting \code{cmds} from the returned output of \code{.sanitize_SE_input}.
#'
#' @return A vector containing the whole code, which can be further passed to 
#' the instance of the \code{shinyAce} editor in the modal popup of the app.
#' 
#' @author Federico Marini
#' @rdname INTERNAL_track_it_all
.track_it_all <- function(active_panels, pObjects, se_name, ecm_name, sanitize_cmds) {
  # Commands only reported for plots, not for the tables
  aobjs <- active_panels[active_panels$Type!="rowStatTable" & active_panels$Type!="heatMapPlot",]
  aobjs <- aobjs[.get_reporting_order(aobjs, pObjects$selection_links),]

  # storing to a text character vector
  tracked_code <- c(
    "## The following list of commands will generate the plots created using iSEE.",
    "## Copy them into a script or an R session containing your SingleCellExperiment.",
    "## All commands below refer to your SingleCellExperiment object as `se`.",
    "",
    sprintf("se <- %s", se_name),
    sanitize_cmds,
    sprintf("colormap <- %s", ecm_name),
    sprintf("colormap <- synchronizeAssays(colormap, se)"),
    "all_coordinates <- list()",
    "")

  # Deparsing the brushes and lasso waypoints.
  brush_code <- lasso_code <- character(0)

  for (i in seq_len(nrow(aobjs))) { 
    panel_type <- aobjs$Type[i]
    panel_id <- aobjs$ID[i]
    panel_name <- paste0(panel_type, panel_id)

    brush_struct <- pObjects$memory[[panel_type]][,.brushData][[panel_id]]
    if (!is.null(brush_struct)) { 
        brush_struct <- deparse(brush_struct, width.cutoff=80)
        brush_code <- c(brush_code, sprintf("all_brushes[['%s']] <- %s", panel_name, paste(brush_struct, collapse="\n")))
    }

    lasso_struct <- pObjects$memory[[panel_type]][,.lassoData][[panel_id]]
    if (!is.null(lasso_struct)) {
        lasso_struct <- deparse(lasso_struct, width.cutoff=80)
        lasso_code <- c(lasso_code, sprintf("all_lassos[['%s']] <- %s", panel_name, paste(lasso_struct, collapse="\n")))
    }
  }

  if (length(brush_code)) { 
      tracked_code <- c(tracked_code, 
                        "# Defining brushes",
                        strrep("#", 80), "",
                        "all_brushes <- list()", 
                        brush_code, "")
  }
  if (length(lasso_code)) {
      tracked_code <- c(tracked_code,
                        strrep("#", 80),
                        "# Defining lassos",
                        strrep("#", 80), "",
                        "all_lassos <- list()",
                        lasso_code, "")
  }

  # Defining the code to create each plot.
  for (i in seq_len(nrow(aobjs))) {
    panel_type <- aobjs$Type[i]
    panel_id <- aobjs$ID[i]
    panel_name <- paste0(panel_type, panel_id)
    
    tracked_code <- c(tracked_code,
                      strrep("#", 80),
                      paste0("## ", .decode_panel_name(panel_type, panel_id)),
                      strrep("#", 80),
                      "")

    # Adding the plotting commands.
    cur_cmds <- pObjects$commands[[panel_name]]
    collated <- c(cur_cmds$data, "")
    if (length(cur_cmds$select)) {
        collated <- c(collated, "# Receiving point selection", cur_cmds$select, "")
    }

    # Saving data for transmission after selections have been processed;
    # this is the equivalent point in .create_plots() where coordinates are saved.
    collated <- c(collated, "# Saving data for transmission", 
                  sprintf("all_coordinates[['%s']] <- plot.data", panel_name),
                  "")

    # Finishing off the rest of the commands.
    if (length(cur_cmds$setup)) {
        collated <- c(collated, "# Setting up plot coordinates", cur_cmds$setup, "")        
    }
    if (length(cur_cmds$plot)) {
        collated <- c(collated, "# Creating the plot", cur_cmds$plot)
    }

    tracked_code <- c(tracked_code, collated, "")
  }

  # Adding the heatmap code.
  tracked_heat <- character(0)
  hobjs <- active_panels[active_panels$Type=="heatMapPlot",]
  heat_names <- paste0(hobjs$Type, hobjs$ID)

  for (i in seq_along(heat_names)) {
    cur_cmds <- pObjects$commands[[heat_names[i]]]

    tracked_heat <- c(tracked_heat,
                      strrep("#", 80),
                      paste0("## ", .decode_panel_name("heatMapPlot", hobjs$ID[i])),
                      strrep("#", 80), "")

    # Adding the data setup.
    collated <- c(cur_cmds$data, "")

    # Finishing off the rest of the commands.
    if (length(cur_cmds$select)) { 
        collated <- c(collated, "# Receiving selection data", cur_cmds$select, "")
    }
    if (length(cur_cmds$zoom)) { 
        collated <- c(collated, "# Zooming in", cur_cmds$zoom, "")
    }
    if (length(cur_cmds$plot)) {
        collated <- c(collated, "# Creating the heat map", cur_cmds$plot, "")
    }
    if (length(cur_cmds$annot)) {
        collated <- c(collated, "# Adding annotations", cur_cmds$annot, "")
    }
    if (length(cur_cmds$grid)) {
        collated <- c(collated, "# Laying out the grid", cur_cmds$grid, "")
    }
    tracked_heat <- c(tracked_heat, collated)
  }
  tracked_code <- c(tracked_code, tracked_heat)

  # Adding session information.
  tracked_code <- c(tracked_code,
                    strrep("#", 80),
                    "## To guarantee the reproducibility of your code, you should also",
                    "## record the output of sessionInfo()",
                    "sessionInfo()")

  # Restoring indenting for multi-line ggplot code.
  tracked_code <- unlist(tracked_code)
  ggplot_ix <- grep("\\+$", tracked_code) + 1L
  to_mod <- tracked_code[ggplot_ix]
  to_mod <- paste0("    ", to_mod)
  to_mod <- gsub("\n", "\n    ", to_mod)
  tracked_code[ggplot_ix] <- to_mod

  return(tracked_code)
}


#' Get the reporting order
#'
#' Reordering plots by whether or not they transmit point selection information.
#' 
#' @param active_panels A data.frame containing information about the currently active panels, of the same form as that produced by \code{\link{.setup_initial}}.
#' @param select_chart A graph object containing information about how point selection information is transferred between panels.
#'
#' @details
#' This function is necessary to ensure that coordinates in transmitting plots are generated before point selection is performed in receiving plots.
#'
#' @return An integer vector specifying the order of the panels for reporting code.
#' @author Aaron Lun
#' @rdname INTERNAL_get_reporting_order
#' @importFrom igraph topo_sort
.get_reporting_order <- function(active_panels, select_chart) 
{
  N <- nrow(active_panels)
  node_names <- sprintf("%s%i", active_panels$Type, active_panels$ID)
  ordering <- topo_sort(select_chart, "out")
  order(match(node_names, names(ordering)))
}


#' Plot a snapshot of the links among panels
#'
#' Creates a graph of the current links among all currently active panels, and plots it.
#'
#' @param active_panels A data.frame containing information about the currently active panels. 
#' @param pObjects An environment containing \code{pObjects$selection_links}.
#'
#' @return Plots the graph representing the snapshot of links among plots/panels
#' @author Federico Marini
#' @rdname INTERNAL_snapshot_graph_linkedpanels
#' @importFrom igraph delete.vertices set_vertex_attr V add_edges
.snapshot_graph_linkedpanels <- function(active_panels, pObjects) {
  cur_plots <- sprintf("%s%i", active_panels$Type, active_panels$ID)
  not_used <- setdiff(V(pObjects$selection_links)$name,cur_plots)
  curgraph <- delete.vertices(pObjects$selection_links, not_used)
  curgraph <- set_vertex_attr(curgraph,"plottype", value = sub("[0-9]+$","",V(curgraph)$name))
  
  # Incorporating table links as well.
  tbl_objs <- active_panels[active_panels$Type=="rowStatTable",]
  cur_tables <- sprintf("%s%i",tbl_objs$Type,tbl_objs$ID)
  all_tlinks <- pObjects$table_links
  cur_tlinks <- all_tlinks[cur_tables]
  
  # instead of taking the links as such, check whether they are really used to link
  # following the implementation of .define_table_links...
  memory <- pObjects$memory
  
  for (i in seq_len(nrow(tbl_objs))) {
    table_used <- cur_tables[i]
    current <- cur_tlinks[[table_used]]
    for (trans in list(c("yaxis", NA, NA),
                       c("xaxis", .featExprXAxis, .featExprXAxisFeatNameTitle),
                       c("color", .colorByField, .colorByFeatNameTitle))
    ) {
      children <- current[[trans[1]]]
      child_enc <- .split_encoded(children)
      child_names <- .decode_panel_name(child_enc$Type, child_enc$ID)
      
      by_field <- trans[2]
      ref_title <- trans[3]
      
      # Only adding the edge if the plot actually receives the information via the
      # appropriate parameter choices. Y-axis for feature plots is NA, as there are no choices there.
      for (i in seq_along(child_names)) {
        if (is.na(by_field) || memory[[child_enc$Type[i]]][child_enc$ID[i], by_field]==ref_title) {
          curgraph <- add_edges(curgraph, c(table_used, children[i]))
        }
      }
    }
  }
  
  
  # Creating the plot. 
  plot(curgraph,
       edge.arrow.size = .8,
       vertex.label.cex = 1.3,
       vertex.label.family = "Helvetica",
       vertex.label.color = "black",
       vertex.label.dist = 2.5,
       vertex.color = panel_colors[V(curgraph)$plottype])
}  

