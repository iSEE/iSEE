#' Track code for the plots
#' 
#' Fetches all the code that was used to generate the plots during the live
#' session of \code{iSEE}
#' 
#' @param rObjects A reactive list containing information for all panels (
#' \code{rObjects$active_panels})
#' @param pObjects An environment containing \code{memory}, a list of DataFrames
#' containing parameters for each panel of each type; \code{brush_links}; 
#' \code{commands} ; \code{extra_plot_cmds}. All of these will be normally 
#' contained in the environment in use when running \code{iSEE} 
#' @param se_name The name of the \code{SummarizedExperiment}/\code{SingleCellExperiment}
#' object
#' @param ecm_name The name of the \code{ExperimentColorMap} in use
#' @param sanitize_cmds The character vector containing the commands resulting 
#' from sanitizing the \code{se} object. This is obtained by extracting \code{cmds}
#' from the returned output of \code{.sanitize_SE_input}.
#'
#' @return A vector containing the whole code, which can be further passed to 
#' the instance of the \code{shinyAce} editor in the modal popup of the app
#' 
#' @author Federico Marini
#' @rdname INTERNAL_track_it_all
.track_it_all <- function(rObjects, pObjects, se_name, ecm_name, sanitize_cmds) 
{
  # Commands only reported for plots, not for the tables
  aobjs <- as.data.frame(rObjects$active_panels)
  aobjs <- aobjs[aobjs$Type!="rowStatTable" & aobjs$Type!="heatMapPlot",]
  aobjs <- aobjs[.get_reporting_order(aobjs, pObjects$brush_links),]

  # storing to a text character vector
  tracked_code <- c(
    "## The following list of commands will generate the plots created using iSEE.",
    "## Copy them into a script or an R session containing your SingleCellExperiment.",
    "## All commands below refer to your SingleCellExperiment object as `se`.",
    "",
    sprintf("se <- %s", se_name),
    sprintf(sanitize_cmds),
    sprintf("colormap <- %s", ecm_name),
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
    if (length(cur_cmds$brush)) {
        collated <- c(collated, "# Receiving brush data", cur_cmds$brush, "")
    }

    # Saving data for transmission after brushes have been processed;
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
  hobjs <- as.data.frame(rObjects$active_panels)
  hobjs <- hobjs[hobjs$Type=="heatMapPlot",]
  heat_names <- paste0(hobjs$Type, hobjs$ID)

  for (i in seq_along(heat_names)) {
    cur_cmds <- pObjects$commands[[heat_names[i]]]

    tracked_heat <- c(tracked_heat,
                      strrep("#", 80),
                      paste0("## ", .decode_panel_name("heatMapPlot", hobjs$ID[i])),
                      strrep("#", 80), "",
                      cur_cmds[["y"]], cur_cmds[["order"]], cur_cmds[["subset"]], "")

    tracked_heat <- c(tracked_heat, "# Constructing heat map", cur_cmds[["heatmap"]], "",
                      "# Adding annotations", cur_cmds[["annotations"]], "", 
                      "# Generating legends", cur_cmds[["legends"]], "",
                      "# Laying out the grid", cur_cmds[["grid"]], "")
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
#' Reordering plots by whether or not they exhibited brushing.
#' 
#' @param active_plots The active plots, obtained for example after calling
#' \code{as.data.frame(rObjects$active_panels)}
#' @param brush_chart The links between brushed objects (e.g. via 
#' \code{pObjects$brush_links})
#'
#' @return Indices for the plots, sorted upon active usage of brushing or not
#' @author Federico Marini
#' @rdname INTERNAL_get_reporting_order
#' @importFrom igraph topo_sort
.get_reporting_order <- function(active_plots, brush_chart) 
{
  N <- nrow(active_plots)
  node_names <- sprintf("%s%i", active_plots$Type, active_plots$ID)
  ordering <- topo_sort(brush_chart, "out")
  order(match(node_names, names(ordering)))
}


#' Plot a snapshot of the links among panels
#'
#' Reads in the stored reactive (\code{rObjects}) and the \code{pObjects}
#' environment, and creates a graph of the current links among all panels 
#' open in the app 
#' @param rObjects A reactive list containing incrementable counters for all panels
#' @param pObjects An environment containing \code{pObjects$brush_links}
#'
#' @return Plots the graph representing the snapshot of links among plots/panels
#' @author Federico Marini
#' @rdname INTERNAL_snapshot_graph_linkedpanels
#' @importFrom igraph delete.vertices set_vertex_attr V
.snapshot_graph_linkedpanels <- function(rObjects, pObjects) 
{
  cur_plots <- sprintf("%s%i", rObjects$active_panels$Type, rObjects$active_panels$ID)
  not_used <- setdiff(V(pObjects$brush_links)$name,cur_plots)
  currgraph_used <- delete.vertices(pObjects$brush_links,not_used)
  currgraph_used <- set_vertex_attr(currgraph_used,"plottype",
                                    value = sub("[0-9]+$","",V(currgraph_used)$name))
  
  currgraph_used <- .find_links_to_table(rObjects, pObjects, currgraph_used)
  
  plot(currgraph_used,
       edge.arrow.size = .8,
       vertex.label.cex = 1.3,
       vertex.label.family = "Helvetica",
       vertex.label.color = "black",
       vertex.label.dist = 2.5,
       vertex.color = panel_colors[V(currgraph_used)$plottype])
}  


#' Find links in the table used
#' 
#' Finds the links for the tables that are used, and sets the new edges in the graph
#' this updated graph is then returned as output
#'
#' @param rObjects A reactive list containing info on the active panels
#' @param pObjects An environment containing \code{table_links}, a list of DataFrames containing parameters for each table
#' @param graph The graph object, generated for example with \code{pObjects$brush_links}, 
#' after pruning of the unused vertices
#'
#' @return The updated graph
#' @author Federico Marini
#' @rdname INTERNAL_find_links_to_table
#' @importFrom igraph add_edges
.find_links_to_table <- function(rObjects, pObjects, graph)
{
  tbl_objs <- rObjects$active_panels[rObjects$active_panels$Type=="rowStatTable",]
  cur_tables <- sprintf("%s%i",tbl_objs$Type,tbl_objs$ID)
  all_tlinks <- pObjects$table_links
  cur_tlinks <- all_tlinks[cur_tables]
  
  # for every table in use
  for (i in seq_len(nrow(tbl_objs))) {
    table_used <- cur_tables[i]
    col_links <- unlist(cur_tlinks[[table_used]]$color)
    x_links <- unlist(cur_tlinks[[table_used]]$xaxis)
    y_links <- unlist(cur_tlinks[[table_used]]$yaxis)
    any_links <- unique(c(col_links,x_links,y_links))
    
    # add the edges corresponding
    for(j in seq_len(length(any_links))){
      graph <- add_edges(graph,c(table_used, any_links[j]))
    }
  }
  return(graph)
}
