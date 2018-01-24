.track_it_all <- function(rObjects, pObjects, se_name) 
{
  # Commands only reported for plots, not for the tables
  aobjs <- as.data.frame(rObjects$active_plots)
  aobjs <- aobjs[aobjs$Type!="geneStat",]
  aobjs <- aobjs[.get_reporting_order(aobjs, pObjects$brush_parent),]

  # storing to a text character vector
  tracked_code <- c(
    "## The following list of commands will generate the plots created using iSEE.",
    "## Copy them into a script or an R session containing your SingleCellExperiment.",
    "## All commands below refer to your SingleCellExperiment object as `se`.",
    "",
    sprintf("se <- %s", se_name),
    "all.coordinates <- list()",
    "")

  for (i in seq_len(nrow(aobjs))) {
    panel_type <- aobjs$Type[i]
    panel_id <- aobjs$ID[i]
    panel_name <- paste0(panel_type, "Plot", panel_id)
    
    tracked_code <- c(tracked_code,
                      strrep("#", 80),
                      paste0("## ", .decode_panel_name(panel_type, panel_id)),
                      strrep("#", 80),
                      "",
                      pObjects$commands[[panel_name]]
                      )

    # Adding commands to facilitate cross-plot brushing.
    if (pObjects$memory[[panel_type]][panel_id, .brushActive]) {
        tracked_code <- c(tracked_code, "",
                          "# Saving for brush transmission",
                          sprintf("all.coordinates[['%s']] <- plot.data", panel_name))
    }

    tracked_code <- c(tracked_code, "")
  }

  tracked_code <- c(tracked_code,
                    "## to guarantee the reproducibility of your code, you should also",
                    "## record the output of sessionInfo()",
                    "sessionInfo()")

  return(tracked_code)
}

.get_reporting_order <- function(active_plots, brush_parent) 
# Reordering plots by whether or not they exhibited brushing.
{
  N <- nrow(active_plots)
  edges <- isolates <- vector("list", N)
  node_names <- character(N)
  
  for (i in seq_len(N)) { 
    panel_type <- active_plots$Type[i]
    panel_id <- active_plots$ID[i]
    panel_name <- paste0(panel_type, "Plot", panel_id)
    node_names[i] <- panel_name

    parent <- brush_parent[[panel_name]]
    if (!is.null(parent)) { 
        edges[[i]] <- c(parent, panel_name)
    } 
  }

  edges <- as.character(unlist(edges))
  g <- make_graph(edges, isolates=setdiff(node_names, edges), directed=TRUE)
  ordering <- topo_sort(g, "out")
  match(names(ordering), node_names)
}

