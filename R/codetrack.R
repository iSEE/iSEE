.track_it_all <- function(rObjects, pObjects, se_name) 
{
  # Commands only reported for plots, not for the tables
  aobjs <- as.data.frame(rObjects$active_plots)
  aobjs <- aobjs[aobjs$Type!="geneStat",]
  aobjs <- aobjs[.get_reporting_order(aobjs, pObjects$brush),]

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
    tracked_code <- c(tracked_code, "",
                      "# Saving for brush transmission",
                      sprintf("all.coordinates[['%s']] <- plot.data", panel_name))

    tracked_code <- c(tracked_code, "")
  }

  tracked_code <- c(tracked_code,
                    "## to guarantee the reproducibility of your code, you should also",
                    "## record the output of sessionInfo()",
                    "sessionInfo()")

  return(tracked_code)
}

.get_reporting_order <- function(active_plots, brush_chart) 
# Reordering plots by whether or not they exhibited brushing.
{
  N <- nrow(active_plots)
  node_names <- sprintf("%sPlot%i", active_plots$Type, active_plots$ID)
  ordering <- topo_sort(brush_chart, "out")
  order(match(node_names, names(ordering)))
}

