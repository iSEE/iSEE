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
                      "")

    # Adding the plotting commands.
    mult <- pObjects$brush[panel_name, panel_name]
    cur_cmds <- pObjects$commands[[panel_name]]
    save_cmd <- c("# Saving for brush transmission",
                  sprintf("all.coordinates[['%s']] <- plot.data", panel_name))

    if (mult==0) { 
      tracked_code <- c(tracked_code, cur_cmds, "", save_cmd)

    } else {
      # Some special handling required for self-brushing, to generate
      # the plot.data for calculation of BrushBy.
      # All non-plotting code goes into a loop.
      brushby_ix <- grep("^brushedPts <- ", cur_cmds)
      plot_ix <- which("# Generating the plot"==cur_cmds)
      plot_cmds <- cur_cmds[plot_ix:length(cur_cmds)]
      init_cmds <- cur_cmds[seq_len(brushby_ix-1L)]
      other_cmds <- cur_cmds[brushby_ix:(plot_ix-1L)]

      # Avoiding application of the brush in the first iteration.
      other_cmds[1] <- paste0("if (ix!=1) {\n", .indent(other_cmds[1]), 
                              "\n} else {\n    plot.data$BrushBy <- TRUE\n}")

      lim_ix <- grep("^[xy]bounds <- ", other_cmds)
      other_cmds[lim_ix] <- paste0("if (ix==1) {\n", .indent(other_cmds[lim_ix]), "\n}")

      # Saving the transmitted points.
      other_cmds <- c(other_cmds, save_cmd)

      tracked_code <- c(tracked_code, init_cmds, "for (ix in seq_len(2)) {",
                        .indent(other_cmds), "}", "", plot_cmds)
    }

    tracked_code <- c(tracked_code, "")
  }

  tracked_code <- c(tracked_code,
                    strrep("#", 80),
                    "## To guarantee the reproducibility of your code, you should also",
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

.indent <- function(cmds) {
  cmds <- paste0("    ", cmds)
  cmds <- gsub("\n", "\n    ", cmds)
  return(cmds)
}
