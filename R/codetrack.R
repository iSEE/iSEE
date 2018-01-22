.track_it_all <- function(rObjects, pObjects, se_name) {

  aobjs <- as.data.frame(rObjects$active_plots)

  # cmds are kept only for the plots and not for the tables
  aobjs <- aobjs[aobjs$Type!="geneStat",]

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
        tracked_code <- c(tracked_code, sprintf("all.coordinates[['%s']] <- plot.data", panel_name))
    }

    tracked_code <- c(tracked_code, "")
  }

  tracked_code <- c(tracked_code,
                    "## to guarantee the reproducibility of your code, you should also",
                    "## record the output of sessionInfo()",
                    "sessionInfo()")

  return(tracked_code)
}
