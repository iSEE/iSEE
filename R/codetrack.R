.track_it_all <- function(rObjects, pObjects, se_name) {

  aobjs <- as.data.frame(rObjects$active_plots)

  # cmds are kept only for the plots and not for the tables
  aobjs <- aobjs[aobjs$Type!="geneStat",]

  # storing to a text character vector
  tracked_code <- c(
    "## Here's the list of commands to generate the plots you created using iSEE",
    "## Just copy them in a live session of R where your SingleCellExperiment object is",
    "## ... and if you want to adjust something, just edit the corresponding lines!",
    "",
    "## All commands below refers to your `SingleCellExperiment` object as `se`:",
    sprintf("se <- %s", se_name),
    "")
  for (i in seq_len(nrow(aobjs))) {
    tracked_code <- c(tracked_code,
                      paste0("## ",paste0(aobjs$Type[i],"_",aobjs$ID[i])),
                      pObjects$commands[[aobjs$Type[i]]][aobjs$ID[i]],
                      ""
                      )
    # message(paste0(aobjs$Type[i],"_",aobjs$ID[i]))
  }

  tracked_code <- c(tracked_code,
                    "## to guarantee the reproducibility of your code, you should also",
                    "## record the output of sessionInfo()",
                    "sessionInfo()")

  return(tracked_code)
}
